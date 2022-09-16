AGENTSTATE_MSG = "The id of the agent does not match the given type"

function _free_win(sim, T, name)
    if haskey(sim.typeinfos.nodes_attr[T], name)
        win = sim.typeinfos.nodes_attr[T][name]
        MPI.free(win)
        delete!(sim.typeinfos.nodes_attr[T], name)
    end
end


# The died/reuseable stuff is a little bit tricky: We need the died
# vector to easy check that an agent is still living in agentstate or
# the transition functions.  But when we want to reuse a row, we do
# not want to iterate over the died vector until we found a true
# value.  As people that are dying, are dead at step t+1 and not at
# step t, we can not set the died flag for step t. Instead we add
# this information to write.reuseable, which starts in an interation
# always empty and only contains the rows of agents that died
# at step t.
# In finish_write! we then merge read.reuseable and write.reusable to
# read.reuseable and set all the died flags to true for the
# write.reusable entries. This (hopefully) also expains, why we have
# died not seperated for read and write, as died is only used in read
# actions and updated in finish_write!

# attr is sim.typeinfos.nodes_attr[T]
function construct_agent_functions(T::DataType, typeinfos, simsymbol)
    attr = typeinfos.nodes_attr[T]
    stateless = fieldcount(T) == 0
    immortal = :Immortal in attr[:traits]
    checkliving = :CheckLiving in attr[:traits]
    fixedsize = haskey(attr, :size)

    # in this case we don't have an active died vector, and
    # therefore we can not check the died status anyway
    if immortal && !fixedsize
        checkliving = false
    end
    
    _size = if fixedsize 
        attr[:size]
    else
        0
    end

    # TODO?: better name would be init_agentcontainer! 
    @eval function init_field!(sim::$simsymbol, ::Type{$T})
        if $fixedsize
            resize!(@readstate($T), $_size)
            resize!(@writestate($T), $_size)
            resize!(@reuse($T), $_size)
            fill!(@reuse($T), 0)
            if ! $immortal
                resize!(@died($T), $_size)
                # Strictly speaking they did not die, but they were
                # also never born
                fill(@died($T), $_size, true)
            end
        end
        nothing
    end
    
    typeid = typeinfos.nodes_type2id[T]

    @eval function _get_next_id(sim::$simsymbol, ::Type{$T})
        if ! $immortal
            if length(@readreuseable($T)) > 0
                nr = pop!(@readreuseable($T))
                # TODO AGENT: remove the assertion when we know that
                # this is working
                @assert @died($T)[nr]
                # We cast to UInt64 to ensure that the +1 does not overflow
                reuse = @reuse($T)[nr] |> UInt64
                reuse = reuse + 1
                # TODO AGENT: write a test that check this
                if reuse < 2 ^ BITS_REUSE
                    @died($T)[nr] = false
                    @reuse($T)[nr] = reuse |> Reuse
                    return (reuse, nr)
                end
            end
        end
        # not immortal or no reusable row was found, use the next row
        nr = @nextid($T)
        # TODO AGENT: add an assterions that we have ids left
        @nextid($T) = nr + 1
        if ! $fixedsize
            if nr > length(@reuse($T))
                resize!(@reuse($T), nr)
                @reuse($T)[nr] = 0
                resize!(@writestate($T), nr)
                if ! $immortal
                    resize!(@died($T), nr)
                end
            end
        end
        if $immortal
            return (0, nr)
        else
            reuse = @reuse($T)[nr] |> UInt64
            reuse = reuse + 1
            # TODO AGENT: write a test that check this
            if reuse < 2 ^ BITS_REUSE
                @died($T)[nr] = false
                @reuse($T)[nr] = reuse |> Reuse
                return (reuse, nr)
            end
            _get_next_id(sim, $T)
        end
    end
    
    # As the agent is stateless, we store only true or false for living/died.
    # we track this already in :died, but MPI RMA does not work for
    # bitvectors
    @eval function add_agent!(sim::$simsymbol, agent::$T)
        (reuse, nr) = _get_next_id(sim, $T)
        if ! $fixedsize
            if nr > length(@writestate($T))
                resize!(@writestate($T), nr)
            end
        end
        if $stateless
            @inbounds @writestate($T)[nr] = true
        else
            @inbounds @writestate($T)[nr] = agent
        end
        if $immortal
            immortal_agent_id($typeid, nr)
        else
            agent_id($typeid, Reuse(reuse), nr)
        end
    end

    @eval function agentstate(sim::$simsymbol, id::AgentID, ::Type{$T})
        @mayassert begin
            $type_nr(id) == sim.typeinfos.nodes_type2id[$T]
        end AGENTSTATE_MSG

        r = process_nr(id)
        if r == mpi.rank
            # highest priority: does the agent is still living
            if $checkliving
                nr = agent_nr(id)
                if @died($T)[nr]
                    return nothing
                end
            end
            # if it's living, but has no state, we can return directly
            if $stateless
                return T()
            end
            # we haven't calculate nr in the !checkliving case
            # but need it now (maybe the compiler is clever enough
            # to remove a second agent_nr(id) call, then this if could
            # be remove
            if ! $checkliving
                nr = agent_nr(id)
            end
            @inbounds @readstate($T)[nr]
        else
            # same procedure as above, but with RMA
            if $checkliving
                win_died = sim.typeinfos.nodes_attr[$T][:window_died]
                nr = agent_nr(id)
                as = Vector{Bool}(undef, 1)
                MPI.Win_lock(win_died; rank = r, type = :shared, nocheck = true)
                MPI.Get!(as, r, nr - 1, win_died)
                MPI.Win_unlock(win_died; rank = r)
                if as[1]
                    return nothing
                end
            end
            if $stateless
                return T()
            end
            if ! $checkliving
                nr = agent_nr(id)
            end
            win_state = sim.typeinfos.nodes_attr[$T][:window_state]
            as = Vector{T}(undef, 1)
            MPI.Win_lock(win_state; rank = r, type = :shared, nocheck = true)
            MPI.Get!(as, r, nr - 1, win_state)
            MPI.Win_unlock(win_state; rank = r)
            @inbounds as[1]
        end
    end

    @eval function transition!(sim::$simsymbol, func, ::Type{$T})
        # an own counter (with the correct type) is faster then enumerate 
        idx = AgentNr(0)
        for state::$T in @readstate($T)
            idx += AgentNr(1)
            # jump over died agents
            if $checkliving
                if @died(T)[idx]
                    continue
                end
            end
            newstate = func(state, agent_id(sim, $typeid, idx), sim)
            if $immortal
                @mayassert begin
                    newstate !== nothing
                end "You can not return `nothing` for immortal agents" 
                @writestate($T)[idx] = newstate
            else
                if isnothing(newstate)
                    @died(T)[idx] = true
                    push!(@writereuseable($T), idx)
                else
                    @writestate($T)[idx] = newstate
                end
            end
        end 
    end

    @eval function transition_invariant_compute!(sim::$simsymbol, func, ::Type{$T})
        # an own counter (with the correct type) is faster then enumerate 
        idx = AgentNr(0)
        for state::$T in @readstate($T)
            idx += AgentNr(1)
            # jump over died agents
            if $checkliving
                if @died(T)[idx]
                    continue
                end
            end
            func(state, agent_id(sim, $typeid, idx), sim)
        end 
    end

    @eval function prepare_write!(sim::$simsymbol, add_existing::Bool, ::Type{$T})
        if $immortal # immortal => add_existing; so we can ignore reuseable
            @assert add_existing == true 
            @writestate($T) = deepcopy(@readstate($T))
        else
            # We always only track the newly died agents in reuseable,
            # see also the longer comment at the top of this file
            empty!(@writereuseable($T))
            if add_existing
                @writestate($T) = deepcopy(@readstate($T))
            else
                empty!(@writestate($T))
                # We restart counting form 1 (but of course reuse will be used)
                empty!(@readreuseable($T))
                @nextid($T) = 1
                resize!(@writestate($T), $_size)
            end
        end
    end
    
    @eval function finish_write!(sim::$simsymbol, ::Type{$T})
        attrs = sim.typeinfos.nodes_attr[$T]
        # finish the last epoch
        _free_win(sim, $T, :window_died)
        _free_win(sim, $T, :window_state)
        @readstate($T) = @writestate($T)
        # TODO AGENT: Add infokeys to create
        # TODO AGENT: the died field is always the same, we don't have
        # read and write anymore, so we don't need to create and free it
        # every transition
        win_died = MPI.Win_create(@died($T), MPI.COMM_WORLD)
        attrs[:window_died] = win_died

        if ! $stateless
            win_state = MPI.Win_create(@readstate($T), MPI.COMM_WORLD)
            attrs[:window_state] = win_state
        end
        @readreuseable($T) = [ @readreuseable($T); @writereuseable($T) ]
        foreach(nr -> @died($T)[nr] = true,  @writereuseable($T)) 
    end

    aggregate = (T, _, simsymbol) -> begin
        @eval function aggregate(sim::$simsymbol, f, op, ::Type{$T}; kwargs...)
            mapreduce(f, op, sim.$(readfield(T)); kwargs...)
        end
    end
end
