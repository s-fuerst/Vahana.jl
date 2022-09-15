TODO: Rewrite attrs[:died] to read/write 


AGENTSTATE_MSG = "The id of the agent does not match the given type"

# attr is sim.typeinfos.nodes_attr[T]
function construct_agent_functions(T::DataType, typeinfos, simsymbol)
    attr = typeinfos.nodes_attr[T]
    stateless = :Stateless in attr[:traits]
    immortal = :Immortal in attr[:traits]
    checkliving = :CheckLiving in attr[:traits]
    fixedsize = haskey(attr, :size)
    
    _size = if fixedsize 
        attr[T][:size]
    else
        0
    end
    
    # TODO: add ::$simsymbol to all functions
    @eval function init_field!(sim, ::Type{$T})
        sim.$(readfield(T)) = Vector{$T}()
        sim.$(writefield(T)) = Vector{$T}()
        sim.$(reusefield(T)) = Vector{Reuse}()
        sim.$(nextidfield(T)) = 1
        # create the additional structures we use to
        # track the state of the rows 
        if ! $immortal
            attrs = sim.typeinfos.nodes_attr[$T]
            attrs[:reuseable] = Vector{AgentNr}()
            # we cannot use a bitvector, as MPI RMA does not work with bitvectors
            attrs[:died] = Vector{Bool}()
        end
        if $fixedsize
            resize!(sim.$(writefield(T)), $_size)
            resize!(sim.$(reusefield(T)), $_size)
            if ! $immortal
                # Strictly speaking they did not die, but they were
                # also never born
                fill(attrs[:died], $_size, true)
            end
        end
        nothing
    end
    
    typeid = typeinfos.nodes_type2id[T]

    # TODO: check if we get a noticeable performance improvment, when we
    # move the attrs[:died] etc. to direct sim fields
    @eval function _get_next_id(sim, ::Type{$T})
        reusefield = sim.$(reusefield(T))
        if ! $immortal
            # TODO: check if we can use attr instead (but does this work with deepcopy?)
            attrs = sim.typeinfos.nodes_attr[T]
            if length(attrs[:reuseable]) > 0
                nr = pop!(attrs[:reuseable])
                # TODO: remove the assertion when we know that this is working
                @assert attrs[:died][nr]
                reuse::UInt64 = reusefield[nr] + 1
                # TODO: write a test that check this
                if reuse < 2 ^ BITS_REUSE
                    attrs[:died][nr] = false
                    reusefield[nr] = reuse
                    return (reuse, nr)
                end
            end
        end
        # not immortal or no reusable row was found, use the next row
        nr = sim.$(nextidfield(T))
        sim.$(nextidfield(T)) = nr + 1
        if ! $fixedsize
            if nr > length(reusefield)
                resize!(reusefield, nr)
                resize!(sim.$(writefield(T)), nr)
                if ! $immortal
                    resize!(attrs[:died], nr)
                end
            end
        end
        # TODO: add an assterions that we have ids left
        reusefield[nr] = 0
        (0, nr)
    end
    
    # As the agent is stateless, we store only true or false for living/died.
    # we track this already in :died, but MPI RMA does not work for
    # bitvectors
    @eval function add_agent!(sim, agent::$T)
        (reuse, nr) = _get_next_id(sim, $T)
        @info reuse nr
        if ! fixedsize
            if nr > length(sim.$(writefield(T)))
                resize!(sim.$(writefield(T)), nr)
            end
        end
        if stateless
            @inbounds sim.$(writefield(T))[nr] = true
        else
            @inbounds sim.$(writefield(T))[nr] = agent
        end
        if immortal
            immortal_agent_id($typeid, nr)
        else
            agent_id($typeid, reuse, nr)
        end
    end

    @eval function agentstate(sim::$simsymbol, id::AgentID, ::Type{$T})
        @mayassert $type_nr(id) == sim.typeinfos.nodes_type2id[$T] AGENTSTATE_MSG

        r = process_nr(id)
        if r == mpi.rank
            if ! $immortal && $checkliving
                attrs = sim.typeinfos.nodes_attr[$T]
                nr = agent_nr(id)
                if attrs[:died][nr]
                    return nothing
                end
            end
            if $stateless
                return T()
            end
            if $immortal
                nr = agent_nr(id)
            end
            @inbounds sim.$(readfield(T))[nr]
        else
            if ! $immortal && $checkliving
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
            if $immortal 
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
        read::Vector{$T} = sim.nodes_id2read[$typeid](sim)
        write::Vector{$T} = sim.nodes_id2write[$typeid](sim)
        attrs = sim.typeinfos.nodes_attr[$T]
        # an own counter (with the correct type) is faster then enumerate 
        idx = AgentNr(0)
        for state::$T in read
            idx += AgentNr(1)
            # jump over died agents
            if $checkliving
                if attrs[:died][idx]
                    continue
                end
            end
            newstate = func(state, agent_id(sim, $typeid, idx), sim)
            if $immortal
                @mayassert newstate !== nothing "You can not return `nothing` for immortal agents" 
                write[idx] = newstate
            else
                if isnothing(newstate)
                    attrs[:died][idx] = true
                    push!(attrs[:reuseable], idx)
                else
                    write[idx] = newstate
                end
            end
        end 
    end

    @eval function transition_invariant_compute!(sim::$simsymbol, func, ::Type{$T})
        read::Vector{$T} = sim.nodes_id2read[$typeid](sim)
        attrs = sim.typeinfos.nodes_attr[$T]
        # an own counter (with the correct type) is faster then enumerate 
        idx = AgentNr(0)
        for state::$T in read
            idx += AgentNr(1)
            # jump over died agents
            if $checkliving
                if attrs[:died][idx]
                    continue
                end
            end
            func(state, agent_id(sim, $typeid, idx), sim)
        end 
    end

    @eval function prepare_write!(sim::$simsymbol, add_existing::Bool, ::Type{$T})
        if $immortal
            @assert add_existing == true
            sim.$(writefield(T)) = deepcopy(sim.$(readfield(T)))
        else
            if add_existing
                sim.$(writefield(T)) = deepcopy(sim.$(readfield(T)))
            else
                sim.$(writefield(T)) = Vector{$T}()
                sim.$(reusefield(T)) = Vector{Reuse}()
                sim.$(nextidfield(T)) = 1
                # create the additional structures we use to
                # track the state of the rows 
                if ! $immortal
                    attrs = sim.typeinfos.nodes_attr[$T]
                    attrs[:reuseable] = Vector{AgentNr}()
                    # we cannot use a bitvector, as MPI RMA does not work with bitvectors
                    attrs[:died] = Vector{Bool}()
                end
                if $fixedsize
                    resize!(sim.$(writefield(T)), $_size)
                    resize!(sim.$(reusefield(T)), $_size)
                    if ! $immortal
                        # Strictly speaking they did not die, but they were
                        # also never born
                        fill(attrs[:died], $_size, true)
                    end
                end
            end
        end
    end
    
    function _free_win(sim, T, name)
        if haskey(sim.typeinfos.nodes_attr[T], name)
            win = sim.typeinfos.nodes_attr[T][name]
            MPI.free(win)
            delete!(sim.typeinfos.nodes_attr[T], name)
        end
    end
    
    @eval function finish_write!(sim::$simsymbol, ::Type{$T})
        attrs = sim.typeinfos.nodes_attr[$T]
        # finish the last epoch
        _free_win(sim, $T, :window_died)
        _free_win(sim, $T, :window_state)
        sim.$(readfield(T)) = sim.$(writefield(T))
        # TODO: Add infokeys
        win_died = MPI.Win_create(attrs[:died], MPI.COMM_WORLD)
        sim.typeinfos.nodes_attr[$T][:window_died] = win_died

        win_state = MPI.Win_create(sim.$(readfield(T)), MPI.COMM_WORLD)
        sim.typeinfos.nodes_attr[$T][:window_state] = win_state
    end


    aggregate = (T, _, simsymbol) -> begin
        @eval function aggregate(sim::$simsymbol, f, op, ::Type{$T}; kwargs...)
            mapreduce(f, op, sim.$(readfield(T)); kwargs...)
        end
    end
end
