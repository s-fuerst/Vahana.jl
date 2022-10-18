function construct_agent_functions(T::DataType, typeinfos, simsymbol)
    attr = typeinfos.nodes_attr[T]
    # in the case that the AgentType is stateless, we only check
    # in mpi calls the value of the died entry
    stateless = fieldcount(T) == 0
    immortal = :Immortal in attr[:traits]
    checkliving = ! (:Unsecure in attr[:traits])
    fixedsize = haskey(attr, :size)
    
    # in this case we don't have an active died vector, and
    # therefore we can not check the died status anyway
    if immortal && ! fixedsize
        checkliving = false
    end

    _size = if fixedsize 
        attr[:size]
    else
        0
    end

    nompi = ! mpi.active
    
    @eval function init_field!(sim::$simsymbol, ::Type{$T})
        @agentread($T) = AgentReadWrite($T)
        @agentwrite($T) = AgentReadWrite($T)
        @nextid($T) = AgentNr(1)
        @reuse($T) = Vector{Reuse}()
        if $fixedsize
            resize!(@readstate($T), $_size)
            resize!(@writestate($T), $_size)
            resize!(@reuse($T), $_size)
            fill!(@reuse($T), 0)
            if $checkliving
                resize!(@readdied($T), $_size)
                resize!(@writedied($T), $_size)
                # Strictly speaking they did not die, but they were
                # also never born
                fill!(@readdied($T), true)
                fill!(@writedied($T), true)
            end
        end
        nothing
    end
    
    typeid = typeinfos.nodes_type2id[T]

    @eval function _get_next_id(sim::$simsymbol, ::Type{$T})
        if ! $immortal && length(@readreuseable($T)) > 0
            nr = pop!(@readreuseable($T))
            # We cast to UInt64 to ensure that the +1 does not overflow
            reuse = @reuse($T)[nr] |> UInt64
            reuse = reuse + 1
            # TODO AGENT: write a test that check this
            if reuse < 2 ^ BITS_REUSE
                @writedied($T)[nr] = false
                # cast it back to the correct type
                reuse = Reuse(reuse)
                @reuse($T)[nr] = reuse
                (reuse, nr)
            else
                # this index can not be used anymore, but also
                # removed from the reuse vector. So we can recursively
                # call _get_next_id again.
                _get_next_id(sim, $T)
            end
        else   # immortal or no reusable row was found, use the next row
            nr = @nextid($T)
            # TODO AGENT: add an assterions that we have ids left
            @nextid($T) = nr + 1
            if ! $fixedsize
                if nr > length(@reuse($T))
                    resize!(@reuse($T), nr)
                    @reuse($T)[nr] = 0
                end
                if nr > length(@writestate($T))
                    resize!(@writestate($T), nr)
                    # the length of writestate and writedied is always
                    # the same, so we don't need a seperate if
                    if $checkliving
                        resize!(@writedied($T), nr)
                    end
                end
            end
            if $checkliving
                @writedied($T)[nr] = false
            end
            (0, nr)
        end       
    end    
    @eval function add_agent!(sim::$simsymbol, agent::$T)
        @mayassert sim.initialized == false || sim.transition """
        You can call add_agent! only in the initialization phase (until
        `finish_init!` is called) or within a transition function called by
        `apply_transition`.
        """
        (reuse, nr) = _get_next_id(sim, $T)
        @inbounds @writestate($T)[nr] = agent
        if $immortal
            immortal_agent_id($typeid, nr)
        else
            agent_id($typeid, Reuse(reuse), nr)
        end
    end

    asref = Ref{T}()
    asdt = MPI.Datatype(T)
    
    @eval function agentstate(sim::$simsymbol, id::AgentID, ::Type{$T})
        @mayassert begin
            $type_nr(id) == sim.typeinfos.nodes_type2id[$T]
        end "The id of the agent does not match the given type"

        @mayassert begin
            T = $T
            @windows($T).prepared
        end """
          $T must be in the `accessible` argument of the transition function.
        """

        r = process_nr(id)
        if $nompi || r == mpi.rank
            # highest priority: does the agent is still living
            if $checkliving
                nr = agent_nr(id)
                # TODO AGENT: increase reuse in finish_write,
                # then we can remove the died vector access here
                if @readdied($T)[nr] || @reuse($T)[nr] > reuse_nr(id)
                    return nothing
                end
            end
            # if it's living, but has no state, we can return directly
            if $stateless
                return $T()
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
                win_died = @windows($T).nodedied
                nr = agent_nr(id)
                died = Vector{Bool}(undef, 1)
                MPI.Win_lock(win_died; rank = r, type = :shared, nocheck = true)
                MPI.Get!(died, r, nr - 1, win_died)
                MPI.Win_unlock(win_died; rank = r)
                if died[1]
                    return nothing
                end
            end
            if $stateless
                return T()
            end
            if ! $checkliving
                nr = agent_nr(id)
            end
            win_state = @windows($T).nodestate
            MPI.Win_lock(win_state; rank = r, type = :shared, nocheck = true)
            # MPI.Get!($asref, r, nr - 1, win_state)
            ccall((:MPI_Get, MPI.libmpi), Cint,
                  (MPI.MPIPtr, Cint, MPI.MPI_Datatype, Cint,
                   Cptrdiff_t, Cint, MPI.MPI_Datatype, MPI.MPI_Win),
                  $asref, MPI.Cint(1), $asdt, r,
                  Cptrdiff_t(nr - 1), MPI.Cint(1), $asdt, win_state)
            MPI.Win_unlock(win_state; rank = r)
            $asref[]
        end
    end

    @eval agent_id(sim::$simsymbol, agent_nr, ::Type{$T}) =
        agent_id($typeid, @reuse($T)[agent_nr], agent_nr)
    
    @eval function transition!(sim::$simsymbol, func, ::Type{$T})
        # an own counter (with the correct type) is faster then enumerate 
        idx = AgentNr(0)
        for state::$T in @readstate($T)
            idx += AgentNr(1)
            # jump over died agents
            if $checkliving
                if @readdied($T)[idx]
                    continue
                end
            end
            newstate = func(state, agent_id(sim, idx, $T), sim)
            if $immortal
                @mayassert begin
                    newstate !== nothing
                end "You can not return `nothing` for immortal agents" 
                @writestate($T)[idx] = newstate
            else
                if isnothing(newstate)
                    push!(@writereuseable($T), idx)
                    if $checkliving 
                        @writedied($T)[idx] = true
                    end
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
            if $checkliving
                if @readdied($T)[idx]
                    continue
                end
            end
            func(state, agent_id(sim, idx, $T), sim)
        end 
    end

    @eval function prepare_write!(sim::$simsymbol, add_existing::Bool, ::Type{$T})
        if $immortal 
            # while distributing the initial graph we also kill immortal agents
            if sim.initialized == true
                @assert begin
                    T = $T
                    add_existing == true
                end "You cannot rebuild $T because it has the trait :Immortal."
            end
        end
        if ! add_existing
            @agentwrite($T) = AgentReadWrite($T)
            if $fixedsize
                resize!(@writestate($T), $_size)
                if $checkliving 
                    resize!(@writedied($T), $_size)
                end
            end
            # We restart counting from 1 (but of course reuse will be used)
            @nextid($T) = 1
            if ! $immortal
                # As we start from 1, we empty the reuseable `nr`
                # vector (all are reuseable now)
                empty!(@readreuseable($T))
            end
        end
    end

    @eval function finish_write!(sim::$simsymbol, ::Type{$T})
        if ! $immortal
            aids = map(nr -> agent_id(sim, nr, $T), @writereuseable($T))

            # first we remove all the edges, where the agent is in the
            # target position. As those edges are stored on the same
            # rank as the agent, we can do this locally.
            for id in aids
                for ET in sim.typeinfos.edges_types
                    _remove_edges_agent_traget!(sim, id, ET)
                end
            end

            # for the edges where the agent is in the source position,
            # the situation is worse, as they don't have the
            # information on the agents rank, to which other agents
            # edges are targeted. So we create a join of all the ids of
            # all ranks, and then go locally to all the edges of the rank.
            alldied = $nompi ? aids : join(aids)
            if ! isempty(alldied)
                for ET in sim.typeinfos.edges_types
                    _remove_edges_agent_source!(sim, alldied, ET)
                end
            end
        end

        @readstate($T) = deepcopy(@writestate($T))
        if $checkliving
            @readdied($T) = deepcopy(@writedied($T))
        end
        # for the reusable vector, we merge the new reuseable indices
        # (from agent died in this transition) with the unused indicies that
        # are still in readreuseable.
        @readreuseable($T) = [ @readreuseable($T); @writereuseable($T) ]
        empty!(@writereuseable($T))
    end
    
    
    # this is called after the agents where distributed to the different PEs.
    # In the process add_agent! is called.
    @eval function finish_distribute!(sim::$simsymbol, ::Type{$T})
        nagents = @nextid($T) - 1
        if $fixedsize && $checkliving
            for i in (nagents+1):$_size
                @readdied($T)[i] = true
                @writedied($T)[i] = true
            end
        end
    end

    @eval function prepare_mpi!(sim::$simsymbol, ::Type{$T})
        # we use this as a check that the types are in the accessible
        # vector, and this check should also done in an nompi run
        @windows($T).prepared = true

        if ! $nompi
            @assert isnothing(@windows($T).nodedied)

            win_died = MPI.Win_create(@readdied($T),
                                      MPI.COMM_WORLD;
                                      same_disp_unit = true)
            @windows($T).nodedied = win_died

            if ! $stateless
                @assert isnothing(@windows($T).nodestate)

                win_state = MPI.Win_create(@readstate($T),
                                           MPI.COMM_WORLD,
                                           same_disp_unit = true)
                @windows($T).nodestate = win_state
            end
        end
    end 


    @eval function finish_mpi!(sim::$simsymbol, ::Type{$T})
        # we use this as a check that the types are in the accessible
        # vector, and this check should also done in an nompi run
        @windows($T).prepared = false

        if ! $nompi
            MPI.free(@windows($T).nodedied)
            @windows($T).nodedied = nothing

            if ! $stateless
                MPI.free(@windows($T).nodestate)
                @windows($T).nodestate = nothing
            end
        end
    end

    @eval function agentsonthisrank(sim::$simsymbol, ::Type{$T}, write = false)
        if ! $checkliving
            states = @readstate($T)
        else
            if write
                states = [ @writestate($T)[i] for i in 1:length(@writestate($T))
                              if ! @writedied($T)[i] ]
            else
                states = [ @readstate($T)[i] for i in 1:length(@readstate($T))
                              if ! @readdied($T)[i] ]
            end
        end
    end        

    @eval function aggregate(sim::$simsymbol, f, op, ::Type{$T}; kwargs...)
        emptyval = val4empty(op; kwargs...)
        
        reduced = mapreduce(f, op, agentsonthisrank(sim, $T); init = emptyval)
        if $nompi
            reduced
        else
            mpiop = get(kwargs, :mpiop, op)
            MPI.Allreduce(reduced, mpiop, MPI.COMM_WORLD)
        end
    end
end
