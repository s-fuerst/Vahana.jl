function construct_agent_methods(T::DataType, typeinfos, simsymbol)
    attr = typeinfos.nodes_attr[T]
    # in the case that the AgentType is stateless, we only check
    # in mpi calls the value of the died entry
    stateless = fieldcount(T) == 0
    immortal = :Immortal in attr[:traits]
    mortal = ! immortal

    nompi = ! mpi.active
    shmonly = mpi.size == mpi.shmsize
    multinode = mpi.size > mpi.shmsize

    if multinode
        construct_mpi_agent_methods(T, attr, simsymbol, mortal)
    end
    
    @eval function init_field!(sim::$simsymbol, ::Type{$T})
        @agentread($T) = AgentReadWrite($T)
        @agentwrite($T) = AgentReadWrite($T)
        @nextid($T) = AgentNr(1)

        for ET in sim.typeinfos.edges_types
            @agent($T).last_transmit[ET] = -1
        end
        
        nothing
    end
    
    typeid = typeinfos.nodes_type2id[T]

    @eval function _get_next_id(sim::$simsymbol, ::Type{$T})
        if $mortal && length(@readreuseable($T)) > 0
            nr = pop!(@readreuseable($T))
            @writedied($T)[nr] = false
        else   # immortal or no reusable row was found, use the next row
            nr = @nextid($T)
            # TODO AGENT: add an assterions that we have ids left
            @nextid($T) = nr + 1
            if nr > length(@writestate($T))
                resize!(@writestate($T), nr)
                # the length of writestate and writedied is always
                # the same, so we don't need a seperate if
                if $mortal
                    resize!(@writedied($T), nr)
                end
            end
            if $mortal
                @writedied($T)[nr] = false
            end
            nr
        end       
    end    
    @eval function add_agent!(sim::$simsymbol, agent::$T)
        @mayassert sim.initialized == false || sim.intransition """
        You can call add_agent! only in the initialization phase (until
        `finish_init!` is called) or within a transition function called by
        `apply_transition`.
        """
        nr = _get_next_id(sim, $T)
        @inbounds @writestate($T)[nr] = agent
        agent_id($typeid, nr)
    end

    @eval function agentstate(sim::$simsymbol, id::AgentID, ::Type{$T})
        @mayassert begin
            $type_nr(id) == sim.typeinfos.nodes_type2id[$T]
        end "The id of the agent does not match the given type"

        @mayassert begin
            T = $T
            @windows($T).prepared || ! config.check_readable
        end """
          $T must be in the `accessible` argument of the transition function.
        """

        idrank = process_nr(id)
        if $nompi 
            # highest priority: does the agent is still living
            if $mortal
                nr = agent_nr(id)
                if @readdied($T)[nr] 
                    return nothing
                end
            end
            # if it's living, but has no state, we can return directly
            if $stateless
                return $T()
            end
            # we haven't calculate nr in the $mortal case
            # but need it now (maybe the compiler is clever enough
            # to remove a second agent_nr(id) call, then this if could
            # be remove
            if ! $mortal
                nr = agent_nr(id)
            end
            @inbounds @readstate($T)[nr]
        else
            (node, sr) = fldmod(idrank, mpi.shmsize)
            if $shmonly || node == mpi.node
                # same procedure as above, but with access to shared memory
                if $mortal
                    nr = agent_nr(id)
                    if @agent($T).shmdied[sr + 1][nr]
                        return nothing
                    end
                end
                if $stateless
                    return $T()
                end
                if ! $mortal
                    nr = agent_nr(id)
                end
                @inbounds @agent($T).shmstate[sr + 1][nr]
            else
                # same procedure as above, but with access to shared memory
                if $mortal
                    if @agent($T).foreigndied[id]
                        return nothing
                    end
                end
                if $stateless
                    return $T()
                end
                @agent($T).foreignstate[id]
            end
        end
    end

    @eval agent_id(_::$simsymbol, agent_nr, ::Type{$T}) =
        agent_id($typeid, agent_nr)
    
    @inline @eval function transition_with_write!(sim, idx, newstate, ::Type{$T})
        if $immortal
            @mayassert begin
                newstate !== nothing
            end "You can not return `nothing` for immortal agents" 
            @inbounds @writestate($T)[idx] = newstate
        else
            if isnothing(newstate)
                push!(@writereuseable($T), idx)
                @inbounds @writedied($T)[idx] = true
            else
                @inbounds @writestate($T)[idx] = newstate
            end
        end
    end

    @inline @eval function transition_without_write!(sim, idx, newstate, ::Type{$T})
        @mayassert begin
            T = $T
            typeof(newstate) != $T
        end """
        The transition function returned an agent of type $T,  
        but $T is not in the `write` vector.
        """
    end
    
    @eval function transition_with_read!(wfunc, sim::$simsymbol, tfunc, ::Type{$T})
        # an own counter (with the correct type) is faster then enumerate 
        idx = AgentNr(0)
        for state::$T in @readstate($T)
            idx += AgentNr(1)
            # jump over died agents
            if $mortal
                @inbounds if @readdied($T)[idx]
                    continue
                end
            end
            newstate = tfunc(state, agent_id($typeid, idx), sim)
            wfunc(sim, idx, newstate, $T)
        end 
    end

    @eval function transition_without_read!(wfunc, sim::$simsymbol, tfunc, ::Type{$T})
        # an own counter (with the correct type) is faster then enumerate 
        idx = AgentNr(0)
        for _ in 1:length(@readstate($T))
            idx += AgentNr(1)
            if $mortal
                @inbounds if @readdied($T)[idx]
                    continue
                end
            end
            r = tfunc(Val($T), agent_id($typeid, idx), sim)
            wfunc(sim, idx, r, $T)
        end 
    end

    @eval function prepare_write!(sim::$simsymbol, add_existing::Bool, ::Type{$T})
        if $immortal 
            # distributing the initial graph will also kill immortal agents,
            # so we can assert this only after the sim is initialized
            if sim.initialized
                @assert begin
                    T = $T
                    add_existing == true
                end "You cannot rebuild $T because it has the trait :Immortal."
            end
        end
        if ! add_existing
            @agentwrite($T) = AgentReadWrite($T)
            @nextid($T) = 1
            if $mortal
                # As we start from 1, we empty the reuseable `nr`
                # vector (all are reuseable now)
                empty!(@readreuseable($T))
            end
        end
    end

    @eval function finish_write!(sim::$simsymbol, ::Type{$T})
        with_logger(sim) do
            @debug("<Begin> finish_write!",
                   agenttype=$T, transition=sim.num_transitions)
        end
        if $mortal
            # in writereuseable we have collected all agents that died
            # in this iteration
            aids = map(nr -> agent_id($typeid, nr), @writereuseable($T))

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

            # maybe the state has change, so we must clear the cache 
            empty!(@agent($T).foreigndied)
        end

        if ! $stateless
            if ! isnothing(@windows($T).shmstate)
                MPI.free(@windows($T).shmstate)
            end
            (@windows($T).shmstate, sarr) = 
                MPI.Win_allocate_shared(Array{$T},
                                        length(@writestate($T)),
                                        mpi.shmcomm)

            Base._memcpy!(sarr, @writestate($T),
                          length(@writestate($T)) * sizeof($T))
            
            MPI.Win_fence(0, @windows($T).shmstate)
            @readstate($T) = sarr

            # maybe the state has change, so we must clear the cache 
            empty!(@agent($T).foreignstate)
        else
            # for stateless T this is a fast operation (25 ns), and
            # we need this to detect the number of agents in the
            # case that they are immortal (in this case we could use
            # the died array, but to avoid code complexity, we always
            # ensure that readstate is available
            @readstate($T) = fill($T(), length(@writestate($T)))
        end
        if $mortal
            if ! isnothing(@windows($T).shmdied)
                MPI.free(@windows($T).shmdied)
            end
            (@windows($T).shmdied, sarr) = 
                MPI.Win_allocate_shared(Array{Bool},
                                        length(@writedied($T)),
                                        mpi.shmcomm)

            Base._memcpy!(sarr, @writedied($T),
                          length(@writedied($T)) * sizeof(Bool))

            MPI.Win_fence(0, @windows($T).shmdied)
            @readdied($T) = sarr
        end
        
        if ! $stateless
            @agent($T).shmstate = 
                [ MPI.Win_shared_query(Array{$T},
                                       @windows($T).shmstate;
                                       rank = i - 1) for i in 1:mpi.shmsize ]
        end
        if $mortal
            @agent($T).shmdied = 
                [ MPI.Win_shared_query(Array{Bool},
                                       @windows($T).shmdied;
                                       rank = i - 1) for i in 1:mpi.shmsize ]
        end
        # for the reusable vector, we merge the new reuseable indices
        # (from agent died in this transition) with the unused indicies that
        # are still in readreuseable.
        @readreuseable($T) = [ @readreuseable($T); @writereuseable($T) ]
        empty!(@writereuseable($T))

        @agent($T).last_change = sim.num_transitions

        _log_debug(sim, "<End> finish_write!")
        nothing
    end
    
    # used in copy_simulation!
    @eval function copy_shm!(sim, ::Type{$T})
        if ! $stateless
            (@windows($T).shmstate, sarr) = 
                MPI.Win_allocate_shared(Array{$T},
                                        length(@readstate($T)),
                                        mpi.shmcomm)

            Base._memcpy!(sarr, @readstate($T),
                          length(@readstate($T)) * sizeof($T))
            
            MPI.Win_fence(0, @windows($T).shmstate)
            @readstate($T) = sarr

            @agent($T).shmstate = 
                [ MPI.Win_shared_query(Array{$T},
                                       @windows($T).shmstate;
                                       rank = i - 1) for i in 1:mpi.shmsize ]
        end
        if $mortal
            (@windows($T).shmdied, sarr) = 
                MPI.Win_allocate_shared(Array{Bool},
                                        length(@readdied($T)),
                                        mpi.shmcomm)

            Base._memcpy!(sarr, @readdied($T),
                          length(@readdied($T)) * sizeof(Bool))

            MPI.Win_fence(0, @windows($T).shmdied)
            @readdied($T) = sarr

            @agent($T).shmdied = 
                [ MPI.Win_shared_query(Array{Bool},
                                       @windows($T).shmdied;
                                       rank = i - 1) for i in 1:mpi.shmsize ]
        end
    end
    
    # this is called after the agents where distributed to the different PEs.
    # In the process add_agent! is called.
    @eval function finish_distribute!(sim::$simsymbol, ::Type{$T})
    end

    @eval function prepare_read!(sim::$simsymbol,
                          readable::Vector{DataType},
                          ::Type{$T})
        if $multinode
            # we check in the function itself, if it's really necessary to
            # transmit agentstate, but we need therefore all readable
            # edgetypes without the :IgnoreFrom trait
            readableET = filter(readable) do r
                r in sim.typeinfos.edges_types &&
                    ! has_trait(sim, r, :IgnoreFrom)   
            end
            transmit_agents!(sim, readableET, $T)
        end
        
        # we use this as a check that the types are in the accessible
        # vector, and this check should also done in an nompi run
        @windows($T).prepared = true
    end 

    # some mpi parts are in prepare_write, as we always use the shared_memory
    # mpi calls, even in single process runs.
    @eval function finish_read!(sim::$simsymbol, ::Type{$T})
        # we use this as a check that the types are in the accessible
        # vector, and this check should also done in an nompi run
        @windows($T).prepared = false
    end

    @eval function agentsonthisrank(sim::$simsymbol, ::Type{$T}, write = false)
        max_agents = @agent($T).nextId - 1
        if $immortal
            @agent($T).nextId - 1
            if write
                @writestate($T)
            else
                @readstate($T)
            end
        else
            if write
                [ @writestate($T)[i] for i in 1:min(length(@writestate($T)),
                                                 max_agents)
                     if ! @writedied($T)[i] ]
            else
                [ @readstate($T)[i] for i in 1:min(length(@readstate($T)),
                                                max_agents)
                     if ! @readdied($T)[i] ]
            end
        end
    end        

    @eval function aggregate(sim::$simsymbol, f, op, ::Type{$T}; kwargs...)
        with_logger(sim) do
            @info "<Begin> aggregate agents" f op agenttype=$T
        end
        @mayassert ! sim.intransition """
        You can not call aggregate inside of a transition function."""
        emptyval = val4empty(op; kwargs...)

        if $immortal
            reduced = emptyval
            for i in 1:(@agent($T).nextid - 1)
                reduced = op(f(@readstate($T)[i]), reduced)
            end
        else 
            reduced = emptyval
            for i in 1:(@agent($T).nextid - 1)
                if ! @readdied($T)[i]
                    reduced = op(f(@readstate($T)[i]), reduced)
                end
            end
        end   

        r = if $nompi
            reduced
        else
            mpiop = get(kwargs, :mpiop, op)
            MPI.Allreduce(reduced, mpiop, MPI.COMM_WORLD)
        end

        _log_info(sim, "<End> aggregate agents")
        
        r
    end
end
