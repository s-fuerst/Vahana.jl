# _memcpy! moved in Julia 1.10 from Base._memcpy! to Libc.memcpy. Before
# I start to support two different versions, I just define my own _memcpy!

@inline memcpy!(dst, src, n) = ccall(:memcpy, Cvoid, (Ptr{UInt8}, Ptr{UInt8}, Csize_t), dst, src, n)

function construct_agent_methods(T::DataType, typeinfos, simsymbol)
    attr = typeinfos.nodes_attr[T]
    # in the case that the AgentType is stateless, we only check
    # in mpi calls the value of the died entry
    stateless = fieldcount(T) == 0
    immortal = :Immortal in attr[:hints]
    independent = :Independent in attr[:hints]
    mortal = ! immortal

    nompi = ! mpi.active
    shmonly = mpi.size == mpi.shmsize
    multinode = mpi.size > mpi.shmsize

    if multinode
        construct_mpi_agent_methods(T, attr, simsymbol, mortal)
    end
    
    @eval function init_field!(sim::$simsymbol, ::Type{$T})
        @agentread($T) = AgentReadWrite($T)
        # for independent types we only use the reusable field of the struct
        @agentwrite($T) = AgentReadWrite($T)

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
            nr
        else   # immortal or no reusable row was found, use the next row
            nr = @nextid($T)
            # TODO AGENT: add an assertions that we have ids left
            @nextid($T) = nr + 1
            # this time we do not need an extra handling for the independent
            # hint, as we can not resize the read array (which is a
            # MPI shared array). Instead we copy in the case that the
            # array increased the new elements in finish_write.
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
        `apply`.
            """
        @mayassert begin
            T = $T
            sim.initialized == false ||
                nodes_attrs(sim, $T)[:writeable]
        end """
          $T must be in the `write` argument of the transition function.
        """
        nr = _get_next_id(sim, $T)
        if $independent
            if nr <= length(@readstate($T))
                @inbounds @readstate($T)[nr] = agent
            else
                @inbounds @writestate($T)[nr] = agent
            end
        else
            @inbounds @writestate($T)[nr] = agent
        end
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
          $T must be in the `read` argument of the transition function.
        """

        idrank = process_nr(id)
        if $nompi 
            # highest priority: does the agent is still living
            if $mortal
                nr = agent_nr(id)
                @mayassert ! @readdied($T)[nr] """
                agentstate was requested for agent $id that has been removed
                """
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
                    @mayassert ! @agent($T).shmdied[sr + 1][nr] """
                    agentstate was requested for agent $id that has been removed
                    """
                end
                if $stateless
                    return $T()
                end
                if ! $mortal
                    nr = agent_nr(id)
                end
                @inbounds @agent($T).shmstate[sr + 1][nr]
            else
                # same procedure as above, but with access to foreign nodes
                if $mortal
                    @mayassert ! @agent($T).foreigndied[id] """
                    agentstate was requested for agent $id that has been removed
                    """
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

    @eval @inline function transition_with_write!(sim, idx, newstate, ::Type{$T})
        if $immortal
            @mayassert begin
                newstate !== nothing
            end "You can not return `nothing` for immortal agents"
            if $independent
                @inbounds @readstate($T)[idx] = newstate
            else
                @inbounds @writestate($T)[idx] = newstate
            end
        else
            if isnothing(newstate)
                push!(@writereuseable($T), idx)
                @inbounds @writedied($T)[idx] = true
            else
                if $independent
                    @inbounds @readstate($T)[idx] = newstate
                else
                    @inbounds @writestate($T)[idx] = newstate
                end
            end
        end
    end

    @eval @inline function transition_without_write!(sim, idx, newstate, ::Type{$T})
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

    @eval function transition_with_read_with_edge!(wfunc,
                                            sim::$simsymbol,
                                            tfunc,
                                            ::Type{$T},
                                            ET::DataType)
        @mayassert !has_hint(sim, ET, :SingleType) """
        Only edge types without the :SingeType hint can be added to
        to the `with_edge` keyword of an apply! call, but $ET 
        has the :SingleType hint.    
            """
        tnr = type_nr(sim, $T)
        for (id, _) in edgeread(sim, ET)
            if type_nr(id) == tnr
                idx = agent_nr(id)
                state = @readstate($T)[idx]
                @mayassert ! @readdied($T)[idx]
                newstate = tfunc(state, id, sim)
                wfunc(sim, idx, newstate, $T)
            end
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

    @eval function transition_without_read_with_edge!(wfunc,
                                               sim::$simsymbol,
                                               tfunc,
                                               ::Type{$T},
                                               ET::DataType)
        @mayassert !has_hint(sim, ET, :SingleType) """
        Only edge types without the :SingeType hint can be added to
        to the `with_edge` keyword of an apply! call, but $ET
        has the :SingleType hint.    
        """
        tnr = type_nr(sim, $T)
        for (id, _) in edgeread(sim, ET)
            if type_nr(id) == tnr
                idx = agent_nr(id)
                if $mortal
                    @mayassert ! @readdied($T)[idx]
                end
                newstate = tfunc(Val($T), id, sim)
                wfunc(sim, idx, newstate, $T)
            end
        end
    end


    @eval function prepare_write!(sim::$simsymbol, _, add_existing::Bool, ::Type{$T})
        if $immortal 
            # distributing the initial graph or reading from file will
            # also kill immortal agents, so we can assert this only
            # after the sim is initialized.
            # for agentypes in call, add_existing is always set to true
            if sim.initialized
                @assert begin
                    T = $T
                    add_existing == true
                end """
                  You can add $T to the `write` argument only when $T is also 
                  included in the `add_existing` argument, as $T has the hint :Immortal.
                  """
            end
        end
        if ! add_existing
            @agentread($T) = AgentReadWrite($T)
            @agentwrite($T) = AgentReadWrite($T)
            @nextid($T) = 1
            if $mortal
                # As we start from 1, we empty the reuseable `nr`
                # vector (all are reuseable now)
                empty!(@readreuseable($T))
            end
        end
        nodes_attrs(sim, $T)[:writeable] = true
    end

    @eval function finish_write!(sim::$simsymbol, ::Type{$T})
        with_logger(sim) do
            @debug("<Begin> finish_write!",
                   agenttype=$T, transition=sim.num_transitions)
        end

        must_copy_mem = length(@writestate($T)) > length(@readstate($T)) ||
            ! sim.initialized ||
            ! $independent

        # copy_mem needs collective calls, so if one rank want to
        # copy_mem, all must participate
        must_copy_mem = MPI.Allreduce(must_copy_mem, |, MPI.COMM_WORLD)
        
        if $mortal
            network_changed = fill(false, length(sim.typeinfos.edges_types))
            # in writereuseable we have collected all agents that died
            # in this iteration
            aids = map(nr -> agent_id($typeid, nr), @writereuseable($T))

            edges_types = sim.typeinfos.edges_types
            
            # first we remove all the edges, where the agent is in the
            # target position. As those edges are stored on the same
            # rank as the agent, we can do this locally.
            for id in aids
                for ET in edges_types
                    idx = findfirst(x -> x == ET, edges_types)
                    network_changed[idx] |= 
                        _remove_edges_agent_target!(sim, id, ET)
                end
            end

            # for the edges where we have the died agents on the source
            # position we have the agentsontarget dicts on each process.
            # So we first collect the ids of all died agents and then
            # call remove_edges! for all edges stored in agentsontarget for
            # the collected ids.
            alldied = $nompi ? aids : join(aids)
            if ! isempty(alldied)
                for ET in edges_types
                    idx = findfirst(x -> x == ET, edges_types)
                    network_changed[idx] |=
                        _remove_edges_agent_source!(sim, alldied, ET) 
                end
            end

            MPI.Allreduce!(network_changed, |, mpi.comm)
            for ET in edges_types
                idx = findfirst(x -> x == ET, edges_types)
                if network_changed[idx]
                    getproperty(sim, Symbol(ET)).last_change =
                        sim.num_transitions
                end
            end

            # maybe the state has change, so we must clear the cache 
            empty!(@agent($T).foreigndied)
        end

        if ! $stateless && must_copy_mem
            if $independent && sim.initialized
                memcpy!(@writestate($T), @readstate($T),
                        length(@readstate($T)) * sizeof($T))
            end
            
            if ! isnothing(@windows($T).shmstate)
                MPI.free(@windows($T).shmstate)
            end
            (@windows($T).shmstate, sarr) = 
                MPI.Win_allocate_shared(Array{$T},
                                        length(@writestate($T)),
                                        mpi.shmcomm)

            memcpy!(sarr, @writestate($T),
                    length(@writestate($T)) * sizeof($T))

            
            MPI.Win_fence(0, @windows($T).shmstate)
            @readstate($T) = sarr

            # maybe the state has change, so we must clear the cache 
            empty!(@agent($T).foreignstate)
        elseif $stateless
            # for stateless T this is a fast operation (25 ns), and
            # we need this to detect the number of agents in the
            # case that they are immortal (in this case we could use
            # the died array, but to avoid code complexity, we always
            # ensure that readstate is available
            @readstate($T) = fill($T(), length(@writestate($T)))
        end

        # for the independent case, we can not set died to true directly
        # in the transition function, as in the case that the reused id is
        # after the id of the agent that add the agent, we will
        # call the new agent already in the same iteration.
        # So we have no seperate handling for the indepent case for
        # the died vector.
        if $mortal 
            if ! isnothing(@windows($T).shmdied)
                MPI.free(@windows($T).shmdied)
            end
            (@windows($T).shmdied, sarr) = 
                MPI.Win_allocate_shared(Array{Bool},
                                        length(@writedied($T)),
                                        mpi.shmcomm)
            
            memcpy!(sarr, @writedied($T),
                    length(@writedied($T)) * sizeof(Bool))

            
            MPI.Win_fence(0, @windows($T).shmdied)
            @readdied($T) = sarr
        end
        
        if ! $stateless && must_copy_mem
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

        nodes_attrs(sim, $T)[:writeable] = false
        
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

            memcpy!(sarr, @readstate($T),
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

            memcpy!(sarr, @readdied($T),
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
            # edgetypes without the :IgnoreFrom hint
            readableET = filter(readable) do r
                r in sim.typeinfos.edges_types &&
                    ! has_hint(sim, r, :IgnoreFrom)   
            end
            transmit_agents!(sim, readableET, $T)
        end
        
        # we use this as a check that the types are in the read
        # vector, and this check should also done in an nompi run
        @windows($T).prepared = true
    end 

    # some mpi parts are in prepare_write, as we always use the shared_memory
    # mpi calls, even in single process runs.
    @eval function finish_read!(sim::$simsymbol, ::Type{$T})
        # we use this as a check that the types are in the read
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

    @eval function mapreduce(sim::$simsymbol, f, op, ::Type{$T}; kwargs...)
        with_logger(sim) do
            @info "<Begin> mapreduce agents" f op agenttype=$T
        end
        @mayassert ! sim.intransition """
        You can not call mapreduce inside of a transition function."""
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

        _log_info(sim, "<End> mapreduce agents")
        
        r
    end
end
