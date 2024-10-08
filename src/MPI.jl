using MPI

using Logging

# Distributing the whole graph to the different processes.
# 
# This function is called from finish_init!, per default the sendmap
# is created in finish_init! by Metis, but also a external
# mapping can be used, e.g. for testing, or in the case that the
# modeller can create a better partitioning him/herself then Metis.
function distribute!(sim, sendmap::Dict{AgentID, ProcessID})
    node_types = sim.typeinfos.nodes_types
    edge_types = sim.typeinfos.edges_types
    # We reconstruct the whole graph, so we call prepare_write for
    # all agent and edgetypes
    foreach(prepare_write!(sim, [], []), [ node_types; edge_types ])

    MPI.Barrier(MPI.COMM_WORLD)
    disable_transition_checks(true)
    
    # we also reset the nextid count to 1 for every nodetype
    foreach(node_types) do T
        nextid(sim, T) = AgentNr(1)
    end
    
    # We add structure to the simple AgentID -> ProcessID mapping,
    # so that we can combine all agents of a AgentTypes etc.
    ssm = create_structured_send_map(sim, sendmap)

    # First we send all agentstates and collect the idmapping, as the
    # agent gets an new id in this process. idmapping is a Dict that
    # allows to get the new id when only the old id is known.
    idmapping = Dict{AgentID, AgentID}()
    for T in node_types
        merge!(idmapping, sendagents!(sim, ssm[T], T))
    end

    # Currently, a PE knows only the idmapping of the agent that was
    # mapped to this PE. We collect now this information. As we can not send
    # the Dict directly, we join the keys and values indepently and create
    # a new dict afterwards
    allkeys = join(keys(idmapping) |> collect)
    allvalues = join(values(idmapping) |> collect)
    idmapping = Dict(allkeys .=> allvalues)

    # Till now we have distribute only the agents, and we have the mapping
    # of the agents-ids. We now transfer the edges, and are updating also
    # the id of the agents by this way
    for T in edge_types
        sendedges!(sim, sendmap, idmapping, T)
    end

    # we want to allow that add_raster is only called on the root rank
    # but in this case the sim.rasters Dict missings the keys on the
    # other ranks. So we first send the key names
    if mpi.rank == 0
        rasternames = String.(keys(sim.rasters))
        n = length(rasternames)
        MPI.Bcast!(Ref(n), MPI.COMM_WORLD)
        for s in rasternames
            len = length(s)
            MPI.Bcast!(Ref(len), MPI.COMM_WORLD)
            MPI.Bcast!(Vector{UInt8}(s), MPI.COMM_WORLD)
        end
    else
        n = Ref{Int}()
        MPI.Bcast!(n, MPI.COMM_WORLD)
    
        for _ in 1:n[]
            len = Ref{Int}()
            MPI.Bcast!(len, MPI.COMM_WORLD)
            buffer = Vector{UInt8}(undef, len[])
            MPI.Bcast!(buffer, MPI.COMM_WORLD)
            push!(sim.rasters, Symbol(String(buffer)) => AgentID[])
        end
    end

    # we must also update and broadcast the ids for each raster. As we
    # don't need the edges for that, we must not synchronize the PEs
    # before.
    foreach(grid -> broadcastids(sim, grid, idmapping),  keys(sim.rasters))

    # finish everything and return the idmapping
    disable_transition_checks(false)
    MPI.Barrier(MPI.COMM_WORLD)
    foreach(T -> finish_distribute!(sim, T), node_types)
    
    idmapping
end


# structured_send_map is Dict{DataType, Vector{Vector{AgentID}}, e.g.
# Buyer --- PE1 --- AgentID1
#        |       |- AgentID4
#        |       '- AgentID7
#        '- PE2 --- AgentID3
#                '- AgentID4
# Seller ...
#
# This map is used by sendedges! 
function create_structured_send_map(sim, sendmap::Dict{AgentID, ProcessID})
    _log_debug(sim, "<Begin> create_structured_send_map")
    ssm = Dict{DataType, Vector{Vector{AgentID}}}()
    for T in sim.typeinfos.nodes_types
        ssm[T] = [ Vector{AgentID}() for _ in 1:mpi.size ]
    end
    for (id, p) in sendmap
        push!(ssm[Vahana.type_of(sim, id)][p], id)           
    end
    _log_debug(sim, "<End> create_structured_send_map")
    ssm
end

# Distribute agents of agenttype T to the different PEs. 
function sendagents!(sim, perPE::Vector{Vector{AgentID}}, T::DataType)
    with_logger(sim) do
        @debug "<Begin> sendagents!" agenttype=T
    end
    # ST is the transmitted datatype. Beside the state itself (T) we
    # need also the AgentID to create the mappning from the old
    # AgentID to the new AgentID
    ST = Vector{Tuple{AgentID,T}} 
    
    longvec = reduce(vcat, perPE)
    sendbuf = if length(longvec) > 0
        agentstates = map(longvec) do id
            (id, agentstate(sim, id, T))
        end
        VBuffer(agentstates, [ length(perPE[i]) for i in 1:mpi.size ])
    else
        VBuffer(ST(), [ 0 for _ in 1:mpi.size ])
    end

    # first we transmit the number of agents a PE want to send to the
    # other PEs
    sendNumElems = [ length(perPE[i]) for i in 1:mpi.size ]
    recvNumElems = MPI.Alltoall(UBuffer(sendNumElems, 1), mpi.comm)
    # with this information we can prepare the receive buffer
    recvbuf = MPI.VBuffer(ST(undef, sum(recvNumElems)), recvNumElems)
    # and then transfer the {AgentID,T (AgentState)} tuples.
    MPI.Alltoallv!(sendbuf, recvbuf, mpi.comm)
    # we need oldid as key and newid as value
    idmapping = Dict{AgentID, AgentID}() 
    foreach(recvbuf.data) do (id, agent)
        idmapping[id] = add_agent!(sim, agent)
    end
    _log_debug(sim, "<End> sendagents!")
    idmapping
end


function construct_mpi_agent_methods(T::DataType, attr, simsymbol, mortal)
    stateless = :Stateless in attr[:hints]

    @eval function transmit_agents!(sim::$simsymbol,
                             readableET::Vector{DataType},
                             ::Type{$T})
        if $stateless
            return
        end

        with_logger(sim) do
            @debug "<Begin> transmit_agents!" agenttype=$T
        end
        
        for ET in readableET
            if :IgnoreSourceState in sim.typeinfos.edges_attr[ET][:hints]
                continue
            end
            
            edgefield = getproperty(sim, Symbol(ET))
            # there are two reasons why we must transmit the agentstate:
            # - the agentstates has changed since the last transmit:
            #   last_change >= last_transmit[ET]
            # - a readable network R has changed since the last transmit
            #   of agents caused by this network:
            #   @edge($ET).last_change >= last_transmit[ET]
            if @agent($T).last_transmit[ET] <= @agent($T).last_change ||
                @agent($T).last_transmit[ET] <= edgefield.last_change

                perPE = edgefield.accessible[sim.typeinfos.nodes_type2id[$T]]

                # only access the state if it isn't already in the dict
                # (the foreign dict is cleared when $T is writeable).
                if ! isempty(@agent($T).foreignstate)
                    for i in 1:mpi.size
                        filter!(d -> ! haskey(@agent($T).foreignstate, d),
                                perPE[i])
                    end
                end
                
                ## first we transfer all the AgentIDs for which we need the state

                # for sending them via AllToAll we flatten the perPE structure
                asvec = map(s -> collect(s), filter(! isempty, perPE))

                sendbuf = if ! isempty(asvec)
                    VBuffer(reduce(vcat, asvec),
                            [ length(perPE[i]) for i in 1:mpi.size ])
                else
                    VBuffer(Vector{AgentID}(), [ 0 for _ in 1:mpi.size ])
                end

                # transmit the number of AgentIDs the current PE want to
                # send to the other PEs
                sendNumElems = [ length(perPE[i]) for i in 1:mpi.size ]
                recvNumElems = MPI.Alltoall(UBuffer(sendNumElems, 1), mpi.comm)
                # with this information we can prepare the receive buffer
                recvbuf = MPI.VBuffer(Vector{AgentID}(undef, sum(recvNumElems)),
                                      recvNumElems)

                # transmit the AgentIDs itself
                _log_time(sim, "transmit_agents Alltoallv! AgentIDs", true) do
                    MPI.Alltoallv!(sendbuf, recvbuf, mpi.comm)
                end

                ## now we gather the agentstate
                send = if $mortal
                    if ! isempty(recvbuf.data) 
                        map(recvbuf.data) do id
                            (id,
                             @readdied($T)[agent_nr(id)],
                             @readstate($T)[agent_nr(id)])
                        end
                    else
                        Vector{Tuple{AgentID, Bool, $T}}()
                    end
                else
                    if ! isempty(recvbuf.data)
                        map(recvbuf.data) do id
                            (id, @readstate($T)[agent_nr(id)])
                        end
                    else
                        Vector{Tuple{AgentID, $T}}()
                    end
                end

                ## and send this back
                # recvNumElems will be now sendNumElems and vis a vis
                sendbuf = VBuffer(send, recvNumElems)
                recv = if $mortal
                    Vector{Tuple{AgentID, Bool, $T}}(undef, sum(sendNumElems))
                else
                    Vector{Tuple{AgentID, $T}}(undef, sum(sendNumElems))
                end
                recvbuf = VBuffer(recv, sendNumElems)
                _log_time(sim, "transmit_agents Alltoallv! state", true) do
                    MPI.Alltoallv!(sendbuf, recvbuf, mpi.comm)
                end

                ## And now fill the foreign dictonaries
                if $mortal 
                    for (id, died, state) in recvbuf.data
                        @agent($T).foreigndied[id] = died
                        @agent($T).foreignstate[id] = state
                    end
                else
                    for (id, state) in recvbuf.data
                        @agent($T).foreignstate[id] = state
                    end
                end
                @agent($T).last_transmit[ET] = sim.num_transitions
            end
        end

        _log_debug(sim, "<End> transmit_agents!")
    end
end 

function construct_mpi_edge_methods(T::DataType, typeinfos, simsymbol, CE)
    attr = typeinfos.edges_attr[T]

    ignorefrom = :IgnoreFrom in attr[:hints]
    singleedge = :SingleEdge in attr[:hints]
    singletype = :SingleType in attr[:hints]
    stateless = :Stateless in attr[:hints]
    
    ST = Vector{Tuple{AgentID, CE}}

    # typeid = typeinfos.edges_type2id[T]
    
    # For sending the edges we need different versions, depending on the
    # edge hints, as e.g. for the :HasEdgeOnly or :NumEdgeOnly we
    # transfer only the number of edges. In overall, when we iterate over
    # the container, we get the following values:
    # |                    | Statel. | Ignore | get edges via | sending (ST below) |
    # |--------------------+---------+--------+---------------+--------------------|
    # | (Vector){Edge{$T}} |         |        | edges         | [(toid, Edge{$T})] |
    # | (Vector){AgentID}  | x       |        | neighborids       | [(toid, fromid)]   |
    # | (Vector){$T}       |         | x      | edgestates    | [(toid, $T)]       |
    # | Int64              | x       | x      | num_edges     | num_edges      |
    @eval function sendedges!(sim::$simsymbol, sendmap::Dict{AgentID, ProcessID},
                       idmapping, ::Type{$T})
        with_logger(sim) do
            @debug "<Begin> sendedges!" edgetype=$T
        end

        if $singletype
            AT = sim.typeinfos.edges_attr[$T][:target]
            agent_typeid = sim.typeinfos.nodes_type2id[AT]
        end

        ST = Vector{Tuple{AgentID, sim.typeinfos.edges_attr[$T][:containerelement]}}
        # We construct for each PE a vector with all the edges (incl. toid) that
        # should be transmit to the PE
        perPE = [ ST() for _ in 1:mpi.size ]

        # The iterator for the edges depends on the hints of the edgetype
        if $stateless && $ignorefrom
            iter = @edgeread($T)
            if $singletype 
                iter = enumerate(iter)
            end
        else
            iter = edges_iterator(sim, $T)
        end

        # we now iterate over the edges and add them to the perPE vector
        # that corresponds to the process id given in the sendmap for the
        # target of the edge
        for (to, e) in iter
            id = if $singletype
                agent_id(agent_typeid, AgentNr(to))
            else
                to
            end

            # in the default finish_init! case with an initialization
            # that does not care about mpi.isroot, we have edges on
            # all ranks, but on the sendmap there are only the edges
            # of rank 0, as this are the only edges we want to distribute
            if haskey(sendmap, id)
                # in the SingleType version, we also get entries with 0 edges
                # we skip them, there is no need to use bandwith for that
                if $stateless && $ignorefrom && e == 0
                    continue
                end
                push!(perPE[sendmap[id]], (id, e))
            end
        end

        updateid(id::AgentID) = idmapping[id] |> AgentID

        edges_alltoall!(sim, perPE, $T, updateid)

        with_logger(sim) do
            @debug "<End> sendedges!" 
        end
    end

    # updateid is a func that update the ids of the agents, for the case
    # that the ids have changed in the distribution process.
    @eval function edges_alltoall!(sim::$simsymbol, perPE, ::Type{$T},
                            updateid = identity)
        with_logger(sim) do
            @debug "<Begin> edges_alltoall!" edgetype=$T
        end
        # we call add_edge! but add_edge! has an check that we
        # are in the initialization phase or in a transition function.
        # So we set the intranstion flag, is necessary
        wasintransition = sim.intransition
        sim.intransition = true
        waswriteable = edge_attrs(sim, $T)[:writeable]
        edge_attrs(sim, $T)[:writeable] = true

        # for sending them via AllToAll we flatten the perPE structure
        longvec = reduce(vcat, perPE)
        sendbuf = if length(longvec) > 0
            VBuffer(longvec, [ length(perPE[i]) for i in 1:mpi.size ])
        else
            VBuffer($ST(), [ 0 for _ in 1:mpi.size ])
        end

        # transmit the number of edges the current PE want to send to the
        # other PEs
        sendNumElems = [ length(perPE[i]) for i in 1:mpi.size ]
        recvNumElems = MPI.Alltoall(UBuffer(sendNumElems, 1), mpi.comm)
        # with this information we can prepare the receive buffer
        recvbuf = MPI.VBuffer($ST(undef, sum(recvNumElems)),
                              recvNumElems)

        # transmit the edges itself
        MPI.Alltoallv!(sendbuf, recvbuf, mpi.comm)

        # In the case that the edgetype count only the number of edges, write
        # this number directly into the edges container of the receiving PE
        if $stateless && $ignorefrom
            for (to, numedges) in recvbuf.data
                up = if $singletype
                    updateid(to) |> agent_nr
                else
                    updateid(to)
                end
                _check_size!(@edgewrite($T), up, $T)
                if $singleedge
                    @inbounds @edgewrite($T)[up] = true
                else
                    if $singletype || haskey(@edgewrite($T), up)
                        @inbounds @edgewrite($T)[up] += numedges
                    else
                        @inbounds @edgewrite($T)[up] = numedges
                    end
                end
            end
            # Else iterate of the received edges and add them via add_edge!    
        elseif $stateless
            for (to, from) in recvbuf.data
                add_edge!(sim, updateid(from), updateid(to), $T())
            end
        elseif $ignorefrom
            for (to, edgestate) in recvbuf.data
                # fromid will be ignored, so we use an dummy id
                add_edge!(sim, AgentID(0), updateid(to), edgestate)
            end
        else
            for (to, edge) in recvbuf.data
                add_edge!(sim, updateid(to), Edge(updateid(edge.from),
                                                  edge.state))
            end
        end

        @edge($T).last_transmit = sim.num_transitions

        sim.intransition = wasintransition 
        edge_attrs(sim, $T)[:writeable] = waswriteable
       
        _log_debug(sim, "<End> edges_alltoall!")

        nothing
    end

    @eval function removeedges_alltoall!(sim::$simsymbol, perPE, ::Type{$T})
        with_logger(sim) do
            @debug "<Begin> removeedges_alltoall!" edgetype=$T
        end
        # we call remove_edges! but remove_edges! has an check that we
        # are in the initialization phase or in a transition function.
        # So we set the intranstion flag, is necessary
        wasintransition = sim.intransition
        sim.intransition = true
        waswriteable = edge_attrs(sim, $T)[:writeable]
        edge_attrs(sim, $T)[:writeable] = true

        # for sending them via AllToAll we flatten the perPE structure
        longvec = reduce(vcat, perPE)
        sendbuf = if length(longvec) > 0
            VBuffer(longvec, [ length(perPE[i]) for i in 1:mpi.size ])
        else
            VBuffer($ST(), [ 0 for _ in 1:mpi.size ])
        end

        # transmit the number of edges the current PE want to send to the
        # other PEs
        sendNumElems = [ length(perPE[i]) for i in 1:mpi.size ]
        recvNumElems = MPI.Alltoall(UBuffer(sendNumElems, 1), mpi.comm)
        # with this information we can prepare the receive buffer
        recvbuf = MPI.VBuffer(fill((0,0), sum(recvNumElems)),
                              recvNumElems)

        # transmit the information about the removed edge itself
        MPI.Alltoallv!(sendbuf, recvbuf, mpi.comm)

        for (source, target) in recvbuf.data
            target = AgentID(target)
            @mayassert process_nr(target) == mpi.rank
            if source == 0
                remove_edges!(sim, target, $T)
            else
                remove_edges!(sim, AgentID(source), target, $T)
            end
        end

        sim.intransition = wasintransition 
        edge_attrs(sim, $T)[:writeable] = waswriteable
       
        _log_debug(sim, "<End> removeedges_alltoall!")

        nothing
    end
end



# Join a vector that is distributed over serveral processes. vec can be [].
#
# # Example
# rank 0: vec = [1, 2]
# rank 1: vec = []
# rank 2: vec = [4]
#
# join(vec) returns [1, 2, 4] on all ranks
function join(vec::Vector{T}) where T
    # transfer the vector sizes
    sizes = Vector{Int32}(undef, mpi.size)
    sizes[mpi.rank + 1] = length(vec)

    send = MPI.VBuffer(sizes, fill(Int32(1), mpi.size),
                       fill(Int32(mpi.rank), mpi.size))
    recv = MPI.VBuffer(sizes, fill(Int32(1), mpi.size))

    MPI.Alltoallv!(send, recv, mpi.comm)
    sizes = recv.data

    # transfer the vector itself
    displace = append!([0], cumsum(sizes)[1 : length(sizes) - 1])
    values = Vector{T}(undef, sum(sizes))
    values[displace[mpi.rank+1] + 1 : displace[mpi.rank+1] + sizes[mpi.rank+1]] = vec

    send = MPI.VBuffer(values,
                       fill(Int32(sizes[mpi.rank + 1]), mpi.size),
                       fill(Int32(displace[mpi.rank + 1]), mpi.size))
    recv = MPI.VBuffer(values, sizes)

    MPI.Alltoallv!(send, recv, mpi.comm)

    recv.data
end
