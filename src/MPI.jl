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
    foreach(prepare_write!(sim, []), [ node_types; edge_types ])

    #    foreach(T -> prepare_mpi!(sim, T), node_types)

    MPI.Barrier(MPI.COMM_WORLD)
    disable_transition_checks(true)
    
    # we also reset the nextid count to 1 for every nodetype
    foreach(node_types) do T
        nextid(sim, T) = AgentNr(1)
    end
    
    # We add structure to the simple AgentID -> ProcessID mapping,
    # so that we can combine all agents of the AgentTypes etc.
    ssm = create_structured_send_map(sim, sendmap)


    # First we send all agentstates and collect the idmapping, as the
    # agent gets an new id in this process. idmapping is a Dict that
    # allows to get the new id when only the old id is known.
    # The keys in the idmapping are the original id without the
    # "reuse" information, as this can not be reconstructed on the
    # new PEs. When the map is used in sendeges!, "reuse" will be also
    # ignored for the old id. 
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

    # we must also update and broadcast the ids for each raster. As we
    # don't need the edges for that, we must not synchronize the PEs
    # before.
    foreach(grid -> broadcastids(sim, grid, idmapping),  keys(sim.rasters))

    # finish everything and return the idmapping
    disable_transition_checks(false)
    MPI.Barrier(MPI.COMM_WORLD)
    foreach(T -> finish_distribute!(sim, T), node_types)
    foreach(finish_write!(sim), [ node_types; edge_types ])
    # foreach(T -> finish_mpi!(sim, T), node_types)
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
    ssm = Dict{DataType, Vector{Vector{AgentID}}}()
    for T in sim.typeinfos.nodes_types
        ssm[T] = [ Vector{AgentID}() for _ in 1:mpi.size ]
    end
    for (id, p) in sendmap
        push!(ssm[Vahana.type_of(sim, id)][p], id)           
    end
    ssm
end

# Distribute agents of agenttype T to the different PEs. 
function sendagents!(sim, perPE::Vector{Vector{AgentID}}, T::DataType)
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
    # we need oldid (without the reuse bits) as key and newid as value
    idmapping = Dict{AgentID, AgentID}() 
    foreach(recvbuf.data) do (id, agent)
        idmapping[remove_reuse(id)] = add_agent!(sim, agent)
    end
    idmapping
end

# For sending the edges we need different versions, depending on the
# edge traits, as e.g. for the :HasEdgeOnly or :NumEdgeOnly we
# transfer only the number of edges. In overall, when we iterate over
# the container, we get the following values:
# |                    | Statel. | Ignore | get edges via | sending (ST below) |
# |--------------------+---------+--------+---------------+--------------------|
# | (Vector){Edge{$T}} |         |        | edges_to      | [(toid, Edge{$T})] |
# | (Vector){AgentID}  | x       |        | neighborids   | [(toid, fromid)]   |
# | (Vector){$T}       |         | x      | edgestates    | [(toid, $T)]       |
# | Int64              | x       | x      | num_neighbors | num_neighbors      |
function sendedges!(sim, sendmap::Dict{AgentID, ProcessID}, idmapping, T::DataType)
    ST = Vector{Tuple{AgentID, sim.typeinfos.edges_attr[T][:containerelement]}}
    # We construct for each PE a vector with all the edges (incl. toid) that
    # should be transmit to the PE
    perPE = [ ST() for _ in 1:mpi.size ]

    # The iterator for the edges depends on the traits of the edgetype
    if has_trait(sim, T, :Stateless) && has_trait(sim, T, :IgnoreFrom)
        iter = edgeread(sim, T)
        if has_trait(sim, T, :SingleAgentType)
            iter = enumerate(iter)
        end
    else
        iter = edges_iterator(sim, T)
    end

    # we now iterate over the edges and add them to the perPE vector
    # that corresponds to the process id given in the sendmap for the
    # target of the edge
    for (to, e) in iter
        id = if has_trait(sim, T, :SingleAgentType)
            AT = sim.typeinfos.edges_attr[T][:to_agenttype]
            typeID = sim.typeinfos.nodes_type2id[AT]
            # we don't know the reuse value of the agent in the
            # :SingleAgentType case, so we call immortal_agent_id,
            # which uses the reuse value of 0.  This is okay, as we
            # have removed the reuse bits from the AgentID already
            # before in sendagents!
            immortal_agent_id(typeID, AgentNr(to))
        else
            # To be consistent with sendagents!, we must remove
            # the reuse bits here too
            remove_reuse(to)
        end
        # in the default finish_init! case with an initialization
        # that does not care about mpi.isroot, we have edges on
        # all ranks, but on the sendmap there are only the edges
        # of rank 0, as this are the only edges we want to distribute
        if haskey(sendmap, id)
            # in the SingleAgentType version, we also get entries with 0 edges
            # we skip them, there is no need to use bandwith for that
            if has_trait(sim, T, :Stateless) &&
                has_trait(sim, T, :IgnoreFrom) &&
                e == 0
                continue
            end
            push!(perPE[sendmap[id]], (id, e))
        end
    end

    # get the updated id of an agent with the old id `id`, whereby the
    # reuse bits of `id` are set to zero (as this is also the case for
    # the keys of idmapping)
    updateid(id::AgentID) = idmapping[id] |> AgentID
    
    edges_alltoall!(sim, perPE, T, updateid)
end

function construct_mpi_agent_methods(T::DataType, attr, simsymbol, checkliving)
    @eval function transmit_agents!(sim::$simsymbol,
                             readableET::Vector{DataType},
                             ::Type{$T})
        for ET in readableET
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

                ## first we transfer all the AgentIDs for which we need the state

                # for sending them via AllToAll we flatten the perPE structure
                asvec = map(s -> collect(s), filter(! isempty, perPE))
                
                sendbuf = if ! isempty(asvec)
                    longvec = reduce(vcat, asvec)
                    VBuffer(reduce(vcat, asvec),
                            [ length(perPE[i]) for i in 1:mpi.size ])
                else
                    VBuffer(Vector{AgentID}(), [ 0 for _ in 1:mpi.size ])
                end

                # transmit the number of edges the current PE want to
                # send to the other PEs
                sendNumElems = [ length(perPE[i]) for i in 1:mpi.size ]
                recvNumElems = MPI.Alltoall(UBuffer(sendNumElems, 1), mpi.comm)
                # with this information we can prepare the receive buffer
                recvbuf = MPI.VBuffer(Vector{AgentID}(undef, sum(recvNumElems)),
                                      recvNumElems)

                # transmit the edges itself
                MPI.Alltoallv!(sendbuf, recvbuf, mpi.comm)

                ## now we gather the agentstate
                send = if $checkliving
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
                recv = if $checkliving 
                    Vector{Tuple{AgentID, Bool, $T}}(undef, sum(sendNumElems))
                else
                    Vector{Tuple{AgentID, $T}}(undef, sum(sendNumElems))
                end
                recvbuf = VBuffer(recv, sendNumElems)
                MPI.Alltoallv!(sendbuf, recvbuf, mpi.comm)

                ## And now fill the foreign dictonaries
                if $checkliving 
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
    end
end 

function construct_mpi_edge_methods(T::DataType, attr, simsymbol, CE)
    ignorefrom = :IgnoreFrom in attr[:traits]
    singleedge = :SingleEdge in attr[:traits]
    singletype = :SingleAgentType in attr[:traits]
    stateless = :Stateless in attr[:traits]

    ST = Vector{Tuple{AgentID, CE}}

    # updateid is a func that update the ids of the agents, for the case
    # that the ids have changed in the distribution process.
    @eval function edges_alltoall!(sim::$simsymbol, perPE, ::Type{$T}, updateid = identity)
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
                @assert numedges == 1
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
                        @inbounds @edgewrite($T)[up] += 1
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
                add_edge!(sim, updateid(to), Edge(updateid(edge.from), edge.state))
            end
        end

        @edge($T).last_transmit = sim.num_transitions
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

    send = MPI.VBuffer(sizes, fill(Int32(1), mpi.size), fill(Int32(mpi.rank), mpi.size))
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
