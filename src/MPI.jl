using MPI

using Logging


#                             NodeType -- PE  -- AgentID
# structured_send_map is Dict{DataType, Vector{Vector{AgentID}}
# NodeType1 -- PE1 -- AgentID1
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


# T is agenttype
function sendagents!(sim, perPE::Vector{Vector{AgentID}}, T::DataType) 
    longvec = reduce(vcat, perPE)
    sendbuf = if length(longvec) > 0
        agentstates = map(longvec) do id
            agentstate(sim, id, T)
        end
        VBuffer(agentstates, [ length(perPE[i]) for i in 1:mpi.size ])
    else
        VBuffer(Vector{T}(), [ 0 for _ in 1:mpi.size ])
    end

    sendNumElems = [ length(perPE[i]) for i in 1:mpi.size ]
    recvNumElems = MPI.Alltoall(UBuffer(sendNumElems, 1), mpi.comm)
    recvbuf = MPI.VBuffer(Vector{T}(undef, sum(recvNumElems)), recvNumElems)

    MPI.Alltoallv!(sendbuf, recvbuf, mpi.comm)
    #    @info "received buffer" mpi.rank recvbuf
    add_agents!(sim, recvbuf.data)
end

# For sending the edges we need different versions, depending on the
# edge traits, as e.g. for the :HasEdgeOnly or :NumEdgeOnly we
# transfer only the number of edges. In overall, when we iterate over
# the container, we get the following values:
# |                    | Stateless | Ignore | get edges via | sending            |
# |--------------------+-----------+--------+---------------+--------------------|
# | (Vector){Edge{$T}} |           |        | edges_to      | [(toid, Edge{$T})] |
# | (Vector){AgentID}  | x         |        | neighborids   | [(toid, fromid)]   |
# | (Vector){$T}       |           | x      | edgestates    | [(toid, $T)]       |
# | Int64              | x         | x      | num_neighbors | MPI_reduce         |
function sendedges!(sim, sendmap::Dict{AgentID, ProcessID}, T::DataType)
    ST = Vector{Tuple{AgentID, Edge{T}}} 
    perPE = [ ST() for _ in 1:mpi.size ]
    
    for (to, e) in edges_iterator(sim, T)
        push!(perPE[sendmap[to]], (to, e))
    end

    longvec = reduce(vcat, perPE)
    sendbuf = if length(longvec) > 0
        VBuffer(longvec, [ length(perPE[i]) for i in 1:mpi.size ])
    else
        VBuffer(Vector{Tuple{AgentID, Edge{T}}}(), [ 0 for _ in 1:mpi.size ])
    end

    sendNumElems = [ length(perPE[i]) for i in 1:mpi.size ]
    recvNumElems = MPI.Alltoall(UBuffer(sendNumElems, 1), mpi.comm)
    recvbuf = MPI.VBuffer(Vector{Tuple{AgentID, Edge{T}}}(undef, sum(recvNumElems)), recvNumElems)

    MPI.Alltoallv!(sendbuf, recvbuf, mpi.comm)

    for (to, e) in recvbuf.data
        add_edge!(sim, to, e)
    end    
end

function distribute!(sim, sendmap::Dict{AgentID, ProcessID})
    node_types = sim.typeinfos.nodes_types
    edge_types = sim.typeinfos.edges_types
    # We reconstruct the whole graph
    foreach(prepare_write!(sim, []), [ node_types; edge_types ])

    # Send all agentstates
    ssm = create_structured_send_map(sim, sendmap)
    for T in node_types
        sendagents!(sim, ssm[T], T)
    end

    for T in edge_types
        sendedges!(sim, sendmap, T)
    end

    
    foreach(finish_write!(sim), [ node_types; edge_types ])
end
