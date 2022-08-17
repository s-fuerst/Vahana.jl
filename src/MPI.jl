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
# |                    | Statel. | Ignore | get edges via | sending            |
# |--------------------+---------+--------+---------------+--------------------|
# | (Vector){Edge{$T}} |         |        | edges_to      | [(toid, Edge{$T})] |
# | (Vector){AgentID}  | x       |        | neighborids   | [(toid, fromid)]   |
# | (Vector){$T}       |         | x      | edgestates    | [(toid, $T)]       |
# | Int64              | x       | x      | num_neighbors | MPI_reduce         |
function sendedges!(sim, sendmap::Dict{AgentID, ProcessID}, T::DataType)
    CE = sim.typeinfos.edges_attr[T][:containerelement]
    # if (T == Main.CurrentEdgeType)
    #     @info "sendedges begin" mpi.rank CE T 
    #     if mpi.rank == 0
    #         println(sendmap)
    #         println((edges_iterator(sim, T) |> collect))
    #     end
    # end
    ST = Vector{Tuple{AgentID, CE}} 
    perPE = [ ST() for _ in 1:mpi.size ]

    if has_trait(sim, T, :Stateless) && has_trait(sim, T, :IgnoreFrom)
        iter = getproperty(sim, readfield(Symbol(T)))
        if has_trait(sim, T, :SingleAgentType)
            iter = enumerate(iter)
        end
    else
        iter = edges_iterator(sim, T)
    end
    
    for (to, e) in iter
        if has_trait(sim, T, :SingleAgentType)
            AT = sim.typeinfos.edges_attr[T][:to_agenttype]
            to = agent_id(sim, to, AT)
        end
        # in the :SingleEdge, :SingleAgentType case we check via
        # the sendmap if there is really this agent on the PE
        if haskey(sendmap, to)
            # in the SingleAgentType version, we also get entries with 0 edges
            # we skip them, there is no need to use bandwith for that
            if has_trait(sim, T, :Stateless) &&
                has_trait(sim, T, :IgnoreFrom) &&
                e == 0
                continue
            end
            push!(perPE[sendmap[to]], (to, e))
        end
    end

    longvec = reduce(vcat, perPE)
    sendbuf = if length(longvec) > 0
        VBuffer(longvec, [ length(perPE[i]) for i in 1:mpi.size ])
    else
        VBuffer(Vector{Tuple{AgentID, CE}}(), [ 0 for _ in 1:mpi.size ])
    end

    sendNumElems = [ length(perPE[i]) for i in 1:mpi.size ]
    recvNumElems = MPI.Alltoall(UBuffer(sendNumElems, 1), mpi.comm)
    recvbuf = MPI.VBuffer(Vector{Tuple{AgentID, CE}}(undef, sum(recvNumElems)),
                          recvNumElems)

    MPI.Alltoallv!(sendbuf, recvbuf, mpi.comm)

    if has_trait(sim, T, :Stateless) && has_trait(sim, T, :IgnoreFrom)
        if (T == Main.CurrentEdgeType) @info "SI" end
            
        container = getproperty(sim, writefield(Symbol(T)))
         
        for (to, numedges) in recvbuf.data
            @assert numedges > 0
            @info numedges
            add_edge!(sim, AgentID(0), to, T())
#            container[to] = numedges
        end
    elseif has_trait(sim, T, :Stateless)
        if (T == Main.CurrentEdgeType) @info "S" end

        for (to, from) in recvbuf.data
            add_edge!(sim, from, to, T())
        end
    elseif has_trait(sim, T, :IgnoreFrom)
        if (T == Main.CurrentEdgeType) @info "I" end

        for (to, edgestate) in recvbuf.data
            # the fromid will be ignored, so we use an dummy id
            add_edge!(sim, AgentID(0), to, edgestate)
        end
    else
        if (T == Main.CurrentEdgeType) @info "D" end
        for (to, edge) in recvbuf.data
            add_edge!(sim, to, edge)
        end
    end
end

# # we send the number of edges with the id
# function sendnumedges!(sim, sendmap::Dict{AgentID, ProcessID}, T::DataType)
#     ST = Vector{Tuple{AgentID, Int64}} 
#     perPE = [ ST() for _ in 1:mpi.size ]

#     for (to, e) in edges_iterator(sim, T)
#         if has_trait(sim, T, :SingleAgentType)
#             AT = sim.typeinfos.edges_attr[T][:to_agenttype]
#             to = agent_id(sim, to, AT)
#         end
#         # in the :SingleEdge, :SingleAgentType case we check via
#         # the sendmap if there is really this agent on the PE
#         if haskey(sendmap, to)
#             push!(perPE[sendmap[to]], (to, e))
#         end
#     end
    
# end

function distribute!(sim, sendmap::Dict{AgentID, ProcessID})
    @info "In distribute!" mpi.rank 
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
#        if !(has_trait(sim, T, :Stateless) && has_trait(sim, T, :IgnoreFrom))
            sendedges!(sim, sendmap, T)
#        else
#            sendnumedges!(sim, sendmap, T)
#        end
    end

    
    foreach(finish_write!(sim), [ node_types; edge_types ])
end
