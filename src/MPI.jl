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
    # the communicated datatype, we need the AgentID to create
    # a mappning from the old AgentID to the new AgentID
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

    sendNumElems = [ length(perPE[i]) for i in 1:mpi.size ]
    recvNumElems = MPI.Alltoall(UBuffer(sendNumElems, 1), mpi.comm)
    recvbuf = MPI.VBuffer(ST(undef, sum(recvNumElems)), recvNumElems)

    MPI.Alltoallv!(sendbuf, recvbuf, mpi.comm)
    # we need oldid as key and newid as value
    idmapping = Dict{AgentID, AgentID}() 
    foreach(recvbuf.data) do (id, agent)
        idmapping[id] = add_agent!(sim, agent)
    end
    idmapping
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
function sendedges!(sim, sendmap::Dict{AgentID, ProcessID}, idmapping, T::DataType)
    CE = sim.typeinfos.edges_attr[T][:containerelement]
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
        id = if has_trait(sim, T, :SingleAgentType)
            AT = sim.typeinfos.edges_attr[T][:to_agenttype]
            agent_id(sim, to, AT)
        else
            to
        end
        # in the :SingleEdge, :SingleAgentType case we check via
        # the sendmap if there is really this agent on the PE
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

    longvec = reduce(vcat, perPE)
    sendbuf = if length(longvec) > 0
        VBuffer(longvec, [ length(perPE[i]) for i in 1:mpi.size ])
    else
        VBuffer(ST(), [ 0 for _ in 1:mpi.size ])
    end

    sendNumElems = [ length(perPE[i]) for i in 1:mpi.size ]
    recvNumElems = MPI.Alltoall(UBuffer(sendNumElems, 1), mpi.comm)
    recvbuf = MPI.VBuffer(ST(undef, sum(recvNumElems)),
                          recvNumElems)

    MPI.Alltoallv!(sendbuf, recvbuf, mpi.comm)

    transform(id::AgentID) = idmapping[id] |> AgentID
    
    if has_trait(sim, T, :Stateless) && has_trait(sim, T, :IgnoreFrom)
        container = getproperty(sim, writefield(Symbol(T)))
         
        for (to, numedges) in recvbuf.data
            @assert numedges > 0
            to = transform(to)
            add_edge!(sim, AgentID(0), to, T())
            if has_trait(sim, T, :SingleAgentType)
                container[agent_nr(to)] = numedges
            else
                container[to] = numedges
            end
        end
    elseif has_trait(sim, T, :Stateless)
        for (to, from) in recvbuf.data
            add_edge!(sim, from, transform(to), T())
        end
    elseif has_trait(sim, T, :IgnoreFrom)
        for (to, edgestate) in recvbuf.data
            # the fromid will be ignored, so we use an dummy id
            add_edge!(sim, AgentID(0), transform(to), edgestate)
        end
    else
        for (to, edge) in recvbuf.data
            add_edge!(sim, transform(to), edge)
        end
    end
end

function distribute!(sim, sendmap::Dict{AgentID, ProcessID})
    node_types = sim.typeinfos.nodes_types
    edge_types = sim.typeinfos.edges_types
    # We reconstruct the whole graph
    foreach(prepare_write!(sim, []), [ node_types; edge_types ])

    # Send all agentstates and collect the idmapping
    idmapping = Dict{Int64, Int64}()
    ssm = create_structured_send_map(sim, sendmap)
    for T in node_types
        merge!(idmapping, sendagents!(sim, ssm[T], T))
    end

    for T in edge_types
        sendedges!(sim, sendmap, idmapping, T)
    end

    foreach(finish_write!(sim), [ node_types; edge_types ])
end
