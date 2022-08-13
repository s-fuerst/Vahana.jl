using MPI

using Logging


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
    @info "received buffer" mpi.rank recvbuf
end

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


function distribute!(sim, sendmap::Dict{AgentID, ProcessID})
    ssm = create_structured_send_map(sim, sendmap)
    for T in sim.typeinfos.nodes_types
        sendagents!(sim, ssm[T], T)
    end
end
