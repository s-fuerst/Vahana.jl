function sendagents!(sim, movemap::Dict{AgentID, ProcessID}, agenttypes)
    for at in agenttypes
        perPE = [ Vector{AgentID}() for _ in 1:mpi.size ]
        for (id, p) in movemap
            if Vahana.type_of(sim, id) == at
                push!(perPE[p], id)
            end
        end
        @show at perPE
    end
end
