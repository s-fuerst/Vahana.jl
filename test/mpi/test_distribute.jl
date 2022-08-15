using Test

using Vahana

# All ids are the initial ids 

@assert mod(mpi.size, 2) == 0 We need as minimum 2 PEs and also an even number of PEs

struct AgentState1
    id::Int64
end

struct AgentState2
    id::Int64
    something::Bool
end

struct EdgeState
    from::AgentID
    to::AgentID
end


model = ModelTypes() |>
    register_agenttype!(AgentState1) |>
    register_agenttype!(AgentState2) |>
    register_edgetype!(EdgeState) |>
    construct_model("MPI Basics");

sim = new_simulation(model, nothing, nothing)

part = Dict{AgentID, ProcessID}()

if mpi.isroot 
    agentids = add_agents!(sim, [ AgentState1(i) for i in 1:mpi.size ])
    agentids2 = add_agents!(sim, [ AgentState2(i, true) for i in 1:mpi.size ])
    

    for i in 1:mpi.size
        fromid = agentids[mod1(i-1, mpi.size)]
        toid = agentids[i]
        toid2 = agentids2[i]
        add_edge!(sim, fromid, toid, EdgeState(fromid, toid))
        add_edge!(sim, fromid, toid2, EdgeState(fromid, toid2))
    end

    for i in 1:mpi.size
        part[agentids[i]] = i
        part[agentids2[i]] = mod1(i, 2)
    end
end

finish_init!(sim; partition = part)

@test num_agents(sim, AgentState1) == (mpi.isroot ? mpi.size : 0)
@test num_agents(sim, AgentState2) == (mpi.isroot ? mpi.size : 0)

@test num_edges(sim, EdgeState) == (mpi.isroot ? mpi.size * 2 : 0)

Vahana.distribute!(sim, part)

@info "After distribute!" mpi.rank sim

@test num_agents(sim, AgentState1) == 1
@test num_agents(sim, AgentState2) == (mpi.rank < 2 ? mpi.size / 2 : 0)
