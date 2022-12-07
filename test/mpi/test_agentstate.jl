using Test

using Vahana

using MPI

using Logging

enable_asserts(true)

suppress_warnings(true)

# Logging.disable_logging(Logging.Info)

#MPI.set_errorhandler!(MPI.COMM_WORLD, MPI.ERRORS_RETURN)

# All ids are the initial ids 

struct Agent
    state::Int64
end

struct EdgeState
    state::Int64
end

const sim = ModelTypes() |>
    register_agenttype!(Agent) |>
    register_edgetype!(EdgeState, :SingleEdge) |>
    construct_model("agentstatetest") |>
    new_simulation()

ids = add_agents!(sim, [ Agent(i) for i in 1:mpi.size ])

for to in 2:mpi.size
    add_edge!(sim, ids[to-1], ids[to], EdgeState(to-1))
end

finish_init!(sim; partition_algo = :SameSize)

apply_transition!(sim, [ Agent ], [ Agent, EdgeState ], []) do _, id, sim
    e = edges_to(sim, id, EdgeState)
    if ! isnothing(e)
        a = agentstate(sim, e.from, Agent)
        @test e.state.state == a.state
    end
end

finish_simulation!(sim)
