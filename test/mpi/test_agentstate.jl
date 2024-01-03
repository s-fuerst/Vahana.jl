# This test should be run on multiple nodes, as it is designed to
# test the transfer of the agentstate between several nodes
using Test

using Vahana

using MPI

using Logging

enable_asserts(true)

suppress_warnings(true)

if mpi.size == mpi.shmsize
    @rootonly println("This test is only for multi-node configurations")
    exit()
end

# Logging.disable_logging(Logging.Info)

#MPI.set_errorhandler!(MPI.COMM_WORLD, MPI.ERRORS_RETURN)

# All ids are the initial ids 

struct Agent
    state::Int64
end

struct EdgeState
    state::Int64
end

struct NewEdge
    state::Int64
end

sim = ModelTypes() |>
    register_agenttype!(Agent, :Immortal) |>
    register_edgetype!(EdgeState, :SingleEdge) |>
    register_edgetype!(NewEdge, :SingleEdge) |>
    create_model("agentstatetest") |>
    create_simulation()

ids = add_agents!(sim, [ Agent(i) for i in 1:mpi.size ])

for to in 2:mpi.size
    add_edge!(sim, ids[to-1], ids[to], EdgeState(to-1))
end

newids = finish_init!(sim; partition_algo = :EqualAgentNumbers)

# first a check that we can read the state after initialization
apply!(sim, [ Agent ], [ Agent, EdgeState ], []) do _, id, sim
    e = edges(sim, id, EdgeState)
    if ! isnothing(e)
        a = agentstate(sim, e.from, Agent)
        @test e.state.state == a.state
    end
end

# now we update the agentstate and check afterwards that the
# accessible state is also updated
apply!(sim, [ Agent ], [ Agent ], [ Agent ]) do state, _, _
    Agent(state.state * 2)
end

apply!(sim, [ Agent ], [ Agent, EdgeState ], []) do _, id, sim
    e = edges(sim, id, EdgeState)
    if ! isnothing(e)
        a = agentstate(sim, e.from, Agent)
        @test e.state.state * 2 == a.state
    end
end


# when we access the agentstate via the NewEdge type, this will not transfer any
# new state
apply!(sim, [ Agent ], [ Agent ], [ Agent ]) do state, _, _
    Agent(state.state / 2)
end

apply!(sim, [ Agent ], [ Agent, NewEdge ], []) do _, id, sim
    e = edges(sim, id, NewEdge)
    @test isnothing(e)
end

# now add the NewEdges (by copying the old edges)
apply!(sim, [ Agent ], [ EdgeState ], [ NewEdge ]) do _, id, simsymbol
    e = edges(sim, id, EdgeState)
    if ! isnothing(e)
        add_edge!(sim, e.from, id, NewEdge(e.state.state))
    end
end

# and check that we get the correct agentstate via the new edges
apply!(sim, [ Agent ], [ Agent, EdgeState ], []) do _, id, sim
    e = edges(sim, id, EdgeState)
    if ! isnothing(e)
        a = agentstate(sim, e.from, Agent)
        @test e.state.state == a.state
    end
end


finish_simulation!(sim)

# Test the same for mortal agents
sim = ModelTypes() |>
    register_agenttype!(Agent) |>
    register_edgetype!(EdgeState, :SingleEdge) |>
    register_edgetype!(NewEdge, :SingleEdge) |>
    create_model("agentstatetest-mortal") |>
    create_simulation()

ids = add_agents!(sim, [ Agent(i) for i in 1:mpi.size ])

for to in 2:mpi.size
    add_edge!(sim, ids[to-1], ids[to], EdgeState(to-1))
end

newids = finish_init!(sim; partition_algo = :EqualAgentNumbers)

# first a check that we can read the state after initialization
apply!(sim, [ Agent ], [ Agent, EdgeState ], []) do _, id, sim
    e = edges(sim, id, EdgeState)
    if ! isnothing(e)
        a = agentstate(sim, e.from, Agent)
        @test e.state.state == a.state
    end
end

# call it a second time to trigger the filter of already existing keys
apply!(sim, [ Agent ], [ Agent, EdgeState ], []) do _, id, sim
    e = edges(sim, id, EdgeState)
    if ! isnothing(e)
        a = agentstate(sim, e.from, Agent)
        @test e.state.state == a.state
    end
end


# now we update the agentstate and check afterwards that the
# accessible state is also updated
apply!(sim, [ Agent ], [ Agent ], [ Agent ]) do state, _, _
    Agent(state.state * 2)
end

apply!(sim, [ Agent ], [ Agent, EdgeState ], []) do _, id, sim
    e = edges(sim, id, EdgeState)
    if ! isnothing(e)
        a = agentstate(sim, e.from, Agent)
        @test e.state.state * 2 == a.state
    end
end

# when we access the agentstate via the NewEdge type, this will not transfer any
# new state
apply!(sim, [ Agent ], [ Agent ], [ Agent ]) do state, _, _
    Agent(state.state / 2)
end

apply!(sim, [ Agent ], [ Agent, NewEdge ], []) do _, id, sim
    e = edges(sim, id, NewEdge)
    @test isnothing(e)
end

# now add the NewEdges (by copying the old edges)
apply!(sim, [ Agent ], [ EdgeState ], [ NewEdge ]) do _, id, simsymbol
    e = edges(sim, id, EdgeState)
    if ! isnothing(e)
        add_edge!(sim, e.from, id, NewEdge(e.state.state))
    end
end

# and check that we get the correct agentstate via the new edges
apply!(sim, [ Agent ], [ Agent, EdgeState ], []) do _, id, sim
    e = edges(sim, id, EdgeState)
    if ! isnothing(e)
        a = agentstate(sim, e.from, Agent)
        @test e.state.state == a.state
    end
end


finish_simulation!(sim)

# this hack should help that the output is not scrambled
sleep(mpi.rank * 0.05)
