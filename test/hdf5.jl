using Revise

using Vahana

struct Agent
    i::Int64
    f::Float64
end

struct StatelessAgent end

struct EdgeState
    f::AgentID
    t::AgentID
end

struct StatelessEdge end

struct Globals
    ga::Int64
    gb::Int64
end

struct Params
    pa::Int64
    pb::Int64
end

function test(model)
    sim = model |>
        new_simulation(Params(1,2), Globals(1,2))

    ids = add_agents!(sim, [ Agent(i,i) for i in 1:(5 * mpi.size) ])

    for id in ids
        add_edge!(sim, id, id, EdgeState(id, id))
        add_edge!(sim, id, ids[2], StatelessEdge())
        add_edge!(sim, id, ids[2], EdgeState(id, ids[1]))
    end

    add_agent!(sim, StatelessAgent())

    add_raster!(sim, :vierdspace, (2,2,2,1), _ -> StatelessAgent())

    finish_init!(sim;
                 partition_algo = :EqualAgentNumbers)


    write_snapshot(sim)

    apply_transition!(sim, [ Agent ], [ Agent, EdgeState ], [ Agent ]) do state, id, sim
        num_neighbors(sim, id, EdgeState) == 1 ? nothing : state
    end

    write_agents(sim, [ Agent ])


    sim.params = Params(3,4)
    
    write_params(sim)

    close(sim.h5file)
    #    finish_simulation!(sim)
    sim
end

model = ModelTypes() |>
    register_agenttype!(Agent) |>
    register_agenttype!(StatelessAgent) |>
    register_edgetype!(EdgeState) |>
    register_edgetype!(StatelessEdge) |>
    construct_model("hdf5_default")

test(model)

model = ModelTypes() |>
    register_agenttype!(Agent) |>
    register_agenttype!(StatelessAgent, :Immortal) |>
    register_edgetype!(EdgeState, :IgnoreFrom) |>
    register_edgetype!(StatelessEdge) |>
    construct_model("hdf5_ignore_immortal")

test(model)


model = ModelTypes() |>
    register_agenttype!(Agent) |>
    register_agenttype!(StatelessAgent, :Immortal) |>
    register_edgetype!(EdgeState, :NumNeighborsOnly) |>
    register_edgetype!(StatelessEdge, :HasNeighborOnly, :SingleAgentType;
                       to_agenttype = Agent) |>
    construct_model("hdf5_neighbors")

sim  = test(model)
