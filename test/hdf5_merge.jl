include("hdf5_common.jl")

function test_merge(model)
    sim = runsim(model, false)
    restored = new_simulation(model, Params(3,4), Globals(3,4))

    fids = open_h5file(restored, sim.name)
    @assert mpi.size == 1 && length(fids) > 1 """
        this should test the merge functionality (reading a distributed
        sim back to a single process), so the test should not be run 
        with mpi but the files should be written from a mpi simulation 
    """
    foreach(close, fids)
    
    @info read_agents!(restored, sim.name)

    num_agents(sim, Agent) == num_agents(restored, Agent)
end

model = ModelTypes() |>
    register_agenttype!(Agent) |>
    register_agenttype!(StatelessAgent) |>
    register_edgetype!(EdgeState) |>
    register_edgetype!(StatelessEdge) |>
    construct_model("hdf5_default")

test_merge(model)

model = ModelTypes() |>
    register_agenttype!(Agent) |>
    register_agenttype!(StatelessAgent, :Immortal) |>
    register_edgetype!(EdgeState, :IgnoreFrom) |>
    register_edgetype!(StatelessEdge) |>
    construct_model("hdf5_ignore_immortal")

# test_merge(model)


model = ModelTypes() |>
    register_agenttype!(Agent) |>
    register_agenttype!(StatelessAgent, :Immortal) |>
    register_edgetype!(EdgeState, :NumNeighborsOnly) |>
    register_edgetype!(StatelessEdge, :HasNeighborOnly, :SingleAgentType;
                       to_agenttype = Agent) |>
                           construct_model("hdf5_neighbors")

# test_merge(model)

