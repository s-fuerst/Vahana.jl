include("hdf5_common.jl")

function test_merge(model)
    sim = runsim(model, false)
    restored = restore(sim)

    fids = open_h5file(restored, sim.name)
    @assert mpi.size == 1 && length(fids) > 1 """
        this should test the merge functionality (reading a distributed
        sim back to a single process), so the test should not be run 
        with mpi but the files should be written from a mpi simulation 
    """
    foreach(close, fids)

    function checkedges(sim_edges, restored_edges, T)
        if Vahana.has_trait(sim, T, :SingleAgentType)
            @test sim_edges == restored_edges
        else
            for to in keys(sim_edges)
                for edge in sim_edges[to]
                    @test edge in restored_edges[to]
                end
            end
        end
    end

    checkedges(sim.EdgeState.read, restored.EdgeState.read, EdgeState)

    checkedges(sim.RasterEdge.read, restored.RasterEdge.read, RasterEdge)

    checkedges(sim.StatelessEdge.read, restored.StatelessEdge.read, StatelessEdge)
end

model = ModelTypes() |>
    register_agenttype!(Agent) |>
    register_agenttype!(RasterAgent) |>
    register_edgetype!(EdgeState) |>
    register_edgetype!(RasterEdge) |>
    register_edgetype!(StatelessEdge) |>
    construct_model("hdf5_default")

test_merge(model)

model = ModelTypes() |>
    register_agenttype!(Agent) |>
    register_agenttype!(RasterAgent, :Immortal) |>
    register_edgetype!(EdgeState, :IgnoreFrom) |>
    register_edgetype!(RasterEdge) |>
    register_edgetype!(StatelessEdge) |>
    construct_model("hdf5_ignore_immortal")

# test_merge(model)


model = ModelTypes() |>
    register_agenttype!(Agent) |>
    register_agenttype!(RasterAgent, :Immortal) |>
    register_edgetype!(EdgeState, :NumNeighborsOnly) |>
    register_edgetype!(RasterEdge) |>
    register_edgetype!(StatelessEdge, :HasNeighborOnly, :SingleAgentType;
                       to_agenttype = Agent) |>
                           construct_model("hdf5_neighbors")

# test_merge(model)

