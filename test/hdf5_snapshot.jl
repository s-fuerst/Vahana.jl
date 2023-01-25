include("hdf5_common.jl")

rm("h5", recursive = true, force = true)

model = ModelTypes() |>
    register_agenttype!(Agent) |>
    register_agenttype!(RasterAgent) |>
    register_edgetype!(EdgeState) |>
    register_edgetype!(RasterEdge) |>
    register_edgetype!(StatelessEdge) |>
    construct_model("hdf5_default")

test_write_restore(model)

model = ModelTypes() |>
    register_agenttype!(Agent, :Immortal) |>
    register_agenttype!(RasterAgent) |>
    register_edgetype!(EdgeState, :IgnoreFrom) |>
    register_edgetype!(RasterEdge) |>
    register_edgetype!(StatelessEdge) |>
    construct_model("hdf5_ignore_immortal")

test_write_restore(model)

model = ModelTypes() |>
    register_agenttype!(Agent, :Immortal) |>
    register_agenttype!(RasterAgent) |>
    register_edgetype!(EdgeState, :NumNeighborsOnly) |>
    register_edgetype!(RasterEdge) |>
    register_edgetype!(StatelessEdge, :HasNeighborOnly, :SingleAgentType;
                       to_agenttype = Agent) |>
                           construct_model("hdf5_neighbors")

test_write_restore(model)


