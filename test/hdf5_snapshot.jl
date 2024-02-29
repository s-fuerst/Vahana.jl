include("hdf5_common.jl")

set_hdf5_path(joinpath(dirname(@__FILE__), "h5"))

function test_write_restore(model)
    sim = runsim(model, true)
    restored = restore(model, sim)
    test(sim, restored)

    apply!(sim,
           remove_some_rasternodes,
           [ RasterAgent ],
           [ RasterAgent, RasterEdge, Agent ],
           [ RasterAgent, RasterEdge ])

    apply!(restored,
           remove_some_rasternodes,
           [ RasterAgent ],
           [ RasterAgent, RasterEdge, Agent ],
           [ RasterAgent, RasterEdge ])

    test(sim, restored)
    
    finish_simulation!(restored)
    finish_simulation!(sim)
end


function test(sim, restored)
    @test sim.Agent.read.died == restored.Agent.read.died
    @test sim.Agent.read.state == restored.Agent.read.state
    @test sim.Agent.read.reuseable == restored.Agent.read.reuseable
    @test sim.Agent.nextid == restored.Agent.nextid
    @test sim.Agent.last_change == restored.Agent.last_change

    @test sim.RasterAgent.read.died == restored.RasterAgent.read.died
    @test sim.RasterAgent.read.state == restored.RasterAgent.read.state
    @test sim.RasterAgent.read.reuseable == restored.RasterAgent.read.reuseable
    @test sim.RasterAgent.nextid == restored.RasterAgent.nextid
    @test sim.RasterAgent.last_change == restored.RasterAgent.last_change

    @test sim.params.pa == restored.params.pa
    @test sim.params.pb == restored.params.pb
    @test sim.params.pos == restored.params.pos
    #    @test sim.globals == restored.globals

    @test sim.globals_last_change == restored.globals_last_change

    function checkedges(sim_edges, restored_edges, T)
        @test getproperty(sim, Symbol(T)).last_change ==
            getproperty(restored, Symbol(T)).last_change
        if Vahana.has_hint(sim, T, :SingleType)
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

@testset "Snapshot" begin
    model_default = ModelTypes() |>
        register_agenttype!(Agent) |>
        register_agenttype!(RasterAgent) |>
        register_edgetype!(EdgeState) |>
        register_edgetype!(RasterEdge) |>
        register_edgetype!(StatelessEdge) |>
        register_agenttype!(EmptyAgentVector) |>
        register_edgetype!(EmptyEdgeVector) |> 
        create_model("hdf5_default")

    test_write_restore(model_default)

    model_immortal = ModelTypes() |>
        register_agenttype!(Agent, :Immortal) |>
        register_agenttype!(RasterAgent) |>
        register_edgetype!(EdgeState, :IgnoreFrom) |>
        register_edgetype!(RasterEdge) |>
        register_edgetype!(StatelessEdge, :Stateless) |>
        create_model("hdf5_ignore_immortal")

    test_write_restore(model_immortal)

    model_neighbors = ModelTypes() |>
        register_agenttype!(Agent, :Immortal) |>
        register_agenttype!(RasterAgent) |>
        register_edgetype!(EdgeState, :NumEdgesOnly) |>
        register_edgetype!(RasterEdge) |>
        register_edgetype!(StatelessEdge, :HasEdgeOnly, :SingleType; target = Agent) |>
        create_model("hdf5_neighbors")

    test_write_restore(model_neighbors)

    # this hack should help that the output is not scrambled
    sleep(mpi.rank * 0.05)
end

@testset "Metadata" begin
    mutable struct ParGlo
        vec::Vector{Int64}
        mat::Matrix{Int64}
        arr::Array{Int64, 3}
        empty::Vector{Int64}
    end

    struct MetaDataCell end
    
    model_arrays = ModelTypes() |>
        register_agenttype!(MetaDataCell) |> 
        create_model("Metadata")
    sim = create_simulation(model_arrays,
                            ParGlo([1, 2, 3], [1 2 3; 4 5 6],
                                   ones(2,3,4), Int64[]),
                            ParGlo([1, 2, 3], [1 2 3; 4 5 6],
                                   ones(2,3,4), Int64[]);
                            name = "Metadata_sim")

    add_raster!(sim,
                :grid,
                (10,8),
                _ -> MetaDataCell())

    # check that we can also write metadata before finish_inith!
    write_metadata(sim, :Param, :empty, :foo, 1)
    write_metadata(sim, :Raster,:grid, :bar, 2)
    write_metadata(sim, :foo, 2)
    finish_init!(sim; partition_algo = :EqualAgentNumbers)
    write_metadata(sim, :Global, :mat, :dim1, "hhtype")
    write_snapshot(sim)
    write_metadata(sim, :Global, :mat, :dim2, "agegroup")
    finish_simulation!(sim)

    sim = create_simulation(model_arrays,
                            ParGlo(Int64[], zeros(1,1),
                                   zeros(1,1,1), Int64[]),
                            ParGlo(Int64[], zeros(1,1),
                                   zeros(1,1,1), Int64[]);
                            name = "Metadata_sim")
    
    read_snapshot!(sim)
    @test sim.params.vec == [1, 2, 3]
    @test sim.params.mat == [1 2 3; 4 5 6]
    @test sim.params.arr == ones(2,3,4)
    @test sim.params.empty == Int64[]
    @test sim.globals.vec == [1, 2, 3]
    @test sim.globals.mat == [1 2 3; 4 5 6]
    @test sim.globals.arr == ones(2,3,4)
    @test sim.globals.empty == Int64[]
    @test read_metadata(sim, :Param, :empty, :foo) == 1
    @test read_metadata(sim, :Global, :mat, :dim1) == "hhtype"
    @test read_metadata(sim, :Global, :mat, :dim2) == "agegroup"
    @test read_metadata(sim, :Global, :mat, :nonsense) == nothing
    @test read_metadata(sim, :Global, :nonsense, :nonsense) == nothing
    @test read_metadata(sim, :Raster, :grid, :bar) == 2

    asdict = read_metadata(sim, :Global, :mat)
    @test asdict[:dim1] == "hhtype"

    @test read_metadata(sim, :foo) == 2
    asdict = read_metadata(sim)
    asdict[:model_name] == "Metadata"
    asdict[:simulation_name] == "Metadata_sim"

    finish_simulation!(sim)

    # this hack should help that the output is not scrambled
    sleep(mpi.rank * 0.05)
end
