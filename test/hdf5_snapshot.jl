include("hdf5_common.jl")

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
        if Vahana.has_trait(sim, T, :SingleType)
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

