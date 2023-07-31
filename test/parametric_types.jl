@testset "Parametric T." begin
    struct PAgent{T}
        t::T
    end

    struct PAgent2
        t::Float64
    end

    struct PEdge{T}
        t::T
    end

    
    model = ModelTypes() |>
        register_agenttype!(PAgent{Float64}) |>
        register_agenttype!(PAgent2) |>
        register_edgetype!(PEdge{Float64}) |>
        create_model("parametric types")

    sim = create_simulation(model)

    id1 = add_agent!(sim, PAgent(1.0))
    id2 = add_agent!(sim, PAgent2(2.0))
    add_edge!(sim, id1, id2, PEdge(3.0))

    finish_init!(sim)

    apply!(sim, PAgent2, [ PAgent{Float64}, PAgent2, PEdge{Float64} ], PAgent2) do agent, id, sim
        @test (edges(sim, id, PEdge{Float64}) |> first).state.t == 3.0
        @test edgestates(sim, id, PEdge{Float64}) == [ PEdge(3.0) ]
        @test neighborstates(sim, id, PEdge{Float64}, PAgent{Float64}) ==
            [ PAgent(1.0) ]
        agent
    end

    finish_simulation!(sim)
end
