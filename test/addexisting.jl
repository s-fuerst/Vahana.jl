struct ComputeAgent end
struct ConstructedAgent end
struct Connection end

function test_for_model(model)
    sim = new_simulation(model)

    computeid = add_agent!(sim, ComputeAgent())
    constructedid = add_agent!(sim, ConstructedAgent())
    add_edge!(sim, constructedid, computeid, Connection())

    finish_init!(sim)

    # add_existing is empty and we have added ConstructedAgent and
    # Connection to rebuild, so after the transition function only the
    # ComputeAgent exists anymore
    apply_transition!(sim, [ ComputeAgent ], [], [ ConstructedAgent, Connection ]) do state, id, sim
        state
    end

    @test num_agents(sim, ComputeAgent) == 1
    @test num_agents(sim, ConstructedAgent) == 0
    @test num_edges(sim, Connection) == 0

    
    # this time we readd the second agent and the edge
    apply_transition!(sim, [ ComputeAgent ], [], [ ConstructedAgent, Connection ]) do state, id, sim
        constructedid = add_agent!(sim, ConstructedAgent())
        add_edge!(sim, constructedid, computeid, Connection())
        state
    end

    @test num_agents(sim, ComputeAgent) == 1
    @test num_agents(sim, ConstructedAgent) == 1
    @test num_edges(sim, Connection) == 1

    # and as we add now ConstructedAgent and Connection to add_existing, we should
    # still have them even with this "do nothing" transition function. And thanks to
    # invariant_compute, we also must not return the ComputeAgent 
    apply_transition!(sim, [ ComputeAgent ], [], [ ConstructedAgent, Connection ];
                      add_existing = [ ConstructedAgent, Connection ],
                      invariant_compute = true) do state, id, sim
                          nothing
                      end

    @test num_agents(sim, ConstructedAgent) == 1
    @test num_agents(sim, ComputeAgent) == 1
    @test num_edges(sim, Connection) == 1
end

@testset "addexisiting" begin
    model = ModelTypes() |>
        register_agenttype!(ComputeAgent) |>
        register_agenttype!(ConstructedAgent) |>
        register_edgetype!(Connection) |>
        construct_model("Test add_existing")

    test_for_model(model)

    model_vec = ModelTypes() |>
        register_agenttype!(ComputeAgent) |>
        register_agenttype!(ConstructedAgent, :Vector) |>
        register_edgetype!(Connection) |>
        construct_model("Test add_existing vector")

    test_for_model(model_vec)
end
