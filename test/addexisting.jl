struct ComputeAgent end
struct ConstructedAgent end
struct Connection end

function test_model(model)
    sim = create_simulation(model)

    computeid = add_agent!(sim, ComputeAgent())
    constructedid = add_agent!(sim, ConstructedAgent())
    add_edge!(sim, constructedid, computeid, Connection())

    finish_init!(sim)

    # add_existing is empty and we have added ConstructedAgent and
    # Connection to rebuild, so after the transition function only the
    # ComputeAgent exists anymore
    apply!(sim,
                      [ ComputeAgent ],
                      [],
                      [ ConstructedAgent, Connection ]) do _, id, sim
                      end

    @test num_agents(sim, ComputeAgent) == 1
    @test num_agents(sim, ConstructedAgent) == 0
    @test num_edges(sim, Connection) == 0

    
    # this time we readd the second agent and the edge
    apply!(sim, [ ComputeAgent ], [], [ ConstructedAgent, Connection ]) do _, id, sim
        constructedid = add_agent!(sim, ConstructedAgent())
        add_edge!(sim, constructedid, computeid, Connection())
    end

    @test num_agents(sim, ComputeAgent) == 1
    @test num_agents(sim, ConstructedAgent) == 1
    @test num_edges(sim, Connection) == 1

    # and as we add now ConstructedAgent and Connection to add_existing, we should
    # still have them even with this "do nothing" transition function. And thanks to
    # invariant_compute, we also must not return the ComputeAgent 
    apply!(sim,
                      [ ComputeAgent ], [], [ ConstructedAgent, Connection ];
                      add_existing = [ ConstructedAgent, Connection ]) do _, id, sim
                      end

    @test num_agents(sim, ConstructedAgent) == 1
    @test num_agents(sim, ComputeAgent) == 1
    @test num_edges(sim, Connection) == 1

    finish_simulation!(sim)
end

function test_assertion(model)
    sim = create_simulation(model)

    computeid = add_agent!(sim, ComputeAgent())
    constructedid = add_agent!(sim, ConstructedAgent())
    add_edge!(sim, constructedid, computeid, Connection())

    finish_init!(sim)

    @test_throws AssertionError apply!(sim,
                                                  [ ComputeAgent ],
                                                  [],
                                                  [ ConstructedAgent,
                                                    Connection ]) do state, _, _
                                                        state
                                                    end
    finish_simulation!(sim)
end


@testset "addexisiting" begin
    model = ModelTypes() |>
        register_agenttype!(ComputeAgent) |>
        register_agenttype!(ConstructedAgent) |>
        register_edgetype!(Connection) |>
        create_model("Test add_existing")

    test_model(model)

    model_imm_comp = ModelTypes() |>
        register_agenttype!(ComputeAgent, :Immortal) |>
        register_agenttype!(ConstructedAgent) |>
        register_edgetype!(Connection) |>
        create_model("Test add_existing vector")

    test_model(model_imm_comp)

    model_imm_cons = ModelTypes() |>
        register_agenttype!(ComputeAgent) |>
        register_agenttype!(ConstructedAgent, :Immortal) |>
        register_edgetype!(Connection) |>
        create_model("Test add_existing vector")

    test_assertion(model_imm_cons)
end

