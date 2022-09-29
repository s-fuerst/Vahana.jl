struct ReuseAgent end

sim = ModelTypes() |>
    register_agenttype!(ReuseAgent) |>
    construct_model("Reuse2") |> 
    new_simulation(nothing, nothing)

add_agent!(sim, ReuseAgent())

finish_init!(sim)

expected_reuse = 0

function add_new_remove_me(_, id, sim)
    global expected_reuse
    @test Vahana.reuse_nr(id) == expected_reuse
    add_agent!(sim, ReuseAgent())
    nothing
end

@testset "Reuse" begin
    global expected_reuse

    # index 2 is added, index 1 is set to died
    # the function is receiving reuse 0 index 1
    apply_transition!(sim, add_new_remove_me, [ ReuseAgent ], [], [])
    @test num_agents(sim, ReuseAgent) == 1
    @test sim.ReuseAgent_died[1] == true
    @test length(sim.ReuseAgent_read.reuseable) == 1

    # the function is receiving reuse 0 index 2
    apply_transition!(sim, add_new_remove_me, [ ReuseAgent ], [], [])
    @test num_agents(sim, ReuseAgent) == 1
    # the agent should be reused now
    @test sim.ReuseAgent_died[1] == false
    @test sim.ReuseAgent_died[2] == true
    @test length(sim.ReuseAgent_read.reuseable) == 1

    # the function is receiving reuse 1 index 1
    expected_reuse = 1

    apply_transition!(sim, add_new_remove_me, [ ReuseAgent ], [], [])
    @test sim.ReuseAgent_died[1] == true
    @test sim.ReuseAgent_died[2] == false

    # the function is receiving reuse 1 index 2
    apply_transition!(sim, add_new_remove_me, [ ReuseAgent ], [], [])
    @test sim.ReuseAgent_died[1] == true
    @test sim.ReuseAgent_died[2] == false

    # for i in 2:5
    #     expected_reuse = i
    # apply_transition!(sim, add_new_remove_me, [ ReuseAgent ], [], [])
    # apply_transition!(sim, add_new_remove_me, [ ReuseAgent ], [], [])
    # end        
        
end
