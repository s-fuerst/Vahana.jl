struct ReuseAgent end

sim = ModelTypes() |>
    register_agenttype!(ReuseAgent) |>
    construct_model("Reuse2") |> 
    new_simulation(nothing, nothing)

add_agent!(sim, ReuseAgent())

finish_init!(sim)

function add_new_remove_me(_, id, sim)
    add_agent!(sim, ReuseAgent())
    nothing
end

apply_transition!(sim, add_new_remove_me, [ ReuseAgent ], [], [])

@test num_agents(sim, ReuseAgent) == 1

@test sim.ReuseAgent_died[1] == true

@test length(sim.ReuseAgent_read.reuseable) == 1

apply_transition!(sim, add_new_remove_me, [ ReuseAgent ], [], [])

@test num_agents(sim, ReuseAgent) == 1

# the agent should be reused now
@test sim.ReuseAgent_died[1] == false

@test sim.ReuseAgent_died[2] == true

@test length(sim.ReuseAgent_read.reuseable) == 1
