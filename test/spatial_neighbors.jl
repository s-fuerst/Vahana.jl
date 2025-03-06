using Vahana

using Test

import Vahana: @onrankof, disable_transition_checks

struct AgentWithPosFrom
    pos::Tuple{Int64, Int64}
end    

struct AgentWithPosTo
    pos::Tuple{Int64, Int64}
end    

struct Agent3DFrom
    pos::Tuple{Float64, Float64, Float64}
end

struct Agent3DTo
    pos::Tuple{Float64, Float64, Float64}
end

struct Neighbor end

spatial_model = ModelTypes() |>
    register_agenttype!(AgentWithPosFrom) |> 
    register_agenttype!(AgentWithPosTo) |>
    register_agenttype!(Agent3DFrom) |> 
    register_agenttype!(Agent3DTo) |>
    register_edgetype!(Neighbor) |>
    create_model("Spatial_Neighbors")

@testset "connect_spatial_neighbors" begin
    sim = create_simulation(spatial_model)

    # We add source agents (From) with different positions
    from1 = add_agent!(sim, AgentWithPosFrom((5,5)))
    from2 = add_agent!(sim, AgentWithPosFrom((5,5)))
    from3 = add_agent!(sim, AgentWithPosFrom((4,4)))
    from4 = add_agent!(sim, AgentWithPosFrom((4,5)))
    from5 = add_agent!(sim, AgentWithPosFrom((4,6)))

    # And target agents (To) at the same positions
    to1 = add_agent!(sim, AgentWithPosTo((5,5)))
    to2 = add_agent!(sim, AgentWithPosTo((5,5)))
    to3 = add_agent!(sim, AgentWithPosTo((4,4)))
    to4 = add_agent!(sim, AgentWithPosTo((4,5)))
    to5 = add_agent!(sim, AgentWithPosTo((4,6)))

    finish_init!(sim)

    # Connect agents within distance 1.5
    # This should connect FromAgents to ToAgents within the distance
    # - (5,5) with (4,5) (distance = 1.0)
    # - (4,4) with (4,5) (distance = 1.0)
    # - (4,5) with (4,6) (distance = 1.0)
    # - (4,4) with (4,6) (distance = 2.0 > 1.5, not connected)
    # - (5,5) with (4,4) (distance = √2 ≈ 1.414 < 1.5)
    # - (5,5) with (4,6) (distance = √2 ≈ 1.414 < 1.5)
    connect_spatial_neighbors!(sim, AgentWithPosFrom, AgentWithPosTo, 
                               Neighbor,
                               distance = 1.5)
    
    # Verify that connections were created correctly
    # FromAgents at (5,5) should connect to ToAgents at (5,5), (4,4), (4,5), and (4,6)
    # FromAgents at (4,4) should connect to ToAgents at (4,4), (4,5), and (5,5)
    # etc.
    
    disable_transition_checks(true)
    # First from agent at (5,5) should connect to 5 to agents: (5,5)x2, (4,4), (4,5), (4,6)
    @test num_edges(sim, to1, Neighbor) == 5
    # Second to agent at (5,5) should also connect to 5 to agents
    @test num_edges(sim, to2, Neighbor) == 5
    # To agent at (4,4) should connect to 3 to agents: (4,4), (4,5), (5,5)x2
    @test num_edges(sim, to3, Neighbor) == 4
    # To agent at (4,5) should connect to 5 to agents: (4,5), (4,4), (4,6), (5,5)x2
    @test num_edges(sim, to4, Neighbor) == 5
    # To agent at (4,6) should connect to 4 to agents: (4,6), (4,5), (5,5)x2
    @test num_edges(sim, to5, Neighbor) == 4
    
    disable_transition_checks(false)
    
    finish_simulation!(sim)
    
    sim = create_simulation(spatial_model)
    
    # Add from agents with 3D positions
    from1 = add_agent!(sim, Agent3DFrom((0.0, 0.0, 0.0)))
    from2 = add_agent!(sim, Agent3DFrom((1.0, 0.0, 0.0)))
    from3 = add_agent!(sim, Agent3DFrom((0.0, 1.0, 0.0)))
    from4 = add_agent!(sim, Agent3DFrom((0.0, 0.0, 1.0)))
    from5 = add_agent!(sim, Agent3DFrom((2.0, 2.0, 2.0)))
    
    # Add to agents with 3D positions at the same locations
    to1 = add_agent!(sim, Agent3DTo((0.0, 0.0, 0.0)))
    to2 = add_agent!(sim, Agent3DTo((1.0, 0.0, 0.0)))
    to3 = add_agent!(sim, Agent3DTo((0.0, 1.0, 0.0)))
    to4 = add_agent!(sim, Agent3DTo((0.0, 0.0, 1.0)))
    to5 = add_agent!(sim, Agent3DTo((2.0, 2.0, 2.0)))
    
    finish_init!(sim)
    
    # Connect agents within distance 1.5
    connect_spatial_neighbors!(sim, Agent3DFrom, Agent3DTo,
                               Neighbor,
                               distance = 1)
    
    # Verify 3D connections
    disable_transition_checks(true)
    @test num_edges(sim, to1, Neighbor) == 4  # Connected to all to-agents within distance 1.5
    @test num_edges(sim, to2, Neighbor) == 2  # Connected to to1 and to2
    @test num_edges(sim, to3, Neighbor) == 2  # Connected to to1 and to3
    @test num_edges(sim, to4, Neighbor) == 2  # Connected to to1 and to4
    @test num_edges(sim, to5, Neighbor) == 1  # Connected only to to5
    disable_transition_checks(false)
    
    finish_simulation!(sim)
end

