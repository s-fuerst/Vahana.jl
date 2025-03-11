using Revise

using Vahana

using Test

using StaticArrays

using Infiltrator

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
    pos::SVector{3, Float64}
end

struct Neighbor end

spatial_model = ModelTypes() |>
    register_agenttype!(AgentWithPosFrom) |> 
    register_agenttype!(AgentWithPosTo, :Immortal) |>
    register_agenttype!(Agent3DFrom) |> 
    register_agenttype!(Agent3DTo) |>
    register_edgetype!(Neighbor) |>
    create_model("Spatial_Neighbors")



@testset "connect_spatial_neighbors" begin
    function count_edges(edgelist, idmap, id)
        # to test this on every rank, we must remove the mpi.rank
        # from the id, as we distribute only the graph that is
        # created on rank 0
        aid = Vahana.agent_id(Vahana.type_nr(id), 0, Vahana.agent_nr(id))
        count(e -> e[1] == idmap[aid], edgelist)
    end

    sim = create_simulation(spatial_model)

    # e.g. lowercase: from, uppercase: to
    #   1   2   3   4   5
    # 1 aAB             b
    # 2 c   C
    # 3 d
    # 4
    # 5 D
    # 6             e   E
    # We add source agents (From) with different positions
    from_a = add_agent!(sim, AgentWithPosFrom((1,1)))
    from_b = add_agent!(sim, AgentWithPosFrom((5,1)))
    from_c = add_agent!(sim, AgentWithPosFrom((1,2)))
    from_d = add_agent!(sim, AgentWithPosFrom((1,3)))
    from_e = add_agent!(sim, AgentWithPosFrom((4,6)))

    # And target agents (To) at the same positions
    to_A = add_agent!(sim, AgentWithPosTo((1,1)))
    to_B = add_agent!(sim, AgentWithPosTo((1,1)))
    to_C = add_agent!(sim, AgentWithPosTo((2,2)))
    to_D = add_agent!(sim, AgentWithPosTo((1,5)))
    to_E = add_agent!(sim, AgentWithPosTo((5,6)))

    idmap = finish_init!(sim; return_idmapping = true)

    connect_spatial_neighbors!(sim,
                               AgentWithPosFrom,
                               pos_tuple(:pos, 2),
                               AgentWithPosTo, 
                               pos_tuple(:pos, 2),
                               Neighbor;
                               distance = 1.5)

    # using all_edges allow to run the tests als in parallel
    es = all_edges(sim, Neighbor)
    @test count_edges(es, idmap, to_A) == 2
    @test count_edges(es, idmap, to_B) == 2
    @test count_edges(es, idmap, to_C) == 3
    @test count_edges(es, idmap, to_D) == 0
    @test count_edges(es, idmap, to_E) == 1

    connect_spatial_neighbors!(sim,
                               AgentWithPosFrom,
                               pos_tuple(:pos, 2),
                               AgentWithPosTo, 
                               pos_tuple(:pos, 2),
                               Neighbor;
                               distance = 2.0)

    es = all_edges(sim, Neighbor)
    @test count_edges(es, idmap, to_A) == 3
    @test count_edges(es, idmap, to_B) == 3
    @test count_edges(es, idmap, to_C) == 3
    @test count_edges(es, idmap, to_D) == 1
    @test count_edges(es, idmap, to_E) == 1

    # test also connections of the same type, both To
    # should not contain the agent itself
    connect_spatial_neighbors!(sim,
                               AgentWithPosTo,
                               pos_tuple(:pos, 2),
                               AgentWithPosTo, 
                               pos_tuple(:pos, 2),
                               Neighbor;
                               distance = 3.0)

    es = all_edges(sim, Neighbor)
    @test count_edges(es, idmap, to_A) == 2
    @test count_edges(es, idmap, to_B) == 2
    @test count_edges(es, idmap, to_C) == 2
    @test count_edges(es, idmap, to_D) == 0
    @test count_edges(es, idmap, to_E) == 0

    # test periodic boundaries
    connect_spatial_neighbors!(sim,
                               AgentWithPosFrom,
                               pos_tuple(:pos, 2),
                               AgentWithPosTo, 
                               pos_tuple(:pos, 2),
                               Neighbor;
                               distance = 1.0,
                               periodic_boundaries = [(1,5), (1,6)])
    
    es = all_edges(sim, Neighbor)
    @test count_edges(es, idmap, to_A) == 3
    @test count_edges(es, idmap, to_B) == 3
    @test count_edges(es, idmap, to_C) == 1
    @test count_edges(es, idmap, to_D) == 0
    @test count_edges(es, idmap, to_E) == 2

    connect_spatial_neighbors!(sim,
                               AgentWithPosFrom,
                               pos_tuple(:pos, 2),
                               AgentWithPosTo, 
                               pos_tuple(:pos, 2),
                               Neighbor;
                               distance = 1.0,
                               periodic_boundaries = [(1,5), ()])
    
    es = all_edges(sim, Neighbor)
    @test count_edges(es, idmap, to_A) == 3
    @test count_edges(es, idmap, to_B) == 3
    @test count_edges(es, idmap, to_C) == 1
    @test count_edges(es, idmap, to_D) == 0
    @test count_edges(es, idmap, to_E) == 1
    
    
    finish_simulation!(sim)
    
    sim = create_simulation(spatial_model)

    # e.g. lowercase: from, uppercase: to
    # z:4                5               6
    #   1  2  3  4       1  2  3  4      1  2  3  4
    # 3 aA             3 c             3
    # 4                4 C             4 d        D
    # 5                5               5
    # 6 b        B     6               6 E        e

    # Add from agents with 3D positions
    from_a = add_agent!(sim, Agent3DFrom((1.0, 3.0, 4.0)))
    from_b = add_agent!(sim, Agent3DFrom((1.0, 6.0, 4.0)))
    from_c = add_agent!(sim, Agent3DFrom((1.0, 3.0, 5.0)))
    from_d = add_agent!(sim, Agent3DFrom((1.0, 4.0, 6.0)))
    from_e = add_agent!(sim, Agent3DFrom((4.0, 6.0, 6.0)))
    
    # Add to agents with 3D positions at the same locations
    to_A = add_agent!(sim, Agent3DTo(SVector(1.0, 3.0, 4.0)))
    to_B = add_agent!(sim, Agent3DTo(SVector(4.0, 6.0, 4.0)))
    to_C = add_agent!(sim, Agent3DTo(SVector(1.0, 4.0, 5.0)))
    to_D = add_agent!(sim, Agent3DTo(SVector(4.0, 4.0, 6.0)))
    to_E = add_agent!(sim, Agent3DTo(SVector(1.0, 6.0, 6.0)))
    
    idmap = finish_init!(sim; return_idmapping = true)
    
    connect_spatial_neighbors!(sim,
                               Agent3DFrom,
                               pos_tuple(:pos, 3),
                               Agent3DTo,
                               pos_tuple(:pos, 3),
                               Neighbor,
                               distance = 1.5)
    
    # Verify 3D connections
    es = all_edges(sim, Neighbor)
    @test count_edges(es, idmap, to_A) == 2
    @test count_edges(es, idmap, to_B) == 0
    @test count_edges(es, idmap, to_C) == 3
    @test count_edges(es, idmap, to_D) == 0
    @test count_edges(es, idmap, to_E) == 0
    
    finish_simulation!(sim)

    # testing periodic bounding for floating point position/ranges
    sim = create_simulation(spatial_model)
    a1 = add_agent!(sim, Agent3DTo((0.0, 0.0, 0.0)))
    a2 = add_agent!(sim, Agent3DTo((1.0, 0.0, 0.0)))
    a3 = add_agent!(sim, Agent3DTo((0.1, 0.1, 0.0)))
    a4 = add_agent!(sim, Agent3DTo((0.0, 0.0, 0.1)))
    a5 = add_agent!(sim, Agent3DTo((0.0, 0.0, 1.0)))
    a6 = add_agent!(sim, Agent3DTo((1.0, 0.0, 1.0)))
    idmap = finish_init!(sim; return_idmapping = true)

    connect_spatial_neighbors!(sim,
                               Agent3DTo,
                               pos_tuple(:pos, 3),
                               Agent3DTo,
                               pos_tuple(:pos, 3),
                               Neighbor,
                               periodic_boundaries = [(0.0,1.0), (), ()], 
                               distance = 0.1001)
    
    es = all_edges(sim, Neighbor)
    @test count_edges(es, idmap, a1) == 2 # a2, a4
    @test count_edges(es, idmap, a2) == 2 # a1, a4
    @test count_edges(es, idmap, a3) == 0
    @test count_edges(es, idmap, a4) == 2 # a1, a2
    @test count_edges(es, idmap, a5) == 1 # a6
    @test count_edges(es, idmap, a6) == 1 # a1

    connect_spatial_neighbors!(sim,
                               Agent3DTo,
                               pos_tuple(:pos, 3),
                               Agent3DTo,
                               pos_tuple(:pos, 3),
                               Neighbor,
                               periodic_boundaries =
                                   [(0.0,1.0), (0.0,1.0), (0.0, 1.0)], 
                               distance = 0.1001)
@infiltrate    
    es = all_edges(sim, Neighbor)
    @test count_edges(es, idmap, a1) == 4 # a2, a4, a5, a6
    @test count_edges(es, idmap, a2) == 4 # a1, a4, a5, a6
    @test count_edges(es, idmap, a3) == 0
    @test count_edges(es, idmap, a4) == 4 # a1, a2, a5, a6
    @test count_edges(es, idmap, a5) == 4 # a1, a2, a5, a6
    @test count_edges(es, idmap, a6) == 4 # a1, a2, a5, a6
    
    
    # this hack should help that the output is not scrambled
    sleep(mpi.rank * 0.05)
end

