@testset "Core" begin
    sim = construct(model, "Test", nothing, nothing)
    
    (a1id, a2id, a3id, avids, avfids) = add_example_network!(sim)
    
    finish_init!(sim)

    @testset "agentstate" begin
        @test agentstate_flexible(sim, a1id) == ADict(1)
        @test agentstate(sim, a2id, Val(ADict)) == ADict(2)
        @test agentstate(sim, avids[1], Val(AVec)) == AVec(1)
        @test agentstate(sim, avfids[1], Val(AVecFixed)) == AVecFixed(1)
        @test agentstate(sim, avids[10], Val(AVec)) == AVec(10)
        @test agentstate(sim, avfids[10], Val(AVecFixed)) == AVecFixed(10)
        # calling agentstate with the wrong typ should throw an AssertionError
        @test_throws AssertionError agentstate(sim, a2id, Val(AVec))
        @test_throws AssertionError agentstate(sim, avids[1], Val(ADict))
    end

    @testset "edges_to" begin
        @test size(edges_to(sim, a1id, Val(ESDict)), 1) == 4
        @test size(edges_to(sim, a1id, Val(ESLDict1)), 1) == 10
        # Check that we can call edges_to also for an empty set of neighbors
        @test size(edges_to(sim, a2id, Val(ESDict)), 1) == 0
        @test size(edges_to(sim, a2id, Val(ESLDict1)), 1) == 0
        # Check that edges_to for empty sets return the correct type for the empty vector
        @test edges_to(sim, a2id, Val(ESDict)) |> typeof ==  Vector{Edge{ESDict}}
        @test edges_to(sim, a2id, Val(ESLDict1)) |> typeof == Vector{Edge{ESLDict1}}
        @test neighborids(sim, avids[1], Val(ESLDict2)) |> typeof == Vector{AgentID}
    end

    @testset "neighbors & edgestates" begin
        @test size(neighborids(sim, avids[1], Val(ESLDict2)), 1) == 1
        @test size(neighborids(sim, avids[10], Val(ESLDict2)), 1) == 1

        @test_throws AssertionError neighborids(sim, a2id, Val(ESLDict2))

        edges = edges_to(sim, a1id, Val(ESLDict1))
        @test neighborids(edges)[1] == avids[1]
        @test neighborids(edges)[10] == avids[10]
        edges = neighborids(sim, avids[10], Val(ESLDict2))
        @test edges[1] == avfids[10]
    end

    @testset "neighborstates" begin
        @test neighborstates(sim, a1id, Val(ESLDict1), Val(AVec))[1] == AVec(1)
        @test neighborstates_flexible(sim, a1id, Val(ESDict))[2] == ADict(3)
    end

    # working on a deepcopy should be possible and not change the
    # original state
    @testset "deepcopy" begin
        copy = deepcopy(sim)
        add_edge!(copy, a2id, a1id, ESDict(20))
        add_edge!(copy, a2id, avids[1], ESLDict2())
        @test_throws AssertionError add_edge!(copy, a2id, avfids[1], ESLDict2())
        @test size(edges_to(sim, a1id, Val(ESDict)), 1) == 4
        @test size(edges_to(copy, a1id, Val(ESDict)), 1) == 5
        @test size(neighborids(sim, avids[1], Val(ESLDict2)), 1) == 1
        @test size(neighborids(copy, avids[1], Val(ESLDict2)), 1) == 2
    end
    
    @testset "transition" begin
        # we need this for each node factory
        copy = deepcopy(sim)
        # we want to check two iterations with the sum_state_neighbors,
        # so we just add an edge loop for the agents where we check the sum
        add_edge!(copy, a1id, a1id, ESLDict1())
        add_edge!(copy, avids[1], avids[1], ESLDict2())
        add_edge!(copy, avfids[1], avfids[1], ESLDict1())
        # for avfids[1] we need a second edge
        add_edge!(copy, avfids[2], avfids[1], ESLDict1())
        # and also avfids[2] should keep its value
        add_edge!(copy, avfids[2], avfids[2], ESLDict1())

        # now check apply_transtition! for the different nodefieldfactories
        copydict = deepcopy(copy)
        apply_transition!(copydict, create_sum_state_neighbors(Val(ESLDict1)),
                          [ ADict ], [ ESLDict1 ], [])
        @test agentstate(copydict, a1id, Val(ADict)) == ADict(sum(1:10) + 1)
        apply_transition!(copydict, create_sum_state_neighbors(Val(ESLDict1)),
                          [ ADict ], [ ESLDict1 ], [])
        @test agentstate(copydict, a1id, Val(ADict)) == ADict(2 * sum(1:10) + 1)

        copyvec = deepcopy(copy)
        apply_transition!(copyvec, create_sum_state_neighbors(Val(ESLDict2)),
                          [ AVec ], [ ESLDict2 ], [])
        @test agentstate(copyvec, avids[1], Val(AVec)) == AVec(2)
        apply_transition!(copyvec, create_sum_state_neighbors(Val(ESLDict2)),
                          [ AVec ], [ ESLDict2 ], [])
        @test agentstate(copyvec, avids[1], Val(AVec)) == AVec(3)
        apply_transition!(copyvec, create_sum_state_neighbors(Val(ESLDict2)),
                          [ AVec ], [ ESLDict2 ], [])
        @test agentstate(copyvec, avids[1], Val(AVec)) == AVec(4)

        copyvecfix = deepcopy(copy)
        apply_transition!(copyvecfix, create_sum_state_neighbors(Val(ESLDict1)),
                          [ AVecFixed ], [ ESLDict1 ], [])
        @test agentstate(copyvecfix, avfids[1], Val(AVecFixed)) == AVecFixed(3)
        apply_transition!(copyvecfix, create_sum_state_neighbors(Val(ESLDict1)),
                          [ AVecFixed ], [ ESLDict1 ], [])
        @test agentstate(copyvecfix, avfids[1], Val(AVecFixed)) == AVecFixed(5)
        apply_transition!(copyvecfix, create_sum_state_neighbors(Val(ESLDict1)),
                          [ AVecFixed ], [ ESLDict1 ], [])
        @test agentstate(copyvecfix, avfids[1], Val(AVecFixed)) == AVecFixed(7)

        # check return nothing (currently only supported by Dicts)
        apply_transition!(copydict, nothing_transition, [ ADict ], [], [])
        @test_throws KeyError agentstate_flexible(copydict, a1id)
    end

    @testset "num_neighbors" begin
        @test num_neighbors(sim, a1id, Val(ESDict)) == 4
        @test num_neighbors(sim, a2id, Val(ESDict)) == 0
        @test_throws AssertionError num_neighbors(sim, a2id, Val(ESLDict2))
        @test num_neighbors(sim, avids[1], Val(ESLDict2)) == 1
    end
    

    # TODO transition with add_agent! and add_edge!
end



