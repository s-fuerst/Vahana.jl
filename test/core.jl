# A for Agent, one type per difference container type
struct ADict     foo::Int64 end
struct AVec      foo::Int64 end
struct AVecFixed foo::Int64 end
struct ASLDict end

# ES = EdgeState and ESL = EdgeStateless
struct ESDict  foo::Int64 end
struct ESLDict1 end
struct ESLDict2 end

model = ModelTypes() |>
    add_agenttype!(ADict) |>
    add_agenttype!(AVec, :Vector) |>
    add_agenttype!(AVecFixed, :Vector; size = 10) |>
    add_agenttype!(ASLDict) |>
    add_edgetype!(ESDict) |>
    add_edgetype!(ESLDict1) |> 
    add_edgetype!(ESLDict2, :SingleAgentType; to_agenttype = AVec) |> # to = AVec 
    construct_model("Core")

function add_example_network!(sim)
    # construct 3 ADict agents, 10 AVec agents and 10 AVecFixed
    a1id = add_agent!(sim, ADict(1))
    a2id, a3id = add_agents!(sim, ADict(2), ADict(3))
    avids = add_agents!(sim, [ AVec(i) for i in 1:10])
    avfids = add_agents!(sim, [ AVecFixed(i) for i in 1:10])

    # we construct the following network for ESDict:
    # a2 & a3 & avids[1] & avfids[10] -> a1
    add_edge!(sim, a2id, a1id, ESDict(1))
    add_edge!(sim, a3id, a1id, ESDict(2))
    add_edge!(sim, avids[1], a1id, ESDict(3))
    add_edge!(sim, avfids[10], a1id, ESDict(4))
    # we construct the following network for ESLVec:
    # avids -> a1
    add_edges!(sim, a1id, [ Edge(avids[i], ESLDict1()) for i in 1:10 ])
    # we construct the following network for ESLVecFixed:
    # avfids[i] -> avids[i]
    for i in 1:10
        add_edge!(sim, avfids[i], avids[i], ESLDict2())
    end

    (a1id, a2id, a3id, avids, avfids)
end

function create_sum_state_neighbors(edgetypeval) 
    function sum_state_neighbors(agent, id, sim)
        nstates = neighborstates_flexible(sim, id, edgetypeval)
        s = 0
        if !isnothing(nstates)
            for n in nstates
                s = s + n.foo
            end
        end
        typeof(agent)(s)
    end
end

function nothing_transition(agent, id, sim)
    nothing
end


@testset "Core" begin
    sim = new_simulation(model, nothing, nothing)
    
    (a1id, a2id, a3id, avids, avfids) = add_example_network!(sim)
    
    finish_init!(sim)

    @testset "agentstate" begin
        @test agentstate_flexible(sim, a1id) == ADict(1)
        @test agentstate(sim, a2id, ADict) == ADict(2)
        @test agentstate(sim, avids[1], AVec) == AVec(1)
        @test agentstate(sim, avfids[1], AVecFixed) == AVecFixed(1)
        @test agentstate(sim, avids[10], AVec) == AVec(10)
        @test agentstate(sim, avfids[10], AVecFixed) == AVecFixed(10)
        # calling agentstate with the wrong typ should throw an AssertionError
        @test_throws AssertionError agentstate(sim, a2id, AVec)
        @test_throws AssertionError agentstate(sim, avids[1], ADict)
    end

    @testset "edges_to" begin
        @test size(edges_to(sim, a1id, ESDict), 1) == 4
        @test size(edges_to(sim, a1id, ESLDict1), 1) == 10
        # Check that we can call edges_to also for an empty set of neighbors
        @test edges_to(sim, a2id, ESDict) === nothing
        @test edges_to(sim, a2id, ESLDict1) === nothing
    end

    @testset "neighbors & edgestates" begin
        @test length(neighborids(sim, avids[1], ESLDict2)) == 1
        @test length(neighborids(sim, avids[10], ESLDict2)) == 1

        @test_throws AssertionError neighborids(sim, a2id, ESLDict2)

        edges = edges_to(sim, a1id, ESLDict1)
        @test (edges)[1].from == avids[1]
        @test (edges)[10].from == avids[10]
        edges = neighborids(sim, avids[10], ESLDict2)
        @test edges[1] == avfids[10]
    end

    @testset "neighborstates" begin
        @test neighborstates(sim, a1id, ESLDict1, AVec)[1] == AVec(1)
        @test neighborstates_flexible(sim, a1id, ESDict)[2] == ADict(3)
    end

    # working on a deepcopy should be possible and not change the
    # original state
    @testset "deepcopy" begin
        copy = deepcopy(sim)
        add_edge!(copy, a2id, a1id, ESDict(20))
        add_edge!(copy, a2id, avids[1], ESLDict2())
        @test_throws AssertionError add_edge!(copy, a2id, avfids[1], ESLDict2())
        @test size(edges_to(sim, a1id, ESDict), 1) == 4
        @test size(edges_to(copy, a1id, ESDict), 1) == 5
        @test size(neighborids(sim, avids[1], ESLDict2), 1) == 1
        @test size(neighborids(copy, avids[1], ESLDict2), 1) == 2
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
        apply_transition!(copydict, create_sum_state_neighbors(ESLDict1),
                          [ ADict ], [ ESLDict1 ], [])
        @test agentstate(copydict, a1id, ADict) == ADict(sum(1:10) + 1)
        apply_transition!(copydict, create_sum_state_neighbors(ESLDict1),
                          [ ADict ], [ ESLDict1 ], [])
        @test agentstate(copydict, a1id, ADict) == ADict(2 * sum(1:10) + 1)

        copyvec = deepcopy(copy)
        apply_transition!(copyvec, create_sum_state_neighbors(ESLDict2),
                          [ AVec ], [ ESLDict2 ], [])
        @test agentstate(copyvec, avids[1], AVec) == AVec(2)
        apply_transition!(copyvec, create_sum_state_neighbors(ESLDict2),
                          [ AVec ], [ ESLDict2 ], [])
        @test agentstate(copyvec, avids[1], AVec) == AVec(3)
        apply_transition!(copyvec, create_sum_state_neighbors(ESLDict2),
                          [ AVec ], [ ESLDict2 ], [])
        @test agentstate(copyvec, avids[1], AVec) == AVec(4)

        copyvecfix = deepcopy(copy)
        apply_transition!(copyvecfix, create_sum_state_neighbors(ESLDict1),
                          [ AVecFixed ], [ ESLDict1 ], [])
        @test agentstate(copyvecfix, avfids[1], AVecFixed) == AVecFixed(3)
        apply_transition!(copyvecfix, create_sum_state_neighbors(ESLDict1),
                          [ AVecFixed ], [ ESLDict1 ], [])
        @test agentstate(copyvecfix, avfids[1], AVecFixed) == AVecFixed(5)
        apply_transition!(copyvecfix, create_sum_state_neighbors(ESLDict1),
                          [ AVecFixed ], [ ESLDict1 ], [])
        @test agentstate(copyvecfix, avfids[1], AVecFixed) == AVecFixed(7)

        # check return nothing (currently only supported by Dicts)
        apply_transition!(copydict, nothing_transition, [ ADict ], [], [])
        @test_throws KeyError agentstate_flexible(copydict, a1id)
    end

    @testset "num_neighbors" begin
        @test num_neighbors(sim, a1id, ESDict) == 4
        @test num_neighbors(sim, a2id, ESDict) == 0
        @test_throws AssertionError num_neighbors(sim, a2id, ESLDict2)
        @test num_neighbors(sim, avids[1], ESLDict2) == 1
    end
    

    # TODO transition with add_agent! and add_edge!
end


@testset "Aggregate" begin
    sim = new_simulation(model, nothing, nothing; name = "Aggregate")

    (a1id, a2id, a3id, avids, avfids) = add_example_network!(sim)
    
    finish_init!(sim)
    
    @test aggregate(sim, a -> a.foo, +, ADict) == 6
    @test aggregate(sim, a -> a.foo, +, AVec) == sum(1:10)
    @test aggregate(sim, e -> e.foo, +, ESDict) == 10
end

