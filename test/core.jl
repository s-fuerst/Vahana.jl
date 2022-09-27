import Vahana.@onrankof


# A for Agent, one type per difference container type
struct ADict     foo::Int64 end
struct AVec      foo::Int64 end
struct AVecFixed foo::Int64 end
struct ASLDict end

allagenttypes = [ ADict, AVec, AVecFixed, ASLDict ]

function create_mpi_wins(sim)
    foreach(T -> Vahana.prepare_mpi!(sim, T), allagenttypes)
end

function free_mpi_wins(sim)
    foreach(T -> Vahana.finish_mpi!(sim, T), allagenttypes)
end

# ES = EdgeState and ESL = EdgeStateless
struct ESDict  foo::Int64 end
struct ESLDict1 end
struct ESLDict2 end

model = ModelTypes() |>
    register_agenttype!(ADict) |>
    register_agenttype!(AVec, :Immortal) |>
    register_agenttype!(AVecFixed, :Immortal; size = 10) |>
    register_agenttype!(ASLDict) |>
    register_edgetype!(ESDict) |>
    register_edgetype!(ESLDict1) |> 
    register_edgetype!(ESLDict2, :SingleAgentType; to_agenttype = AVec) |> # to = AVec 
    construct_model("Test Core")

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
    
    idmap = finish_init!(sim)

    newids(ids) = map(ids) do id
        idmap[Vahana.remove_process(Vahana.remove_reuse(id))]
    end
    
    a1id, a2id, a3id = newids(a1id), newids(a2id), newids(a3id)
    avids, avfids = newids(avids), newids(avfids)

    
    @testset "agentstate" begin
        create_mpi_wins(sim)
        @onrankof a1id @test agentstate(sim, a1id, ADict) == ADict(1)
        @onrankof a1id @test agentstate_flexible(sim, a1id) == ADict(1)
        @onrankof a1id @test agentstate_flexible(sim, a1id) == ADict(1)
        @onrankof a2id @test agentstate(sim, a2id, ADict) == ADict(2)
        @onrankof avids[1] @test agentstate(sim, avids[1], AVec) == AVec(1)
        @onrankof avfids[1] @test agentstate(sim, avfids[1], AVecFixed) == AVecFixed(1)
        @onrankof avids[10] @test agentstate(sim, avids[10], AVec) == AVec(10)
        @onrankof avfids[10] @test agentstate(sim, avfids[10], AVecFixed) == AVecFixed(10)
    #     # calling agentstate with the wrong typ should throw an AssertionError
        @test_throws AssertionError agentstate(sim, a2id, AVec)
        @test_throws AssertionError agentstate(sim, avids[1], ADict)
        free_mpi_wins(sim)
    end

    @testset "edges_to" begin
        @onrankof a1id @test size(edges_to(sim, a1id, ESDict), 1) == 4
        @onrankof a1id @test size(edges_to(sim, a1id, ESLDict1), 1) == 10
        # # Check that we can call edges_to also for an empty set of neighbors
        @onrankof a2id @test edges_to(sim, a2id, ESDict) === nothing
        @onrankof a2id @test edges_to(sim, a2id, ESLDict1) === nothing
    end

    @testset "neighbors & edgestates" begin
        @onrankof avids[1] @test length(neighborids(sim, avids[1], ESLDict2)) == 1
        @onrankof avids[10] @test length(neighborids(sim, avids[10], ESLDict2)) == 1

        @test_throws AssertionError neighborids(sim, a2id, ESLDict2)

        edges = edges_to(sim, a1id, ESLDict1)
        @onrankof a1id @test (edges)[1].from == avids[1]
        @onrankof a1id @test (edges)[10].from == avids[10]
        edges = neighborids(sim, avids[10], ESLDict2)
        @onrankof avids[10] @test edges[1] == avfids[10]
    end

    @testset "neighborstates" begin
        create_mpi_wins(sim)
        @onrankof a1id @test neighborstates(sim, a1id, ESLDict1, AVec)[1] ==
            AVec(1)
        @onrankof a1id @test neighborstates_flexible(sim, a1id, ESDict)[2] ==
            ADict(3)
        free_mpi_wins(sim)
    end

    # working on a deepcopy should be possible and not change the
    # original state
    @testset "deepcopy" begin
        copy = deepcopy(sim)
        add_edge!(copy, a2id, a1id, ESDict(20))
        add_edge!(copy, a2id, avids[1], ESLDict2())
        @test_throws AssertionError add_edge!(copy, a2id, avfids[1], ESLDict2())
        @onrankof a1id @test size(edges_to(sim, a1id, ESDict), 1) == 4
        @onrankof a1id @test size(edges_to(copy, a1id, ESDict), 1) == 5
        @onrankof avids[1] @test size(neighborids(sim, avids[1], ESLDict2), 1) == 1
        @onrankof avids[1] @test size(neighborids(copy, avids[1], ESLDict2), 1) == 2
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
                          [ ADict ], allagenttypes, [])
        create_mpi_wins(copydict)
        @onrankof a1id @test agentstate(copydict, a1id, ADict) ==
            ADict(sum(1:10) + 1)
        free_mpi_wins(copydict)
        
        apply_transition!(copydict, create_sum_state_neighbors(ESLDict1),
                          [ ADict ], allagenttypes, [])
        create_mpi_wins(copydict)
        @onrankof a1id @test agentstate(copydict, a1id, ADict) ==
            ADict(2 * sum(1:10) + 1)
        free_mpi_wins(copydict)

        copyvec = deepcopy(copy)
        apply_transition!(copyvec, create_sum_state_neighbors(ESLDict2),
                          [ AVec ], allagenttypes, [])
        create_mpi_wins(copyvec)
        @onrankof avids[1] @test agentstate(copyvec, avids[1], AVec) == AVec(2)
        free_mpi_wins(copyvec)

        apply_transition!(copyvec, create_sum_state_neighbors(ESLDict2),
                          [ AVec ], allagenttypes, [])
        create_mpi_wins(copyvec)
        @onrankof avids[1] @test agentstate(copyvec, avids[1], AVec) == AVec(3)
        free_mpi_wins(copyvec)

        apply_transition!(copyvec, create_sum_state_neighbors(ESLDict2),
                          [ AVec ], allagenttypes, [])
        create_mpi_wins(copyvec)
        @onrankof avids[1] @test agentstate(copyvec, avids[1], AVec) == AVec(4)
        free_mpi_wins(copyvec)

        copyvecfix = deepcopy(copy)
        apply_transition!(copyvecfix, create_sum_state_neighbors(ESLDict1),
                          [ AVecFixed ], allagenttypes, [])
        create_mpi_wins(copyvecfix)
        @onrankof avfids[1] @test agentstate(copyvecfix, avfids[1], AVecFixed) ==
            AVecFixed(3)
        free_mpi_wins(copyvecfix)

        apply_transition!(copyvecfix, create_sum_state_neighbors(ESLDict1),
                          [ AVecFixed ], allagenttypes, [])
        create_mpi_wins(copyvecfix)
        @onrankof avfids[1] @test agentstate(copyvecfix, avfids[1], AVecFixed) ==
            AVecFixed(5)
        free_mpi_wins(copyvecfix)

        apply_transition!(copyvecfix, create_sum_state_neighbors(ESLDict1),
                          [ AVecFixed ], allagenttypes, [])
        create_mpi_wins(copyvecfix)
        @onrankof avfids[1] @test agentstate(copyvecfix, avfids[1], AVecFixed) ==
            AVecFixed(7)
        free_mpi_wins(copyvecfix)

        # TODO check return nothing (currently only supported by Dicts)
        # apply_transition!(copydict, nothing_transition, [ ADict ], [], [])
        # @test_throws KeyError agentstate_flexible(copydict, a1id)
    end

    @testset "num_neighbors" begin
        @onrankof a1id @test num_neighbors(sim, a1id, ESDict) == 4
        @onrankof a2id @test num_neighbors(sim, a2id, ESDict) == 0
        @test_throws AssertionError num_neighbors(sim, a2id, ESLDict2)
        @onrankof avids[1] @test num_neighbors(sim, avids[1], ESLDict2) == 1
    end

    # this hack should help that the output is not scrambled
    sleep(mpi.rank * 0.05)
    
    # TODO transition with add_agent! and add_edge!
end


# @testset "Aggregate" begin
#     sim = new_simulation(model, nothing, nothing; name = "Aggregate")

#     (a1id, a2id, a3id, avids, avfids) = add_example_network!(sim)
    
#     finish_init!(sim)
    
#     @test aggregate(sim, a -> a.foo, +, ADict) == 6
#     @test aggregate(sim, a -> a.foo, +, AVec) == sum(1:10)
#     @test aggregate(sim, e -> e.foo, +, ESDict) == 10
# end

