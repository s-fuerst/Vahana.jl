import Vahana.@onrankof
import Vahana.@rootonly
import Vahana.disable_transition_checks

# A for Agent, Imm for Immortal
# Fixed doesn't have a real meaning anymore, in earlier versions
# there was a size keyword that allows to give an upper bound for the size
# of the population. 
struct AMortal           foo::Int64 end
struct AMortalFixed      foo::Int64 end
struct AImm              foo::Int64 end
struct AImmFixed         foo::Int64 end
struct AImmFixedOversize foo::Int64 end
struct ADefault
    foo::Int64
    bool::Bool
end

allagenttypes = [ AMortal, AImm, AImmFixed ]


# ES = EdgeState and ESL = EdgeStateless
struct ESDict  foo::Int64 end
struct ESLDict1 end
struct ESLDict2 end

begin
    model = ModelTypes() |>
        register_agenttype!(AMortal) |>
        register_agenttype!(AMortalFixed) |> 
        register_agenttype!(AImm, :Immortal) |>
        register_agenttype!(AImmFixed, :Immortal) |>
        register_agenttype!(AImmFixedOversize, :Immortal) |>
        register_agenttype!(ADefault) |>
        register_edgetype!(ESDict) |>
        register_edgetype!(ESLDict1) |> 
        register_edgetype!(ESLDict2, :SingleAgentType; to_agenttype = AImm) |>
        construct_model("Test Core")
end

function add_example_network!(sim)
    # construct 3 AMortal agents, 10 AImm agents and 10 AImmFixed
    a1id = add_agent!(sim, AMortal(1))
    a2id, a3id = add_agents!(sim, AMortal(2), AMortal(3))
    avids  = add_agents!(sim, [ AImm(i)      for i in 1:10 ])
    avfids = add_agents!(sim, [ AImmFixed(i) for i in 1:10 ])

    # agent with edges, used for aggregate
    add_agents!(sim, [ AImmFixedOversize(i) for i in 1:10 ])
    add_agents!(sim, [ AMortalFixed(i)      for i in 1:10 ])
    add_agents!(sim, [ ADefault(i, true)          for i in 1:10 ])

    # we construct the following network for ESDict:
    # a2 & a3 & avids[1] & avfids[10] -> a1
    add_edge!(sim, a2id, a1id, ESDict(1))
    add_edge!(sim, a3id, a1id, ESDict(2))
    add_edge!(sim, avids[1], a1id, ESDict(3))
    add_edge!(sim, avfids[10], a1id, ESDict(4))
    # we construct the following network for ESLDict1:
    # avids -> a1
    add_edges!(sim, a1id, [ Edge(avids[i], ESLDict1()) for i in 1:10 ])
    # we construct the following network for ESLDict2:
    # avfids[i] -> avids[i]
    for i in 1:10
        add_edge!(sim, avfids[i], avids[i], ESLDict2())
    end

    (a1id, a2id, a3id, avids, avfids)
end

function create_sum_state_neighbors(edgetypeval) 
    function sum_state_neighbors(agent, id, sim)
#        @info id edgetypeval getproperty(sim, Symbol(edgetypeval)).readable
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

import Vahana.updateids

function test_aggregate(sim, T::DataType)
    @test aggregate(sim, a -> a.foo, +, T) == reduce(+, 1:10)
    @test aggregate(sim, a -> a.foo, *, T) == reduce(*, 1:10)
    @test aggregate(sim, a -> a.foo, &, T; datatype = Int) == reduce(&, 1:10)
    @test aggregate(sim, a -> a.foo, |, T; datatype = Int) == reduce(|, 1:10)
    @test aggregate(sim, a -> a.foo, max, T) == reduce(max, 1:10)
    @test aggregate(sim, a -> a.foo, min, T) == reduce(min, 1:10)
end    

function test_aggregate_mortal(sim, T::DataType)
    test_aggregate(sim, T)
    
    # we are testing that there aggregate also works when on a rank
    # no agent of this type is existing
    apply_transition!(sim, [ T ], [ T ], [ T ]) do state, _, _
        mpi.isroot ? state : nothing
    end

    agg = aggregate(sim, a -> a.foo, +, T)
end    

function createsim()
    sim = new_simulation(model; logging = true, debug = true)

    (a1id, a2id, a3id, avids, avfids) = add_example_network!(sim)
    
    idmap = finish_init!(sim; return_idmapping = true)

    a1id, a2id, a3id =
        updateids(idmap, a1id), updateids(idmap, a2id), updateids(idmap, a3id)
    avids, avfids = updateids(idmap, avids), updateids(idmap, avfids)

    (sim, a1id, a2id, a3id, avids, avfids)
end
    

@testset "Core" begin
    (sim, a1id, a2id, a3id, avids, avfids) = createsim()
    
    @testset "agentstate" begin
        @onrankof a2id @test_throws AssertionError agentstate(sim, a2id, AImm)
        @onrankof avids[1] @test_throws AssertionError agentstate(sim, avids[1], AMortal)
        enable_asserts(false)
        @onrankof a1id @test agentstate(sim, a1id, AMortal) == AMortal(1)
        @onrankof a1id @test agentstate_flexible(sim, a1id) == AMortal(1)
        @onrankof a1id @test agentstate_flexible(sim, a1id) == AMortal(1)
        @onrankof a2id @test agentstate(sim, a2id, AMortal) == AMortal(2)
        @onrankof avids[1] @test agentstate(sim, avids[1], AImm) == AImm(1)
        @onrankof avfids[1] @test agentstate(sim, avfids[1], AImmFixed) == AImmFixed(1)
        @onrankof avids[10] @test agentstate(sim, avids[10], AImm) == AImm(10)
        @onrankof avfids[10] @test agentstate(sim, avfids[10], AImmFixed) == AImmFixed(10)
        #     # calling agentstate with the wrong typ should throw an AssertionError
        enable_asserts(true)
    end

    @testset "edges_to" begin
        Vahana.disable_transition_checks(true)
        @onrankof a1id @test length(edges_to(sim, a1id, ESDict)) == 4
        @onrankof a1id @test length(edges_to(sim, a1id, ESLDict1)) == 10
        # # Check that we can call edges_to also for an empty set of neighbors
        @onrankof a2id @test edges_to(sim, a2id, ESDict) === nothing
        @onrankof a2id @test edges_to(sim, a2id, ESLDict1) === nothing
        Vahana.disable_transition_checks(false)
    end

    @testset "neighbors & edgestates" begin
        Vahana.disable_transition_checks(true)
        # we must disable the neighborids checks
        @onrankof avids[1] @test length(neighborids(sim, avids[1], ESLDict2)) == 1
        @onrankof avids[10] @test length(neighborids(sim, avids[10], ESLDict2)) == 1


        edges = edges_to(sim, a1id, ESLDict1)
        @onrankof a1id @test (edges)[1].from == avids[1]
        @onrankof a1id @test (edges)[10].from == avids[10]
        edges = neighborids(sim, avids[10], ESLDict2)
        @onrankof avids[10] @test edges[1] == avfids[10]
        Vahana.disable_transition_checks(false)
    end

    @testset "neighborstates" begin
        Vahana.disable_transition_checks(true)
        @onrankof a1id @test AImm(1) in neighborstates(sim, a1id, ESLDict1, AImm)
        @onrankof a1id @test AMortal(3) in neighborstates_flexible(sim, a1id, ESDict)
        Vahana.disable_transition_checks(false)
    end

    @testset "num_neighbors" begin
        Vahana.disable_transition_checks(true)
        @onrankof a1id @test num_neighbors(sim, a1id, ESDict) == 4
        @onrankof a2id @test num_neighbors(sim, a2id, ESDict) == 0
        @onrankof avids[1] @test num_neighbors(sim, avids[1], ESLDict2) == 1
        Vahana.disable_transition_checks(false)
    end

    finish_simulation!(sim)
        
    @testset "transition" begin

        # normally it's not allowed to call add_edge! between transition
        # function, but because of the @onrankof this hack works here
        enable_asserts(false)
        # we want to check two iterations with the sum_state_neighbors,
        # so we just add an edge loop for the agents where we check the sum
        (sim, a1id, a2id, a3id, avids, avfids) = createsim()
        @onrankof a1id add_edge!(sim, a1id, a1id, ESLDict1())
        @onrankof avids[1] add_edge!(sim, avids[1], avids[1], ESLDict2())
        @onrankof avfids[1] add_edge!(sim, avfids[1], avfids[1], ESLDict1())
        # for avfids[1] we need a second edge
        @onrankof avfids[1] add_edge!(sim, avfids[2], avfids[1], ESLDict1())
        # and also avfids[2] should keep its value
        @onrankof avfids[2] add_edge!(sim, avfids[2], avfids[2], ESLDict1())
        enable_asserts(true)

        # now check apply_transition! for the different nodefieldfactories
        apply_transition!(sim, create_sum_state_neighbors(ESLDict1),
                          [ AMortal ], [ allagenttypes; ESLDict1 ],  [ AMortal ])
        disable_transition_checks(true)
        @onrankof a1id @test agentstate(sim, a1id, AMortal) ==
            AMortal(sum(1:10) + 1)
        disable_transition_checks(false)
        
        apply_transition!(sim, create_sum_state_neighbors(ESLDict1),
                          [ AMortal ], [ allagenttypes; ESLDict1 ], [ AMortal ])
        disable_transition_checks(true)
        @onrankof a1id @test agentstate(sim, a1id, AMortal) ==
            AMortal(2 * sum(1:10) + 1)
        disable_transition_checks(false)
        finish_simulation!(sim)

        # copyvec = deepcopy(copy)
        (sim, a1id, a2id, a3id, avids, avfids) = createsim()
        # @onrankof a1id add_edge!(sim, a1id, a1id, ESLDict1())
        enable_asserts(false)
        @onrankof avids[1] add_edge!(sim, avids[1], avids[1], ESLDict2())
        enable_asserts(true)

        apply_transition!(sim, create_sum_state_neighbors(ESLDict2),
                          [ AImm ], [ allagenttypes; ESLDict2 ], [ AImm ])
        disable_transition_checks(true)
        @onrankof avids[1] @test agentstate(sim, avids[1], AImm) == AImm(2)
        disable_transition_checks(false)

        apply_transition!(sim, create_sum_state_neighbors(ESLDict2),
                          [ AImm ], [ allagenttypes; ESLDict2 ], [ AImm ])
        disable_transition_checks(true)
        @onrankof avids[1] @test agentstate(sim, avids[1], AImm) == AImm(3)
        disable_transition_checks(false)

        apply_transition!(sim, create_sum_state_neighbors(ESLDict2),
                          [ AImm ], [ allagenttypes; ESLDict2 ], [ AImm ])
        disable_transition_checks(true)
        @onrankof avids[1] @test agentstate(sim, avids[1], AImm) == AImm(4)
        disable_transition_checks(false)

        finish_simulation!(sim)
        # copyvec = deepcopy(copy)
        (sim, a1id, a2id, a3id, avids, avfids) = createsim()
        enable_asserts(false)
        # @onrankof a1id add_edge!(sim, a1id, a1id, ESLDict1())
        # @onrankof avids[1] add_edge!(sim, avids[1], avids[1], ESLDict2())
        @onrankof avfids[1] add_edge!(sim, avfids[1], avfids[1], ESLDict1())
        # for avfids[1] we need a second edge
        @onrankof avfids[1] add_edge!(sim, avfids[2], avfids[1], ESLDict1())
        # and also avfids[2] should keep its value
        @onrankof avfids[2] add_edge!(sim, avfids[2], avfids[2], ESLDict1())
        enable_asserts(true)

        apply_transition!(sim, create_sum_state_neighbors(ESLDict1),
                          [ AImmFixed ], [ allagenttypes; ESLDict1 ], [ AImmFixed ])
        disable_transition_checks(true)
        @onrankof avfids[1] @test agentstate(sim, avfids[1], AImmFixed) ==
            AImmFixed(3)
        disable_transition_checks(false)

        apply_transition!(sim, create_sum_state_neighbors(ESLDict1),
                          [ AImmFixed ], [ allagenttypes; ESLDict1 ], [ AImmFixed ])
        disable_transition_checks(true)
        @onrankof avfids[1] @test agentstate(sim, avfids[1], AImmFixed) ==
            AImmFixed(5)
        disable_transition_checks(false)

        apply_transition!(sim, create_sum_state_neighbors(ESLDict1),
                          [ AImmFixed ], [ allagenttypes; ESLDict1 ], [ AImmFixed ])
        disable_transition_checks(true)
        @onrankof avfids[1] @test agentstate(sim, avfids[1], AImmFixed) ==
            AImmFixed(7)
        disable_transition_checks(false)
    end


    finish_simulation!(sim)
    
    # TODO transition with add_agent! and add_edge!
    @testset "Aggregate" begin
        sim = new_simulation(model; name = "Aggregate")

        (a1id, a2id, a3id, avids, avfids) = add_example_network!(sim)

        finish_init!(sim)

        for T in [ AImmFixed, AImmFixedOversize ]
            test_aggregate(sim, T)
        end

        for T in [ AMortalFixed, ADefault ]
            test_aggregate_mortal(sim, T)
        end

        finish_simulation!(sim)

        sim = new_simulation(model; name = "Aggregate")

        (a1id, a2id, a3id, avids, avfids) = add_example_network!(sim)

        finish_init!(sim)
        
        #  testing the & and | for boolean 
        # currenty all bool of ADefault are true
        @test aggregate(sim, a -> a.bool, &, ADefault) == true
        @test aggregate(sim, a -> a.bool, |, ADefault) == true

        # set all bool to false
        apply_transition!(sim, [ ADefault ], [ ADefault ], [ ADefault ]) do state, id, sim
            ADefault(state.foo, false)
        end

        @test aggregate(sim, a -> a.bool, &, ADefault) == false
        @test aggregate(sim, a -> a.bool, |, ADefault) == false

        # every second will be true, so that && is false and || is true
        apply_transition!(sim, [ ADefault ], [ ADefault ], [ ADefault ]) do state, id, sim
            ADefault(state.foo, mod(id, 2) == 1)
        end
        @test aggregate(sim, a -> a.bool, &, ADefault) == false
        @test aggregate(sim, a -> a.bool, |, ADefault) == true

        finish_simulation!(sim)
    end
end

# this hack should help that the output is not scrambled
sleep(mpi.rank * 0.05)

