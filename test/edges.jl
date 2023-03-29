import Vahana: disable_transition_checks

import Graphs: SimpleGraphs

# We need a lot of edgetypes to test all the edge trait combinations
# The traits are
# (S) Stateless
# (E) SingleEdge
# (T) SingleAgentType, which can also have a size information (Ts)
# (I) IgnoreFrom
# so EdgeET means an Edge(State) with the SingleEdge and SingleAgentType traits

struct Agent foo::Int64 end
struct AgentB foo::Int64 end # this is used to test the removal of "dead" edges (in edgesiterator)
struct EdgeD foo::Int64 end # D for default (no trait is set)
struct EdgeS end
struct EdgeE foo::Int64 end
struct EdgeT foo::Int64 end
struct EdgeI foo::Int64 end
struct EdgeSE end
struct EdgeST end
struct EdgeSI end
struct EdgeEI foo::Int64 end
struct EdgeTI foo::Int64 end
struct EdgeSEI end
struct EdgeSTI end
struct EdgeSETI end

struct EdgeTs foo::Int64 end
struct EdgeTsI foo::Int64 end

struct EdgeSTs end
struct EdgeSTsI end
struct EdgeSETsI end

statelessEdgeTypes = [ EdgeS, EdgeSE, EdgeST, EdgeSI, EdgeSEI, EdgeSTI, EdgeSETI, EdgeSTs, EdgeSTsI, EdgeSETsI  ]

statefulEdgeTypes = [ EdgeD, EdgeE, EdgeT, EdgeI, EdgeEI, EdgeTI, EdgeTs, EdgeTsI ]

allEdgeTypes = [ statelessEdgeTypes; statefulEdgeTypes ]

model_edges = ModelTypes() |>
    register_agenttype!(Agent) |>
    register_agenttype!(AgentB) |>
    register_edgetype!(EdgeD) |>
    register_edgetype!(EdgeS, :Stateless) |>
    register_edgetype!(EdgeE, :SingleEdge) |>
    register_edgetype!(EdgeT, :SingleAgentType; to_agenttype = Agent) |>
    register_edgetype!(EdgeI, :IgnoreFrom) |>
    register_edgetype!(EdgeSE, :Stateless, :SingleEdge) |>
    register_edgetype!(EdgeST, :Stateless, :SingleAgentType; to_agenttype = Agent) |>
    register_edgetype!(EdgeSI, :Stateless, :IgnoreFrom) |>
    register_edgetype!(EdgeEI, :SingleEdge, :IgnoreFrom) |>
    register_edgetype!(EdgeTI, :SingleAgentType, :IgnoreFrom; to_agenttype = Agent) |>
    register_edgetype!(EdgeSEI, :Stateless, :SingleEdge, :IgnoreFrom) |>
    register_edgetype!(EdgeSTI, :Stateless, :SingleAgentType, :IgnoreFrom; to_agenttype = Agent) |>
    register_edgetype!(EdgeSETI, :Stateless, :SingleEdge, :SingleAgentType, :IgnoreFrom; to_agenttype = Agent) |>
    register_edgetype!(EdgeTs, :SingleAgentType; to_agenttype = Agent, size = 10) |>
    register_edgetype!(EdgeTsI, :SingleAgentType, :IgnoreFrom; to_agenttype = Agent, size = 10) |>
    register_edgetype!(EdgeSTs, :Stateless, :SingleAgentType; to_agenttype = Agent, size = 10) |>
    register_edgetype!(EdgeSTsI, :Stateless, :SingleAgentType, :IgnoreFrom; to_agenttype = Agent, size = 10) |>
    register_edgetype!(EdgeSETsI, :Stateless, :SingleEdge, :SingleAgentType, :IgnoreFrom; to_agenttype = Agent, size = 10) |>
    construct_model("Test Edges")


hastrait(type, trait::String) = occursin(trait, SubString(String(Symbol(type)), 5))


function runedgestest()
    @testset "Edges" begin
        sim = new_simulation(model_edges, nothing, nothing)
        
        (a1id, a2id, a3id) = add_agents!(sim, Agent(1), Agent(2), Agent(3))


        # Lets add some edges for each of the different trait combinations
        # For each combination we will have
        # 2 -> 3 (with state 1 for stateful edges)
        # 3 -> 1 (with state 3 for stateful edges)
        # and for all combination that supports multiple edges we add also
        # 1 -> 3 (with state 2 for stateful edges)

        for t in statelessEdgeTypes
            add_edge!(sim, a2id, a3id, t())
            # we can not check the "ET" combination, instead a warning
            # is given when register_edgetype is called
            if hastrait(nameof(t), "E") && !hastrait(nameof(t), "T") && !(hastrait(nameof(t), "S") && hastrait(nameof(t), "I"))
                # and check in the case that a second edge can not be added to
                # the same agent (and that this can be checked),
                # that this throws an assertion
                @test_throws AssertionError add_edge!(sim, a1id, a3id, t())
            elseif !hastrait(nameof(t), "E")
                add_edge!(sim, a1id, a3id, t())
            end
            edge = Edge(a3id, t())
            add_edge!(sim, a1id, edge)

            # println(t)
            # @eval println($sim.$(Symbol(nameof(t),"_write")))
            # println()
        end

        for t in statefulEdgeTypes
            add_edge!(sim, a2id, a3id, t(1))
            # we can not check the "ET" combination, instead a warning
            # is given when register_edgetype is called
            if hastrait(nameof(t), "E") && !hastrait(nameof(t), "T") && !(hastrait(nameof(t), "S") && hastrait(nameof(t), "I"))
                @test_throws AssertionError add_edge!(sim, a1id, a3id, t(2))
            elseif !hastrait(nameof(t), "E")
                add_edge!(sim, a1id, a3id, t(2))
            end
            edge = Edge(a3id, t(3))
            add_edge!(sim, a1id, edge)

            # println(t)
            # @eval println($sim.$(Symbol(t,"_write")))
            # println()
        end

        finish_init!(sim)
        # # with this apply_transition we ensure
        # apply_transition(sim, [ Agent, AgentB ], allEdgeTypes, []) do _,_,_ end

        @testset "edges_to" begin
            disable_transition_checks(true)
            for t in [EdgeD, EdgeT, EdgeTs]
                e = edges_to(sim, a3id, t)
                @test e[1] == Edge(a2id, t(1))
                @test e[2] == Edge(a1id, t(2))
                e = edges_to(sim, a2id, t)
                @test e === nothing
            end
            for t in [EdgeE]
                e = edges_to(sim, a3id, t)
                @test e == Edge(a2id, t(1))
            end
            disable_transition_checks(false)
                    
            for t in  [ EdgeI, EdgeEI, EdgeTI, EdgeTsI, EdgeS, EdgeSE, EdgeST, EdgeSI, EdgeSEI, EdgeSTI, EdgeSETI, EdgeSTs, EdgeSTsI, EdgeSETsI  ]
                @test_throws AssertionError edges_to(sim, a1id, t)
            end
        end

        @testset "neighborids" begin
            disable_transition_checks(true)
            for t in [EdgeD, EdgeT, EdgeTs, EdgeS, EdgeST, EdgeSTs]
                e = neighborids(sim, a3id, t)
                @test e[1] == a2id
                @test e[2] == a1id
                e = neighborids(sim, a2id, t)
                @test e === nothing
            end
            for t in [EdgeE, EdgeSE]
                e = neighborids(sim, a3id, t)
                @test e == a2id
            end
            disable_transition_checks(false)
            for t in [EdgeI, EdgeTI, EdgeTsI, EdgeSI, EdgeSTI, EdgeSTsI,
                   EdgeEI, EdgeSEI, EdgeSETI, EdgeSETsI]
                @test_throws AssertionError neighborids(sim, a1id, t)
            end
        end

        @testset "edgestates" begin
            disable_transition_checks(true)
            for t in [EdgeD, EdgeT, EdgeTs, EdgeI, EdgeTI, EdgeTsI]
                e = edgestates(sim, a3id, t)
                @test e[1] == t(1)
                @test e[2] == t(2)
                e = edgestates(sim, a2id, t)
                @test e === nothing
            end
            for t in [EdgeE, EdgeEI]
                e = edgestates(sim, a3id, t)
                @test e == t(1)
            end
            disable_transition_checks(false)
            for t in [EdgeS, EdgeST, EdgeSTs, EdgeSI, EdgeSTI, EdgeSTsI,
                   EdgeSE, EdgeSEI, EdgeSETI, EdgeSETsI]
                @test_throws AssertionError edgestates(sim, a1id, t)
            end
        end
        
        @testset "num_neighbors" begin
            disable_transition_checks(true)
            for t in [EdgeD, EdgeT, EdgeTs, EdgeI, EdgeTI, EdgeTsI,
                   EdgeS, EdgeST, EdgeSTs, EdgeST, EdgeSTI, EdgeSTsI]
                @test num_neighbors(sim, a1id, t) == 1
                @test num_neighbors(sim, a2id, t) == 0
                @test num_neighbors(sim, a3id, t) == 2
            end
            disable_transition_checks(false)
            for t in [EdgeE, EdgeEI, EdgeSE, EdgeSETI, EdgeSETsI]
                @test_throws AssertionError num_neighbors(sim, a1id, t)
            end
        end

        @testset "has_neighbor" begin
            disable_transition_checks(true)
            for t in [ EdgeS, EdgeSE, EdgeST, EdgeSI, EdgeSEI, EdgeSTI, EdgeSETI,
                    EdgeSTs, EdgeSTsI, EdgeSETsI, EdgeD, EdgeE, EdgeT, EdgeI,
                    EdgeEI, EdgeTI, EdgeTs, EdgeTsI ]
                @test has_neighbor(sim, a1id, t) == true
                @test has_neighbor(sim, a2id, t) == false
                @test has_neighbor(sim, a3id, t) == true
            end
            disable_transition_checks(false)
        end
        
        @testset "aggregate" begin
            for t in [ EdgeD, EdgeT, EdgeI, EdgeTI, EdgeTs, EdgeTsI ]
                @test aggregate(sim, a -> a.foo, +, t) == 6
            end
            for t in [ EdgeE, EdgeEI ]
                @test aggregate(sim, a -> a.foo, +, t) == 4
            end
            for t in  [ EdgeS, EdgeSE, EdgeST, EdgeSI, EdgeSEI, EdgeSTI, EdgeSETI,
                     EdgeSTs, EdgeSTsI, EdgeSETsI  ]
                @test_throws AssertionError aggregate(sim, a -> a.foo, +, t)
            end
        end

        @testset "neighbor_states" begin
            disable_transition_checks(true)
            for t in [EdgeD, EdgeT, EdgeTs, EdgeS, EdgeST, EdgeSTs]
                e = neighborids(sim, a3id, t)
                @test e[1] == a2id
                @test e[2] == a1id
                e = neighborids(sim, a2id, t)
                @test e === nothing
            end
            disable_transition_checks(false)
        end

        @testset "check Vahana state" begin
            for t in statelessEdgeTypes
                @test_throws AssertionError add_edge!(sim, AgentID(0), AgentID(0), t())
            end
            for t in statefulEdgeTypes
                @test_throws AssertionError add_edge!(sim, AgentID(0), AgentID(0), t(0))
            end
        end

        finish_simulation!(sim)
    end

    @testset "num_edges" begin
        sim = new_simulation(model_edges, nothing, nothing)

        for t in [ statefulEdgeTypes; statelessEdgeTypes ]
            @test num_edges(sim, t; write = false) == 0
        end

        @test num_agents(sim, Agent) == 0

        # We need a gap
        id1 = add_agent!(sim, Agent(0))
        id2 = add_agent!(sim, Agent(0))
        id3 = add_agent!(sim, Agent(0))

        @test num_agents(sim, Agent) == 3
        
        for t in [ statefulEdgeTypes; statelessEdgeTypes ]
            if fieldcount(t) > 0
                add_edge!(sim, id1, id1, t(0))
                add_edge!(sim, id3, id3, t(0))
            else
                add_edge!(sim, id1, id1, t())
                add_edge!(sim, id3, id3, t())
            end
        end

        for t in [ statefulEdgeTypes; statelessEdgeTypes ]
            @test num_edges(sim, t; write = true) == 2
            @test num_edges(sim, t; write = false) == 0
        end
        
        finish_init!(sim)

        for t in [ statefulEdgeTypes; statelessEdgeTypes ]
            @test num_edges(sim, t; write = false) == 2
        end

        finish_simulation!(sim)
    end
end



@testset "transition" begin
    for ET in [ EdgeD, EdgeE, EdgeT, EdgeI, EdgeEI, EdgeTI ]
        sim = new_simulation(model_edges)

        if Vahana.has_trait(sim, ET, :SingleEdge)
            finish_simulation!(sim)
            continue
        end

        
        nagents = mpi.size * 2
        
        add_graph!(sim,
                   SimpleGraphs.complete_graph(nagents),
                   i -> Agent(i),
                   e -> ET(e.dst)
                   )

        finish_init!(sim)

        apply_transition!(sim, [ Agent ], [ ET ], []) do _, id, sim
            @test num_neighbors(sim, id, ET) == nagents-1
        end

        # write was empty, so we should get the same results when doing
        # the same transition again
        apply_transition!(sim, [ Agent ], [ ET ], []) do _, id, sim
            @test num_neighbors(sim, id, ET) == nagents-1
        end

        # write the edges by copying them from before
        apply_transition!(sim, [ Agent ], [], [ ET ]; add_existing = [ ET ]) do _, id, sim
        end
        apply_transition!(sim, [ Agent ], [ ET ], []) do _, id, sim
            @test num_neighbors(sim, id, ET) == nagents-1
        end

        if ET == EdgeD
            # write the edges by readding them 
            apply_transition!(sim, [ Agent ], [ ET ], [ ET ]) do _, id, sim
                add_edges!(sim, id, edges_to(sim, id, ET))
            end
            apply_transition!(sim, [ Agent ], [ ET ], []) do _, id, sim
                @test num_neighbors(sim, id, ET) == nagents-1
            end

            apply_transition!(sim, [ Agent ], [], []) do _, id, sim end

            # write the edges by copying and readding 
            apply_transition!(sim, [ Agent ], [ ET ], [ ET ]; add_existing = [ ET ]) do _, id, sim
                add_edges!(sim, id, edges_to(sim, id, ET))
            end

            # (we should have them twice after now)
            apply_transition!(sim, [ Agent ], [ ET ], []) do _, id, sim
                @test num_neighbors(sim, id, ET) == (nagents-1) * 2
            end

            # this time no one adds the edges, so they should be gone afterwards
            apply_transition!(sim, [ Agent ], [], [ ET ]) do _, id, sim
            end
            apply_transition!(sim, [ Agent ], [ ET ], []) do _, id, sim
                @test num_neighbors(sim, id, ET) == 0
            end
        end

        finish_simulation!(sim)
    end

    # this hack should help that the output is not scrambled
    sleep(mpi.rank * 0.05)
end
