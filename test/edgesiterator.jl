# the edge types and some other stuff is defined in edges.jl

import Vahana: has_trait

@testset "Edges Iter" begin
    function runedgesitertest(ET)
        eisim = new_simulation(model_edges, nothing, nothing)

        @test Vahana.edges_iterator(eisim, ET) |> length == 0
        @test Vahana.edges_iterator(eisim, ET) |> collect |> length == 0
        
        aids = add_agents!(eisim, [ Agent(i) for i in 1:10 ])
        for id in aids
            if fieldcount(ET) > 0
                add_edge!(eisim, aids[1], id, ET(id))
                if ! hastrait(ET, "E")
                    add_edge!(eisim, id, aids[1], ET(id))
                end
            else
                add_edge!(eisim, aids[1], id, ET())
                if ! hastrait(ET, "E")
                    add_edge!(eisim, id, aids[1], ET())
                end
            end
        end

        expected = hastrait(ET, "E") ? 10 : 20
        
        @test Vahana.edges_iterator(eisim, ET, false) |> length == expected
        @test Vahana.edges_iterator(eisim, ET, false) |> collect |> length == expected

        finish_init!(eisim)
        
        @test Vahana.edges_iterator(eisim, ET) |> length == expected
        @test Vahana.edges_iterator(eisim, ET) |> collect |> length == expected
    end

    for ET in [ statelessEdgeTypes; statefulEdgeTypes ]
        if ! (hastrait(ET, "S") & hastrait(ET, "I"))
            runedgesitertest(ET)
        end
    end
end

@testset "Edges Agg" begin
    function runedgesaggregatetest(ET::DataType)
        sim = new_simulation(model_edges, nothing, nothing)

        @test aggregate(sim, e -> e.foo, +, ET) == 0
        
        aids = add_agents!(sim, [ Agent(i) for i in 1:10 ])
        for id in aids
            add_edge!(sim, aids[1], id, ET(Vahana.agent_nr(id)))
        end

        finish_init!(sim)

        @test aggregate(sim, e -> e.foo, +, ET) == sum(1:10)

        # first we test that edges to the agents are removed
        apply_transition!(sim, [ Agent ], [], []) do state, id, sim
            nothing
        end

        @test aggregate(sim, e -> e.foo, +, ET) == 0

        # we now create edges from type AgentB to Agent and remove all
        # agents of type AgentB. This should also remove the edges.
        sim = new_simulation(model_edges, nothing, nothing)

        aids = add_agents!(sim, [ Agent(i) for i in 1:10 ])
        bids = add_agents!(sim, [ AgentB(i) for i in 1:10 ])
        for i in 1:10
            add_edge!(sim, bids[i], aids[i], ET(i))
        end

        finish_init!(sim)

        @test aggregate(sim, e -> e.foo, +, ET) == sum(1:10)

        # first we test that edges to the agents are removed
        apply_transition!(sim, [ Agent ], [], []) do state, id, sim
            nothing
        end

        @test aggregate(sim, e -> e.foo, +, ET) == 0
        
    end

    for ET in statefulEdgeTypes
        runedgesaggregatetest(ET)
    end
end

@testset "Remove Edges" begin
    # some remove stuff is already tested above, but here we
    # use num_edges instead of aggregate to test this for all types.
    # but this does not work for distributed runs
    function runremoveedgestest(ET::DataType)
        sim = new_simulation(model_edges, nothing, nothing)

        @info "run remove test for" ET

        # we create edges from type AgentB to Agent and remove all
        # agents of type AgentB. This should also remove the edges.
        sim = new_simulation(model_edges, nothing, nothing)

        aids = add_agents!(sim, [ Agent(i) for i in 1:10 ])
        bids = add_agents!(sim, [ AgentB(i) for i in 1:10 ])
        for i in 1:10
            if has_trait(sim, ET, :Stateless)
                add_edge!(sim, bids[i], aids[i], ET())
            else
                add_edge!(sim, bids[i], aids[i], ET(i))
            end 
        end

        finish_init!(sim)

        @test num_edges(sim, ET) == 10  

        # first we test that edges to the agents are removed
        apply_transition!(sim, [ Agent ], [], []) do state, id, sim
            nothing
        end

        @test num_edges(sim, ET) == 0  

        if has_trait(sim, ET, :IgnoreFrom)
            return
        end
        # we create edges from type AgentB to Agent and remove all
        # agents of type AgentB. This should also remove the edges.
        sim = new_simulation(model_edges, nothing, nothing)

        aids = add_agents!(sim, [ Agent(i) for i in 1:10 ])
        bids = add_agents!(sim, [ AgentB(i) for i in 1:10 ])
        for i in 1:10
            if has_trait(sim, ET, :Stateless)
                add_edge!(sim, bids[i], aids[i], ET())
            else
                add_edge!(sim, bids[i], aids[i], ET(i))
            end 
        end

        Vahana._remove_edge_from!(sim, bids[1], ET)

        finish_init!(sim)

        @test num_edges(sim, ET) == 9

        # # first we test that edges to the agents are removed
        # apply_transition!(sim, [ AgentB ], [], []) do state, id, sim
        #     nothing
        # end

        # @test num_edges(sim, ET) == 0  

    end

    for ET in [ statefulEdgeTypes; statelessEdgeTypes ]
            runremoveedgestest(ET)
    end
end
