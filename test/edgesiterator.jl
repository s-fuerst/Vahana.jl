# the edge types and some other stuff is defined in edges.jl

import Vahana: has_hint, @onrankof

# this test does not work with MPI, as it assumes that all edges are on one rank
if mpi.size == 1
    @testset "Edges Iter" begin
        function runedgesitertest(ET)
            eisim = create_simulation(model_edges; logging = true, debug = true)

            @test Vahana.edges_iterator(eisim, ET) |> length == 0
            @test Vahana.edges_iterator(eisim, ET) |> collect |> length == 0
            
            aids = add_agents!(eisim, [ Agent(i) for i in 1:10 ])
            for id in aids
                if fieldcount(ET) > 0
                    add_edge!(eisim, aids[1], id, ET(id))
                    if ! hashint(ET, "E")
                        add_edge!(eisim, id, aids[1], ET(id))
                    end
                else
                    add_edge!(eisim, aids[1], id, ET())
                    if ! hashint(ET, "E")
                        add_edge!(eisim, id, aids[1], ET())
                    end
                end
            end

            expected = hashint(ET, "E") ? 10 : 20
            
            @test Vahana.edges_iterator(eisim, ET, false) |> length == expected
            @test Vahana.edges_iterator(eisim, ET, false) |> collect |> length ==
                expected

            finish_init!(eisim)
            
            @test Vahana.edges_iterator(eisim, ET) |> length == expected
            @test Vahana.edges_iterator(eisim, ET) |> collect |> length ==
                expected

            finish_simulation!(eisim)
        end

        for ET in [ statelessEdgeTypes; statefulEdgeTypes ]
            if ! (hashint(ET, "S") & hashint(ET, "I"))
                runedgesitertest(ET)
                GC.gc(true)
            end
        end
    end
    # this hack should help that the output is not scrambled
    sleep(mpi.rank * 0.05)
end

@testset "Edges Agg" begin
    function runedgesaggregatetest(ET::DataType)
        sim = create_simulation(model_edges; logging = true, debug = true)

        @test mapreduce(sim, e -> e.foo, +, ET) == 0
        
        aids = add_agents!(sim, [ Agent(i) for i in 1:10 ])
        for id in aids
            add_edge!(sim, aids[1], id, ET(Vahana.agent_nr(id)))
        end

        finish_init!(sim)

        @test mapreduce(sim, e -> e.foo, +, ET) == sum(1:10)

        # first we test that edges to the agents are removed
        apply!(sim, [ Agent ], [], [ Agent ]) do _, id, sim
            nothing
        end

        @test mapreduce(sim, e -> e.foo, +, ET) == 0

        finish_simulation!(sim)
        # we now create edges from type AgentB to Agent and remove all
        # agents of type AgentB. This should also remove the edges.

        sim = create_simulation(model_edges; logging = true, debug = true)

        aids = add_agents!(sim, [ Agent(i) for i in 1:10 ])
        bids = add_agents!(sim, [ AgentB(i) for i in 1:10 ])
        for i in 1:10
            add_edge!(sim, bids[i], aids[i], ET(i))
        end

        finish_init!(sim)

        @test mapreduce(sim, e -> e.foo, +, ET) == sum(1:10)

        # first we test that edges to the agents are removed
        apply!(sim, [ Agent ], [], [ Agent ]) do _, id, sim
            nothing
        end

        @test mapreduce(sim, e -> e.foo, +, ET) == 0

        finish_simulation!(sim)
    end

    for ET in statefulEdgeTypes
        runedgesaggregatetest(ET)
        GC.gc(true)
    end
    # this hack should help that the output is not scrambled
    sleep(mpi.rank * 0.05)
end

@testset "Remove Edges" begin
    # some remove stuff is already tested above, but here we
    # use num_edges instead of mapreduce to test this for all types.
    # but this does not work for distributed runs
    function runremoveedgestest(ET::DataType)
        # we create edges from type AgentB to Agent and remove all
        # agents of type AgentB. This should also remove the edges.
        sim = create_simulation(model_edges; logging = true, debug = true)

        aids = add_agents!(sim, [ Agent(i) for i in 1:10 ])
        bids = add_agents!(sim, [ AgentB(i) for i in 1:10 ])
        for i in 1:10
            if has_hint(sim, ET, :Stateless)
                add_edge!(sim, bids[i], aids[i], ET())
            else
                add_edge!(sim, bids[i], aids[i], ET(i))
            end 
        end

        finish_init!(sim)

        @test num_edges(sim, ET) == 10

        # first we test that edges to the agents are removed
        apply!(sim, [ Agent ], [], [ Agent ]) do _, id, sim
            nothing
        end

        @test num_edges(sim, ET) == 0  

        if has_hint(sim, ET, :IgnoreFrom)
            return
        end

        finish_simulation!(sim)
        # we create edges from type AgentB to Agent and remove all
        # agents of type AgentB. This should also remove the edges.
        sim = create_simulation(model_edges, nothing, nothing)

        aids = add_agents!(sim, [ Agent(i) for i in 1:10 ])
        bids = add_agents!(sim, [ AgentB(i) for i in 1:10 ])
        for i in 1:10
            if has_hint(sim, ET, :Stateless)
                add_edge!(sim, bids[i], aids[i], ET())
            else
                add_edge!(sim, bids[i], aids[i], ET(i))
            end 
        end

        @onrankof aids[1] Vahana._remove_edges_agent_traget!(sim, aids[1], ET)
        @onrankof bids[2] Vahana._remove_edges_agent_source!(sim, [ bids[2] ], ET)

        finish_init!(sim)

        @test num_edges(sim, ET) == 8

        # # first we test that edges to the agents are removed
        # apply!(sim, [ AgentB ], [], []) do state, id, sim
        #     nothing
        # end

        # @test num_edges(sim, ET) == 0  
        finish_simulation!(sim)
    end

    for ET in [ statefulEdgeTypes; statelessEdgeTypes ]
        runremoveedgestest(ET)
        GC.gc(true)
    end
    
    # this hack should help that the output is not scrambled
    sleep(mpi.rank * 0.05)
end
