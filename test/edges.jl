# All types (for copy, paste and adjust for the individual tests)
#
# [ EdgeD, EdgeE, EdgeT, EdgeI, EdgeET, EdgeEI, EdgeTI, EdgeETI,
#   EdgeTs, EdgeETs, EdgeTsI, EdgeETsI,
#   EdgeS, EdgeSE, EdgeST, EdgeSI, EdgeSET, EdgeSEI, EdgeSTI, EdgeSETI,
#   EdgeSTs, EdgeSETs, EdgeSTsI, EdgeSETsI  ]


@testset "Edges" begin
    sim = construct(model, "Test", nothing, nothing)
    
    (a1id, a2id, a3id) = add_agents!(sim, AVec(1), AVec(2), AVec(3))

    # Lets add some edges for each of the different property combinations
    # For each combination we will have
    # 2 -> 3 (with state 1 for stateful edges)
    # 3 -> 1 (with state 3 for stateful edges)
    # and for all combination that supports multiple edges we add also
    # 1 -> 3 (with state 2 for stateful edges)

    for t in statelessEdgeTypes
        add_edge!(sim, a2id, a3id, t())
        # we can not check the "ET" combination, instead a warning
        # is given when add_edgetype is called
        if hasprop(nameof(t), "E") && !hasprop(nameof(t), "T") && !(hasprop(nameof(t), "S") && hasprop(nameof(t), "I"))
            # and check in the case that a second edge can not be added to
            # the same agent (and that this can be checked),
            # that this throws an assertion
            @test_throws AssertionError add_edge!(sim, a1id, a3id, t())
        elseif !hasprop(nameof(t), "E")
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
        # is given when add_edgetype is called
        if hasprop(nameof(t), "E") && !hasprop(nameof(t), "T") && !(hasprop(nameof(t), "S") && hasprop(nameof(t), "I"))
            @test_throws AssertionError add_edge!(sim, a1id, a3id, t(2))
        elseif !hasprop(nameof(t), "E")
            add_edge!(sim, a1id, a3id, t(2))
        end
        edge = Edge(a3id, t(3))
        add_edge!(sim, a1id, edge)

        # println(t)
        # @eval println($sim.$(Symbol(t,"_write")))
        # println()
    end

    finish_init!(sim)

    @testset "edges_to" begin
        for t in [EdgeD, EdgeT, EdgeTs]
            e = edges_to(sim, a3id, Val(t))
            @test e[1] == Edge(a2id, t(1))
            @test e[2] == Edge(a1id, t(2))
            e = edges_to(sim, a2id, Val(t))
            @test e == Vector()
        end
        for t in [EdgeE, EdgeET, EdgeETs]
            e = edges_to(sim, a3id, Val(t))
            @test e == Edge(a2id, t(1))
        end
        for t in  [ EdgeI, EdgeEI, EdgeTI, EdgeETI, EdgeTsI, EdgeETsI,
                 EdgeS, EdgeSE, EdgeST, EdgeSI, EdgeSET, EdgeSEI, EdgeSTI, EdgeSETI,
                 EdgeSTs, EdgeSETs, EdgeSTsI, EdgeSETsI  ]
            @test_throws AssertionError edges_to(sim, a1id, Val(t))
        end
    end

    @testset "neighborids" begin
        for t in [EdgeD, EdgeT, EdgeTs, EdgeS, EdgeST, EdgeSTs]
            e = neighborids(sim, a3id, Val(t))
            @test e[1] == a2id
            @test e[2] == a1id
            e = neighborids(sim, a2id, Val(t))
            @test e == Vector()
        end
        for t in [EdgeE, EdgeET, EdgeETs, EdgeSE, EdgeSET, EdgeSETs]
            e = neighborids(sim, a3id, Val(t))
            @test e == a2id
        end
        for t in [EdgeI, EdgeTI, EdgeTsI, EdgeSI, EdgeSTI, EdgeSTsI,
               EdgeEI, EdgeETI, EdgeETsI, EdgeSEI, EdgeSETI, EdgeSETsI]
            @test_throws AssertionError neighborids(sim, a1id, Val(t))
        end
    end

    @testset "edgestates" begin
        for t in [EdgeD, EdgeT, EdgeTs, EdgeI, EdgeTI, EdgeTsI]
            e = edgestates(sim, a3id, Val(t))
            @test e[1] == t(1)
            @test e[2] == t(2)
            e = edgestates(sim, a2id, Val(t))
            @test e == Vector()
        end
        for t in [EdgeE, EdgeET, EdgeETs, EdgeEI, EdgeETI, EdgeETsI]
            e = edgestates(sim, a3id, Val(t))
            @test e == t(1)
        end
        for t in [EdgeS, EdgeST, EdgeSTs, EdgeSI, EdgeSTI, EdgeSTsI,
               EdgeSE, EdgeSET, EdgeSETs, EdgeSEI, EdgeSETI, EdgeSETsI]
            @test_throws AssertionError edgestates(sim, a1id, Val(t))
        end
    end
    
    @testset "num_neighbors" begin
        for t in [EdgeD, EdgeT, EdgeTs, EdgeI, EdgeTI, EdgeTsI,
               EdgeS, EdgeST, EdgeSTs, EdgeST, EdgeSTI, EdgeSTsI]
            @test num_neighbors(sim, a1id, Val(t)) == 1
            @test num_neighbors(sim, a2id, Val(t)) == 0
            @test num_neighbors(sim, a3id, Val(t)) == 2
        end
        for t in [EdgeE, EdgeET, EdgeETs, EdgeEI, EdgeETI, EdgeETsI,
               EdgeSE, EdgeSET, EdgeSETs, EdgeSET, EdgeSETI, EdgeSETsI]
            @test_throws AssertionError num_neighbors(sim, a1id, Val(t))
        end
    end

    @testset "has_neighbor" begin
        for t in [ EdgeS, EdgeSE, EdgeST, EdgeSI, EdgeSEI, EdgeSTI, EdgeSETI,
                EdgeSTs, EdgeSTsI, EdgeSETsI, EdgeD, EdgeE, EdgeT, EdgeI,
                EdgeEI, EdgeTI, EdgeTs, EdgeTsI ]
            @test has_neighbor(sim, a1id, Val(t)) == true
            @test has_neighbor(sim, a2id, Val(t)) == false
            @test has_neighbor(sim, a3id, Val(t)) == true
        end
        for t in [ EdgeET, EdgeETs ]
            @test_throws AssertionError has_neighbor(sim, a1id, Val(t))
        end
    end


    @testset "has_neighbor" begin
        for t in [ EdgeS, EdgeSE, EdgeST, EdgeSI, EdgeSEI, EdgeSTI, EdgeSETI,
                EdgeSTs, EdgeSTsI, EdgeSETsI, EdgeD, EdgeE, EdgeT, EdgeI,
                EdgeEI, EdgeTI, EdgeTs, EdgeTsI ]
            @test has_neighbor(sim, a1id, Val(t)) == true
            @test has_neighbor(sim, a2id, Val(t)) == false
            @test has_neighbor(sim, a3id, Val(t)) == true
        end
        for t in [ EdgeET, EdgeETs ]
            @test_throws AssertionError has_neighbor(sim, a1id, Val(t))
        end
    end

    @testset "aggregate" begin
        for t in [ EdgeD, EdgeT, EdgeI, EdgeTI, EdgeTs, EdgeTsI ]
            @test aggregate(sim, Val(t), a -> a.foo, +) == 6
        end
        for t in [ EdgeE, EdgeET, EdgeEI, EdgeETI, EdgeETs, EdgeETsI ]
            @test aggregate(sim, Val(t), a -> a.foo, +) == 4
        end
        for t in  [ EdgeS, EdgeSE, EdgeST, EdgeSI, EdgeSET, EdgeSEI, EdgeSTI, EdgeSETI,
                 EdgeSTs, EdgeSETs, EdgeSTsI, EdgeSETsI  ]
            @test_throws AssertionError aggregate(sim, Val(t), a -> a.foo, +)
        end
    end
    # @testset "neighbor_states" begin
    #     for t in [EdgeD, EdgeT, EdgeTs, EdgeS, EdgeST, EdgeSTs]
    #         e = neighborids(sim, a3id, Val(t))
    #         @test e[1] == a2id
    #         @test e[2] == a1id
    #         e = neighborids(sim, a2id, Val(t))
    #         @test e == Vector()
    #     end
    # end
    
end
