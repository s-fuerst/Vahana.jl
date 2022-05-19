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
        #println(t)
        add_edge!(sim, a2id, a3id, t())
        if hasprop(t, "E") && !hasprop(t, "T") && !(hasprop(t, "S") && hasprop(t, "I"))
            # and check in the case that a second edge can not be added to
            # the same agent (and that this can be checked),
            # that this throws an assertion
            @test_throws AssertionError add_edge!(sim, a1id, a3id, t())
        else
            add_edge!(sim, a1id, a3id, t())
        end
        edge = Edge(a3id, t())
        add_edge!(sim, a1id, edge)

        # @eval println($sim.$(Symbol(t,"_write")))
        # println()
    end

    for t in statefulEdgeTypes
        #println(t)
        add_edge!(sim, a2id, a3id, t(1))
        if hasprop(t, "E") && !hasprop(t, "T") && !(hasprop(t, "S") && hasprop(t, "I"))
            @test_throws AssertionError add_edge!(sim, a1id, a3id, t(2))
        else
            add_edge!(sim, a1id, a3id, t(2))
        end
        edge = Edge(a3id, t(3))
        add_edge!(sim, a1id, edge)

        # @eval println($sim.$(Symbol(t,"_write")))
        # println()
    end
    
    
end
