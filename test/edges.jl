@testset "Edges" begin
    sim = construct(model, "Test", nothing, nothing)
    
#    (a1id, a2id, a3id, avids, avfids) = add_example_network!(sim)

    (a1id, a2id) = add_agents!(sim, AVec(1), AVec(2))

    add_edge!(sim, a2id, a1id, DefaultEdge(1))

    # l= size(statelessEdgeTypes,1)
    # println(l)
    for t in statelessEdgeTypes
        println(t)
        add_edge!(sim, a1id, a2id, t())
    end

    for t in statefulEdgeTypes
        println(t)
        add_edge!(sim, a1id, a2id, t(1))
    end
    
end
