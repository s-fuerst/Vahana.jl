import Graphs

@testset "Graphs" begin
    # calculate the sum of all ids
    function sumids(a, id, sim)
        GraphA(a.id,
               mapreduce(a -> a.id, +, neighborstates_flexible(sim, id, GraphE)))
    end


    sim = construct(model, "Test Graphs", nothing, nothing)

    nagents = 4
    
    add_graph!(sim,
               Graphs.SimpleGraphs.complete_graph(nagents),
               i -> GraphA(i, 0),
               _ -> GraphE()
               )

    finish_init!(sim)

    apply_transition!(sim, sumids, [GraphA], [GraphE], [])

    # we have a complete graph, and all agents sum the
    # ids of the neighbors (but ignoring the own)
    # so in overall we have the nagents-1 times the sum of all ids
    @test aggregate(sim, a -> a.sum, +, GraphA) ==
        sum(1:nagents) * (nagents - 1)
end
