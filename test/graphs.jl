import Graphs

@testset "Graphs" begin
    struct GraphA <: Agent
        id::Int64
        sum::Int64
    end

    struct GraphE <: EdgeState
    end

    # calculate the sum of all ids
    function sumids(a, id, sim)
        GraphA(a.id,
               mapreduce(a -> a.id, +, neighborstates(sim, id, GraphE)))
    end


    sim = Simulation("Test Graphs", nothing, nothing)

    add_agenttype!(sim, GraphA)
    add_edgetype!(sim, GraphE)

    nagents = 4
    
    add_graph!(sim,
               Graphs.SimpleGraphs.complete_graph(nagents),
               i -> GraphA(i, 0),
               _ -> GraphE()
               )

    finish_init!(sim)

    apply_transition!(sim, sumids, [GraphA], [GraphE], [])

    @test aggregate(sim, GraphA, a -> a.sum, +) == sum(1:nagents) * (nagents - 1)
end
