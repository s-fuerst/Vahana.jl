import Graphs.SimpleGraphs

struct GraphA 
    id::Int64
    sum::Int64
end

struct GraphE end


model_graph = ModelTypes() |>
    register_agenttype!(GraphA) |>
    register_edgetype!(GraphE) |>
    create_model("Test Graph")

@testset "Graphs" begin
    # calculate the sum of all ids
    function sumids(a, id, sim)
        GraphA(a.id,
               mapreduce(a -> a.id, +,
                         neighborstates_flexible(sim, id, GraphE)))
    end


    sim = create_simulation(model_graph, nothing, nothing)

    nagents = 4
    
    add_graph!(sim,
               SimpleGraphs.complete_graph(nagents),
               i -> GraphA(i, 0),
               _ -> GraphE()
               )

    finish_init!(sim)

    apply!(sim, sumids, [GraphA], [GraphA, GraphE], [GraphA])

    # we have a complete graph, and all agents sum the
    # ids of the neighbors (but ignoring the own)
    # so in overall we have the nagents-1 times the sum of all ids
    @test mapreduce(sim, a -> a.sum, +, GraphA) ==
        sum(1:nagents) * (nagents - 1)

    finish_simulation!(sim)
end
