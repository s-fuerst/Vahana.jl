using Vahana
using Test

struct Person <: Agent
    foo::Int64
end

struct HH <: Agent
    bar::Int64
end

 
struct FooEdgeState 
    foo::Int64
end

function createPersons(sim)
    [ Person(11), Person(12) ]
end


@testset "Initialization" begin
    sim = Simulation("Example", ())

    p1 = Person(1)
    p2 = Person(2)

    h1 = HH(1)
    h2 = HH(2)

    add_agenttype!(sim, Person)
    @test length(sim.agent_data_tp1) == 1
    @test sim.type2number[Person] == 1
    @test sim.number2type[1] == Person

    add_agenttype!(sim, HH)
    @test length(sim.agent_data_tp1) == 2
    @test sim.type2number[HH] == 2
    @test sim.number2type[2] == HH

    p1id = add_agents!(sim, p1)
    @test length(sim.agent_data_tp1[Person]) == 1
    @test sim.agent_data_tp1[Person][p1id] == p1

    ids = add_agents!(sim, p1, p2)
    p2id = ids[2]
    @test length(sim.agent_data_tp1[Person]) == 3
    @test sim.agent_data_tp1[Person][p2id] == p2

    ids = add_agents!(sim, [[p1, p2], h1])
    @test length(sim.agent_data_tp1[Person]) == 5
    @test length(sim.agent_data_tp1[HH]) == 1
    @test sim.agent_data_tp1[Person][ids[1][1]] == p1
    @test sim.agent_data_tp1[HH][ids[2]] == h1

    h2id = add_agents!(sim, h2)

#    add_edgetype!(sim, StatelessEdge)
    add_edgetype!(sim, FooEdgeState)

    add_edge!(sim, p1id, p2id, FooEdgeState(0))
    add_edge!(sim, p1id, h2id, FooEdgeState(1))
    add_edge!(sim, Edge(h2id, p2id, FooEdgeState(2)))

    @test sim.edge_data_tp1[FooEdgeState] |> length == 2
    @test sim.edge_data_tp1[FooEdgeState][h2id] |> length == 1 
    @test sim.edge_data_tp1[FooEdgeState][p2id] |> length == 2
    #    add_agents!(sim, createPersons)

    finish_init!(sim)
    @test length(sim.agent_data_t[Person]) == 5
    # @test length(sim.agent_data_tp1[Person]) == 0

    # @test sim.edge_data_t[FooEdgeState] |> length == 2
    # @test sim.edge_data_tp1[FooEdgeState] |> length == 0
end



