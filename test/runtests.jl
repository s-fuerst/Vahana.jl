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

function getAgents(sim, T::DataType)
    typenr = sim.agent_type_ids.type2number[T]
    sim.agents[typenr, sim.write[typenr]]
end

@testset "Initialization" begin
    sim = Simulation("Example", ())

    p1 = Person(1)
    p2 = Person(2)

    h1 = HH(1)
    h2 = HH(2)

    add_agenttype!(sim, Person)
    @test length(getAgents(sim, Person)) == 0
    @test sim.agent_type_ids.type2number[Person] == 1
    @test sim.agent_type_ids.number2type[1] == Person

    add_agenttype!(sim, HH)
    @test sim.agent_type_ids.type2number[HH] == 2
    @test sim.agent_type_ids.number2type[2] == HH

    p1id = add_agents!(sim, p1)
    dpersons = getAgents(sim, Person)
    @test length(dpersons) == 1
    @test dpersons[p1id] == p1

    ids = add_agents!(sim, p1, p2)
    p2id = ids[2]
    @test length(dpersons) == 3
    @test dpersons[p2id] == p2

    ids = add_agents!(sim, [[p1, p2], h1])
    # @test length(sim.agents[sim.next][Person]) == 5
    # @test length(sim.agents[sim.next][HH]) == 1
    # @test sim.agents[1][Person][ids[1][1]] == p1
    # @test sim.agents[sim.next][HH][ids[2]] == h1

    # h2id = add_agents!(sim, h2)

    # add_edgetype!(sim, FooEdgeState)

    # add_edge!(sim, p1id, p2id, FooEdgeState(0))
    # add_edge!(sim, p1id, h2id, FooEdgeState(1))
    # add_edge!(sim, Edge(h2id, p2id, FooEdgeState(2)))

    # @test sim.edges[sim.next][FooEdgeState] |> length == 2
    # @test sim.edges[sim.next][FooEdgeState][h2id] |> length == 1 
    # @test sim.edges[sim.next][FooEdgeState][p2id] |> length == 2
    #    add_agents!(sim, createPersons)

    # finish_init!(sim)
    # @test length(sim.agents[sim.next][Person]) == 0

    # @test sim.edges_t[FooEdgeState] |> length == 2
    # @test sim.edges_tp1[FooEdgeState] |> length == 0
end



