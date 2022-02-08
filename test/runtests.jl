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

function getAgent(sim, id::AgentID)
    sim.agents[type_nr(id)][id]
end

function numAgents(sim, type::DataType)
    length(sim.agents[agent_typeid(sim, type)])
end
 
@testset "Initialization" begin
    sim = Simulation("Example", ())

    p1 = Person(1)
    p2 = Person(2)

    h1 = HH(1)
    h2 = HH(2)

    add_agenttype!(sim, Person)
    @test numAgents(sim, Person) == 0
    @test sim.agent_typeids[Person] == 1

    add_agenttype!(sim, HH)
    @test sim.agent_typeids[HH] == 2

    p1id = add_agents!(sim, p1)
    @test numAgents(sim, Person) == 1
    @test getAgent(sim, p1id) == p1

    ids = add_agents!(sim, p1, p2)
    p2id = ids[2]
    @test numAgents(sim, Person) == 3
    @test getAgent(sim, p2id) == p2

    ids = add_agents!(sim, [[p1, p2], h1])
    @test numAgents(sim, Person) == 5
    @test numAgents(sim, HH) == 1
    @test getAgent(sim, ids[1][1]) == p1
    @test getAgent(sim, ids[2]) == h1

    h2id = add_agents!(sim, h2)

    add_edgetype!(sim, Edge{FooEdgeState})

    add_edge!(sim, p1id, p2id, FooEdgeState(0))
    edge = StatelessEdge(p1id, h2id)
    add_edge!(sim, edge)

    edgecoll = sim.edges[sim.edge_typeids[Edge{FooEdgeState}]]
    @test length(edgecoll[p2id]) == 1

    slet = sim.edge_typeids[StatelessEdge]
    @test length(get_edges(sim, slet, h2id)) == 1
    
    @test length(get_edges(sim, StatelessEdge, h2id)) == 1
 end



