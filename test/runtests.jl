using Vahana
using Test

struct Person <: AbstractAgent
    foo::Int64
end

struct HH <: AbstractAgent
    bar::Int64
end

 
struct FooEdgeState 
    foo::Int64
end

struct StatelessEdgeType end

function createPersons(sim)
    [ Person(11), Person(12) ]
end

function get_write_agent(sim, id::AgentID)
    sim.agents[type_nr(id)].containers[sim.agents[type_nr(id)].write][id]
end

function get_write_edges(sim, T::DataType, id::AgentID)
    sim.edges[T].containers[sim.edges[T].write][id]
end

function numAgents(sim, ::Type{T}) where { T <: AbstractAgent }
    sim.agents[typeid(sim, T)].containers[sim.agents[typeid(sim, T)].write] |>
        length
end

function transfoo(p::Person, networks, sim)
    Person(p.foo + 10)
end

function transfoo2(p::Person, networks, sim)
    s = reduce((s,e) -> s = s + e.state.foo, networks[FooEdgeState]; init = 0) 
    Person(s)
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
    @test get_write_agent(sim, p1id) == p1

    ids = add_agents!(sim, p1, p2)
    p2id = ids[2]
    @test numAgents(sim, Person) == 3
    @test get_write_agent(sim, p2id) == p2

    ids = add_agents!(sim, [[p1, p2], h1])
    @test numAgents(sim, Person) == 5
    @test numAgents(sim, HH) == 1
    @test get_write_agent(sim, ids[1][1]) == p1
    @test get_write_agent(sim, ids[2]) == h1

    h2id = add_agents!(sim, h2)

    add_edgetype!(sim, Edge, FooEdgeState)
    add_edgetype!(sim, StatelessEdge, StatelessEdgeType)

    add_edge!(sim, p1id, p2id, FooEdgeState(1))
    edge = StatelessEdge{StatelessEdgeType}(p1id, h2id)
    add_edge!(sim, edge)


    @test length(get_write_edges(sim, FooEdgeState, p2id)) == 1
    
    @test length(get_write_edges(sim, StatelessEdgeType, h2id)) == 1

 #   for i in sim.agents
    finish_init!(sim)

    apply_transition!(sim, transfoo, [ Person ])

    @test get_write_agent(sim, p2id).foo == 12

    apply_transition!(sim, transfoo2, [ Person ]; variant = [ FooEdgeState ])

    @test get_write_agent(sim, p1id).foo == 0
    @test get_write_agent(sim, p2id).foo == 1
    
 end



