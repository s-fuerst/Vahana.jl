function createPersons(sim)
    [ Person(11), Person(12) ]
end

function get_write_agent(sim, id::AgentID)
    sim.agents[type_nr(id)].containers[sim.agents[type_nr(id)].write][id]
end

function get_write_edges(sim, T::DataType, id::AgentID)
    sim.edges[T].containers[sim.edges[T].write][id]
end

function numAgents(sim, ::Type{T}) where {T <: Agent}
    sim.agents[typeid(sim, T)].containers[sim.agents[typeid(sim, T)].write] |>
        length
end

function transfoo(p::Person, id, sim)
    Person(p.foo + 10)
end

function transfoo2(p::Person, id, sim)
    s = reduce((s,e) ->
        s = s + e.state.foo, edges_to(sim, id, FooEdgeState); init = 0) 
    Person(s)
end

function transfoo3(p::Person, id, sim)
    n = edges_to(sim, id, FooEdgeState)
    if length(n) > 0
        s = agentstate(sim, n[1].from).foo
    else
        s = -1
    end
    Person(s)
end

function transnothing(p::Person, _, _)
    nothing
end

function transstateless(h::HH, id, sim)
    n = edges_to(sim, id, StatelessEdgeType) |> neighbors
    if length(n) > 0
        s = agentstate(sim, n[1]).foo
    else
        s = -1
    end
    HH(s)
end

function transaddedges(p::Person, id, sim)
    add_edges!(sim, id, edges_to(sim, id, FooEdgeState))
    p
end


@testset "Initialization" begin
    sim = Simulation("Example", nothing, nothing)

    p1 = Person(1)
    p2 = Person(2)

    h1 = HH(1)
    h2 = HH(2)

    add_agenttype!(sim, Person)
    @test numAgents(sim, Person) == 0
    @test sim.agent_typeids[Person] == 1

    add_agenttype!(sim, HH)
    @test sim.agent_typeids[HH] == 2

    p1id = add_agent!(sim, p1)
    @test numAgents(sim, Person) == 1
    @test get_write_agent(sim, p1id) == p1

    ids = add_agents!(sim, p1, p2)
    p2id = ids[2]
    @test numAgents(sim, Person) == 3
    @test get_write_agent(sim, p2id) == p2

    ids = add_agents!(sim, p1, p2)
    
    h1id = add_agent!(sim, h1)
    @test numAgents(sim, Person) == 5
    @test numAgents(sim, HH) == 1
    @test get_write_agent(sim, ids[1]) == p1
    @test get_write_agent(sim, h1id) == h1

    h2id = add_agent!(sim, h2)

    add_edgetype!(sim, FooEdgeState)
    add_edgetype!(sim, StatelessEdgeType)

    add_edge!(sim, p1id, p2id, FooEdgeState(1))
    # edge = StatelessEdge{StatelessEdgeType}(p1id, h2id)
    # add_edge!(sim, edge)
    add_edge!(sim, p1id, h2id, StatelessEdgeType)


    @test length(get_write_edges(sim, FooEdgeState, p2id)) == 1
    
    @test length(get_write_edges(sim, StatelessEdgeType, h2id)) == 1


    anotherpid = add_agent!(sim, Person(1))
    add_edges!(sim, anotherpid,
               Edge(anotherpid, FooEdgeState(1)),
               Edge(anotherpid, FooEdgeState(2)))
    
    
 #   for i in sim.agents
    finish_init!(sim)

    # this should not change anything, but test the add_edges method
    apply_transition!(sim, transaddedges, [ Person ], [ FooEdgeState ], [ FooEdgeState ])

    apply_transition!(sim, transstateless, [ HH ], [ StatelessEdgeType ], [])
    @test get_write_agent(sim, h1id).bar == -1
    @test get_write_agent(sim, h2id).bar == 1
    
    apply_transition!(sim, transfoo, [ Person ], [], [])

    @test get_write_agent(sim, p2id).foo == 12

    apply_transition!(sim, transfoo2, [ Person ], [ FooEdgeState ], [])

    @test get_write_agent(sim, p1id).foo == 0
    @test get_write_agent(sim, p2id).foo == 1

    @test get_write_agent(sim, anotherpid) == Person(3)
    
    apply_transition!(sim, transfoo3, [ Person ], [ FooEdgeState ], [])

    @test get_write_agent(sim, p1id).foo == -1
    @test get_write_agent(sim, p2id).foo == 0

    apply_transition!(sim, transnothing, [ Person ], [], [])
    @test numAgents(sim, Person) == 0
    
 end


