# function createAgentBars(sim)
#     [ AgentBar(11), AgentBar(12) ]
# end

# function get_write_agent(sim, id::AgentID)
#     sim.agents[type_nr(id)].containers[sim.agents[type_nr(id)].write][id]
# end

# function get_write_edges(sim, T::DataType, id::AgentID)
#     sim.edges[T].containers[sim.edges[T].write][id]
# end

# function numAgents(sim, ::Type{T}) where {T <: Agent}
#     sim.agents[typeid(sim, T)].containers[sim.agents[typeid(sim, T)].write] |>
#         length
# end

function transfoo(p::AgentBar, id, sim)
    AgentBar(p.foo + 10)
end

function transfoo2(p::AgentBar, id, sim)
    s = reduce((s,e) ->
        s = s + e.state.foo, edges_to(sim, id, FooEdgeState); init = 0) 
    AgentBar(s)
end

function transfoo3(p::AgentBar, id, sim)
    n = edges_to(sim, id, FooEdgeState)
    if length(n) > 0
        s = agentstate(sim, n[1].from).foo
    else
        s = -1
    end
    AgentBar(s)
end

function transnothing(p::AgentBar, _, _)
    nothing
end

function transstateless(h::AgentFoo, id, sim)
    n = edges_to(sim, id, StatelessEdgeType) |> neighbors
    if length(n) > 0
        s = agentstate(sim, n[1]).foo
    else
        s = -1
    end
    AgentFoo(s)
end

function transaddedges(p::AgentBar, id, sim)
    add_edges!(sim, id, edges_to(sim, id, FooEdgeState))
    p
end

if false
    struct AgentFoo
        foo::Int64
    end

    struct AgentBar 
        bar::Int64
    end

    struct FooEdgeState 
        foo::Int64
    end

    struct StatelessEdgeType end
end


@testset "Initialization" begin
    sim = ModelTypes() |>
        add_agenttype!(AgentBar) |>
        add_agenttype!(AgentFoo) |>
        add_edgetype!(StatelessEdgeType) |>
        add_edgetype!(FooEdgeState) |>
        construct("Test", nothing, nothing)

    p1 = AgentBar(1)
    p2 = AgentBar(2)

    h1 = AgentFoo(1)
    h2 = AgentFoo(2)

    #    @test numAgents(sim, AgentBar) == 0
    #    @test sim.typeinfos.nodes_type2id[AgentBar] == 1
    #    @test sim.typeinfos.nodes_type2id[AgentFoo] == 2

    p1id = add_agent!(sim, p1)
    h1id = add_agent!(sim, h1)
    h2id, p2id = add_agents!(sim, h2, p2)
    fooids = add_agents!(sim, [ AgentFoo(i) for i in 10:20])

    add_edge!(sim, p1id, h1id, StatelessEdgeType)
    add_edge!(sim, h2id, Edge(p1id, StatelessEdgeType()))
    add_edge!(sim, h2id, Edge(p1id, FooEdgeState(1)))
    add_edges!(sim, h2id, [ Edge(i, FooEdgeState(i)) for i in fooids])

    #finish_init!(sim)
#    sim.AgentBar_read = sim.AgentBar_write

    finish_init!(sim)


    @test agentstate_flexible(sim, p1id) == p1
    @test agentstate(sim, p1id, Val(AgentBar)) == p1
    @test agentstate(sim, last(fooids), Val(AgentFoo)) == AgentFoo(20)

    @test size(edges_to(sim, h1id, Val(StatelessEdgeType)), 1) == 1
    @test size(edges_to(sim, h2id, Val(StatelessEdgeType)), 1) == 1
    @test size(edges_to(sim, h2id, Val(FooEdgeState)), 1) == 12

    return 
    # this should not change anything, but test the add_edges method
    apply_transition!(sim, transaddedges, [ AgentBar ], [ FooEdgeState ], [ FooEdgeState ])

    apply_transition!(sim, transstateless, [ AgentFoo ], [ StatelessEdgeType ], [])
    @test get_write_agent(sim, h1id).bar == -1
    @test get_write_agent(sim, h2id).bar == 1
    
    apply_transition!(sim, transfoo, [ AgentBar ], [], [])

    @test get_write_agent(sim, p2id).foo == 12

    apply_transition!(sim, transfoo2, [ AgentBar ], [ FooEdgeState ], [])

    @test get_write_agent(sim, p1id).foo == 0
    @test get_write_agent(sim, p2id).foo == 1

    @test get_write_agent(sim, anotherpid) == AgentBar(3)
    
    apply_transition!(sim, transfoo3, [ AgentBar ], [ FooEdgeState ], [])

    @test get_write_agent(sim, p1id).foo == -1
    @test get_write_agent(sim, p2id).foo == 0

    apply_transition!(sim, transnothing, [ AgentBar ], [], [])
    @test numAgents(sim, AgentBar) == 0
    
end


