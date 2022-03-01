using Vahana
using Test

struct Person <: AbstractAgent
    foo::Int64
end

struct HH <: AbstractAgent
    bar::Int64
end

 
struct FooEdgeState <: AbstractEdge
    foo::Int64
end

struct StatelessEdgeType <: AbstractEdge end

function createPersons(sim)
    [ Person(11), Person(12) ]
end

function get_write_agent(sim, id::AgentID)
    sim.agents[type_nr(id)].containers[sim.agents[type_nr(id)].write][id]
end

function get_write_edges(sim, T::DataType, id::AgentID)
    sim.edges[T].containers[sim.edges[T].write][id]
end

function numAgents(sim, ::Type{T}) where {T <: AbstractAgent}
    sim.agents[typeid(sim, T)].containers[sim.agents[typeid(sim, T)].write] |>
        length
end

function transfoo(p::Person, id, network, sim)
    Person(p.foo + 10)
end

function transfoo2(p::Person, id, network, sim)
    s = reduce((s,e) ->
        s = s + e.state.foo, network(sim, FooEdgeState); init = 0) 
    Person(s)
end

function transfoo3(p::Person, id, network, sim)
    n = network(sim, FooEdgeState)
    if length(n) > 0
        s = agent_from(sim, n[1]).foo
    else
        s = -1
    end
    Person(s)
end

function transnothing(p::Person, _, _, _)
    nothing
end

function transstateless(h::HH, id, network, sim)
    n = network(sim, StatelessEdgeType)
    if length(n) > 0
        s = agent_from(sim, n[1]).foo
    else
        s = -1
    end
    HH(s)
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

    ids = add_agents!(sim, p1, p2)
    
    h1id = add_agents!(sim, h1)
    @test numAgents(sim, Person) == 5
    @test numAgents(sim, HH) == 1
    @test get_write_agent(sim, ids[1]) == p1
    @test get_write_agent(sim, h1id) == h1

    h2id = add_agents!(sim, h2)

    add_edgetype!(sim, FooEdgeState)
    add_edgetype!(sim, StatelessEdgeType)

    add_edge!(sim, p1id, p2id, FooEdgeState(1))
    # edge = StatelessEdge{StatelessEdgeType}(p1id, h2id)
    # add_edge!(sim, edge)
    add_edge!(sim, p1id, h2id, StatelessEdgeType)


    @test length(get_write_edges(sim, FooEdgeState, p2id)) == 1
    
    @test length(get_write_edges(sim, StatelessEdgeType, h2id)) == 1

 #   for i in sim.agents
    finish_init!(sim)


    apply_transition!(sim, transstateless, [ HH ])
    @test get_write_agent(sim, h1id).bar == -1
    @test get_write_agent(sim, h2id).bar == 1
    
    apply_transition!(sim, transfoo, [ Person ])

    @test get_write_agent(sim, p2id).foo == 12

    apply_transition!(sim, transfoo2, [ Person ])

    @test get_write_agent(sim, p1id).foo == 0
    @test get_write_agent(sim, p2id).foo == 1

    apply_transition!(sim, transfoo3, [ Person ])

    @test get_write_agent(sim, p1id).foo == -1
    @test get_write_agent(sim, p2id).foo == 0

    apply_transition!(sim, transnothing, [ Person ])
    @test numAgents(sim, Person) == 0
    
 end



@testset "Globals" begin
    struct GlobalFoo <: AbstractGlobal
        foo::Float64
        bar::Int64
    end

    struct GlobalBar <: AbstractGlobal
        foo::Float64
        bar::Int64
    end

    
    sim = Simulation("Globals Test", ())
    add_globalstatetype!(sim, GlobalFoo)
    push_global!(sim, GlobalFoo(1.1, 1))

    @test current_state(sim, GlobalFoo) == GlobalFoo(1.1, 1)

    push_global!(sim, GlobalFoo(0, 2))
    @test current_state(sim, GlobalFoo) == GlobalFoo(0, 2)
    @test all_states(sim, GlobalFoo) == GlobalFoo(0, 2)

    add_globalseriestype!(sim, GlobalBar)
    push_global!(sim, GlobalBar(0, 0))
    @test current_state(sim, GlobalBar) == GlobalBar(0, 0)

    push_global!(sim, GlobalBar(1, 1))
    @test current_state(sim, GlobalBar) == GlobalBar(1, 1)
    @test all_states(sim, GlobalBar) |> first == GlobalBar(0, 0)
end

@testset "Aggregate" begin
    sim = Simulation("Aggregate", ())

    add_agenttype!(sim, Person)
    pids = add_agents!(sim, [ Person(i) for i in 1:10 ])

    add_edgetype!(sim, FooEdgeState)
    for i in pids
        add_edge!(sim, i, last(pids), FooEdgeState(1))
    end
    finish_init!(sim)
    
    @test aggregate(sim, Person, p -> p.foo, +) == sum(1:10)
    @test aggregate(sim, FooEdgeState, e -> e.foo, +) == 10
end


@testset "Tutorial1" begin
    
    include("tutorial1.jl")
    params = (numBuyer = 500, numSeller = 2, knownSellers = 2)
    
    sim = run_simulation(5, params)

    @test 0.8 < current_state(sim, AveragePrice).p < 1.2
end
