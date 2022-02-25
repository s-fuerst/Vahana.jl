using Vahana

sim = Simulation("Example", ())

struct Person <: AbstractAgent
    foo::Int64
end

struct Household <: AbstractAgent
end

p1 = Person(1)
p2 = Person(2)
    
h1 = Household()
h2 = Household()


add_agenttype!(sim, Person)

add_agenttype!(sim, Household)


(p1id, p2id) = add_agents!(sim, p1, p2)

sim.agents[typeid(sim, Person)]

sim

struct FooEdgeState 
    foo::Int64
end


add_edgetype!(sim,FooEdgeState)


add_edge!(sim, p1id, p2id, FooEdgeState(0))

persons = [ add_agents!(sim, Person(i)) for i in 1:3]

[ add_edge!(sim, persons[1], persons[i], FooEdgeState(i)) for i = 1:3]

# mean war mal bei 306, im Moment wieder bei 340, obwohl ich eigentlich
# nichts geändert habe?
function bench_this()
    sim = Simulation("Example", ())

    # p1 = Person(1)
    # p2 = Person(2)
    
    # h1 = Household()
    # h2 = Household()
#    add_edgetype!(sim, Edge{FooEdgeState})
    add_agenttype!(sim, Person)

    r = rand(Int64)
    persons = [ add_agents!(sim, Person(i * r)) for i in 1:1000000 ]
    sim
end

function transfoo(p::Person, networks, sim)
    Person(p.foo + 10)
end

function transfoo2(p::Person, networks, sim)
    s = reduce((s,e) -> s = s + e.state.foo, networks[FooEdgeState]; init = 0) 
    Person(s)
end

add_agents!(sim, Household())

struct Info 
    p::Float64
end

add_globalstatetype!(sim, Info)


finish_init!(sim)

# show(sim)
#apply_transition!(sim, transfoo2, [ Person ])
