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

add_edgetype!(sim, Edge{FooEdgeState})

add_edge!(sim, p1id, p2id, FooEdgeState(0))

persons = [ add_agents!(sim, Person(i)) for i in 1:1000]

[ add_edge!(sim, persons[1], persons[2], FooEdgeState(i)) for i = 1:1000]

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
