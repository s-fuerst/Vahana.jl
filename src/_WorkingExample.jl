using Vahana

sim = Simulation("Example", ())

struct Person2 <: Agent
    foo::Int64
end

p1 = Person2(1)
p2 = Person2(2)

struct Household2 <: Agent
end


h1 = Household2()
h2 = Household2()

add_agenttype!(sim, Person2)

add_agenttype!(sim, Household2)


(p1id, p2id) = add_agents!(sim, p1, p2)

sim.agents[agent_typeid(sim, Person2)]

sim

struct FooEdgeState 
    foo::Int64
end

add_edgetype!(sim, Edge{FooEdgeState})

add_edge!(sim, p1id, p2id, FooEdgeState(0))



persons = [ add_agents!(sim, Person2(i)) for i in 1:1000]

[ add_edge!(sim, persons[1], persons[2], FooEdgeState(i)) for i = 1:1000]


