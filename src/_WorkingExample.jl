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


add_agents!(sim, p1, p2)

sim.agents[agent_typeid(sim, Person2)]

sim

AgentID(0x12000020000003)
