sim = Simulation("Example", ())

struct Person <: Agent
    id::T_AgentID
    foo::Int64
end

p1 = Person(1, 1)
p2 = Person(2, 1)

struct Household2 <: Agent
    id::T_AgentID
end

function Household(id)
    Household2(id)
end

h1 = Household(1)
h2 = Household(2)

add_agenttype!(sim, Person)
