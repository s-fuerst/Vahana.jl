export Simulation

# Ist Abstractvector wirklich die sinnvolle Basisklasse
# Ich verlange ja vor allem, dass das Table-Interface unterstützt wird.

mutable struct Simulation5
    name::String
    agentData::Dict{DataType, Vector{DataType}}
end

function Simulation(name::String)
    Simulation5(name, Dict{DataType, Vector{DataType}}())
end

function add_agenttype!(sim::Simulation5, type::DataType)::Simulation5
    push!(sim.agentData, type => Vector{type}())
    sim
end

sim = Simulation("foo")

struct Person2
    id::Int32
end

add_agenttype!(sim, Person2)

function createAgents(sim::Simulation)
    params(sim).foo # access to parameters

    p = params(sim)
    p.foo

    return (Households = Vector())
    # oder
    return Dict(:Households => Vector())
end



