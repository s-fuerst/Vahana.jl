export Simulation
export add_agenttype!
export add_agents!
# Ist Abstractvector wirklich die sinnvolle Basisklasse
# Ich verlange ja vor allem, dass das Table-Interface unterstützt wird.
include("Types.jl")

Base.@kwdef mutable struct Simulation12
    name::String
    params::Union{Tuple, NamedTuple}
    # Any will be something like Vahala_Agent_Collection
    agent_data_t = Dict{DataType, Any}()
    agent_data_tp1 = Dict{DataType, Any}()
    id_counter = Dict{DataType, T_AgentID}()
    type2number = Dict{DataType, T_TypeNumber}()
    number2type = Vector{DataType}(undef, typemax(T_TypeNumber))
end

# TOOD: when simulation structure is fixed, maybe make this
# an inner constructor
function Simulation(name::String,
             params::Union{Tuple, NamedTuple})
    Simulation12(name = name,
                 params = params)
end

# TODO: when simulation structure is fixed, add type annotations to sim
# and return value
# TODO: add data structure for type
function add_agenttype!(sim, ::Type{T}) where { T <: Agent }
    # TODO: check isbitstype incl. ignore optional
    # TODO: check that field _id exists and has the correct type
    # TODO: add/modify IDs (nö, das ist bei addAgents)
    # TODO: was ist mit privaten Daten
    type_number = reduce(max, values(sim.type2number); init = 0) + 1
    # TODO: assert type_number ist klein genug
    push!(sim.agent_data_t, T => Dict{T_AgentID, T}())
    push!(sim.agent_data_tp1, T => Dict{T_AgentID, T}())
    push!(sim.id_counter, T => 0)
    push!(sim.type2number, T => type_number)
    sim.number2type[type_number] = T
    sim
end

function add_agents!(sim, agent::Agent)
    sim.agent_data_tp1[typeof(agent)][agent.id] = agent 
end

function add_agents!(sim, agents)
    for a in agents
        add_agents!(sim, a)
    end
end

function add_agents!(sim, agents...)
    for a in agents
        add_agents!(sim, a)
    end
end
