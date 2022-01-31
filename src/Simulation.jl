export Simulation

# Ist Abstractvector wirklich die sinnvolle Basisklasse
# Ich verlange ja vor allem, dass das Table-Interface unterstützt wird.
include("Types.jl")

Base.@kwdef mutable struct Simulation11
    name::String
    params::Union{Tuple, NamedTuple}
    agent_data_t::Dict{DataType, AbstractDict{T_AgentID, Agent}} =
        Dict{DataType, AbstractDict{T_AgentID, Agent}}()
    agent_data_tp1::Dict{DataType, AbstractDict{T_AgentID, Agent}} =
        Dict{DataType, AbstractDict{T_AgentID, Agent}}()
    id_counter::Dict{DataType, T_AgentID} =
        Dict{DataType, T_AgentID}()
    type2number::Dict{DataType, T_TypeNumber} =
        Dict{DataType, T_TypeNumber}()
    number2type::Vector{DataType} =
        Vector{DataType}(undef, typemax(T_TypeNumber))
end



# TOOD: when simulation structure is fixed, maybe make this
# an inner constructor
function Simulation(name::String,
             params::Union{Tuple, NamedTuple})
    Simulation11(name = name,
                 params = params)
end



# TODO: when simulation structure is fixed, add type annotations to sim
# and return value
# TODO: add data structure for type
function add_agenttype!(sim, type::Type{T}) where { T <: Agent }
    # TODO: check isbitstype incl. ignore optional
    # TODO: check that field _id exists and has the correct type
    # TODO: add/modify IDs (nö, das ist bei addAgents)
    # TODO: was ist mit privaten Daten
    type_number = reduce(max, values(sim.type2number); init = 0) + 1
    # TODO: assert type_number ist klein genug
    push!(sim.agent_data_t, type => Dict{T_AgentID, T}())
    push!(sim.agent_data_tp1, type => Dict{T_AgentID, T}())
    push!(sim.id_counter, type => 0)
    push!(sim.type2number, type => type_number)
    sim.number2type[type_number] = type
    sim
end

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
    sim.agent_data_tp1[typeof(agent)] = 
end

# function add_agents!(sim, agent::iterable agents)
# for
# add_agents()
# end
