export Simulation
export add_agenttype!, add_edgetype!
export add_agents!, add_edge!
export finish_init!
# Ist Abstractvector wirklich die sinnvolle Basisklasse
# Ich verlange ja vor allem, dass das Table-Interface unterstützt wird.

Base.@kwdef mutable struct Simulation13
    name::String
    params::Union{Tuple, NamedTuple}
    # Any will be something like Vahala_Agent_Collection
    agent_data_t = Dict{DataType, Any}()
    agent_data_tp1 = Dict{DataType, Any}()
    edge_data_t = Dict{DataType, Any}()
    edge_data_tp1 = Dict{DataType, Any}()
    id_counter = Dict{DataType, T_AgentNr}()
    type2number = Dict{DataType, T_TypeNumber}()
    number2type = Vector{DataType}(undef, typemax(T_TypeNumber))
end

# TOOD: when simulation structure is fixed, maybe make this
# an inner constructor
function Simulation(name::String,
             params::Union{Tuple, NamedTuple})
    sim = Simulation13(name = name,
                       params = params)
#    push!(sim.edge_data_t, StatelessEdge => Vector{StatelessEdge})
    push!(sim.edge_data_tp1, StatelessEdge => Vector{StatelessEdge})
    
    #    add_edgetype!(sim, StatelessEdge(::UInt64, ::UInt64))
    sim
end

# TODO: when simulation structure is fixed, add type annotations to sim
# and return value
# TODO: add data structure for type
function add_agenttype!(sim, ::Type{T}) where {T} #where { T <: Agent }
    # TODO: check isbitstype incl. ignore optional
    # TODO: check that field _id exists and has the correct type
    # TODO: was ist mit privaten Daten
    type_number = reduce(max, values(sim.type2number); init = 0) + 1
    # TODO: assert type_number ist klein genug
 #   push!(sim.agent_data_t, T => Dict{T_AgentID, T}())
    push!(sim.agent_data_tp1, T => Dict{T_AgentID, T}())
    push!(sim.id_counter, T => 0)
    push!(sim.type2number, T => type_number)
    sim.number2type[type_number] = T
    type_number
end

function add_edgetype!(sim, ::Type{T}) where { T } 
#    push!(sim.edge_data_t, T => Vector{T})
    push!(sim.edge_data_tp1, T => Dict{T_AgentID,  Vector{Edge{T}}}())
end

function add_agents!(sim::Simulation13, agent::T) where { T <: Agent }
    id = agentId(sim.type2number[T], sim.id_counter[T])
    sim.id_counter[T] = sim.id_counter[T] + 1
    sim.agent_data_tp1[T][id] = agent
    id
end

function add_agents!(sim::Simulation13, agents)
    [ add_agents!(sim, a) for a in agents ]
end

function add_agents!(sim::Simulation13, agents...)
    [ add_agents!(sim, a) for a in agents ]
end

add_agents!(f::Function) = sim -> add_agents!(sim, f(sim))


function add_edge!(sim, edge::Edge{T}) where { T }
    dict = get!(sim.edge_data_tp1[T], edge.to, Vector{Edge{T}}())
    push!(dict, edge)
end

function add_edge!(sim, from::T_AgentID, to::T_AgentID, state::T) where { T }
    add_edge!(sim, Edge(from, to, state))
end

function finish_init!(sim)
    sim.agent_data_t = sim.agent_data_tp1
    for (k, v) in sim.agent_data_tp1
        sim.agent_data_tp1[k] = typeof(v)()
    end

    # sim.edge_data_t = sim.edge_data_tp1
    # for (k, v) in sim.edge_data_tp1
    #     sim.edge_data_tp1[k] = typeof(v)()
    # end
end 
