export Simulation
export add_agenttype!, add_edgetype!
export agenttypeid
export add_agents!, add_edge!
export finish_init!

export BufferedAgentDict

const NUM_BUFFERS = 2 
const MAX_TYPES = typemax(TypeID)

######################################## TypeIDs

Base.@kwdef struct TypeIDs
    type2number = Dict{DataType, TypeID}()
    id_counter = Vector{AgentNr}(undef, typemax(TypeID))
    number2type = Vector{DataType}(undef, typemax(TypeID))
end

function add_type!(ids::TypeIDs, T::DataType)
    type_number = reduce(max, values(ids.type2number); init = 0) + 1
    @assert type_number < typemax(TypeID) "Can not add new type, maximal number of types already registered"
    push!(ids.type2number, T => type_number)
    ids.id_counter[type_number] = 0
    ids.number2type[type_number] = T
    type_number
end

function new_agent_id!(ids::TypeIDs, type_nr::TypeID)::AgentID
    current = ids.id_counter[type_nr]
    ids.id_counter[type_nr] = ids.id_counter[type_nr] + 1
    agent_id(type_nr, current)
end

######################################## Simulation

abstract type AgentContainer{T} end
# AgentContainer interface:
# setindex!
# getindex
# length

function setindex!(::AgentContainer{T}, _, _) where {T}
    @assert false "setIndex! is not defined for this AgentCointainer type"
end

function getindex(::AgentContainer{T}, _) where {T}
    @assert false "getIndex is not defined for this AgentCointainer type"
end

# TODO: Disable all constructures beside ()
Base.@kwdef struct BufferedAgentDict{T} <: AgentContainer{T}
    containers = [ Dict{AgentID, T}() for i = 1:NUM_BUFFERS ]
    read = 1
    write = 1
end

function Base.setindex!(coll::BufferedAgentDict{T}, value::T, key::AgentID) where { T }
    Base.setindex!(coll.containers[coll.write], value, key)
end

function Base.getindex(coll::BufferedAgentDict{T}, key::AgentID) where { T }
    Base.getindex(coll.containers[coll.read], key)
end

function Base.iterate(coll::BufferedAgentDict{T}) where { T }
    Base.iterate(coll.containers[coll.read])
end

function Base.iterate(coll::BufferedAgentDict{T}, state) where { T }
    Base.iterate(coll.containers[coll.read], state)
end

function Base.length(coll::BufferedAgentDict{T}) where { T }
    Base.length(coll.containers[coll.read])
end
    
    
Base.@kwdef mutable struct Simulation
    name::String
    params::Union{Tuple, NamedTuple}
    agents = Vector{AgentContainer}(undef, MAX_TYPES)
    # The Values are the Dict{AgentID, Vector{Edges}}
    edges = Array{Dict{AgentID, Any}}(undef, MAX_TYPES, NUM_BUFFERS)
    agent_type_ids = TypeIDs()
    edge_type_ids = TypeIDs()
end

# TOOD: when simulation structure is fixed, maybe make this
# an inner constructor
function Simulation(name::String,
             params::Union{Tuple, NamedTuple})
    sim = Simulation(name = name,
                     params = params)
    # TODO StatelessEdges
    # push!(sim.edges[sim.next], StatelessEdge => Vector{StatelessEdge})
    
    #    add_edgetype!(sim, StatelessEdge(::UInt64, ::UInt64))
    sim
end

agenttypeid(sim, type::DataType) = sim.agent_type_ids.type2number[type]

# TODO: when simulation structure is fixed, add type annotations to sim
# and return value
# TODO: add data structure for type
function add_agenttype!(sim, ::Type{T}) where {T} #where { T <: Agent }
    # TODO: check isbitstype incl. ignore optional
    # TODO: was ist mit privaten Daten
    type_number = add_type!(sim.agent_type_ids, T)

    sim.agents[type_number] = BufferedAgentDict{T}()
    type_number
end


function add_edgetype!(sim, ::Type{T}) where { T } 
    type_number = add_type!(sim.edge_type_ids, T)

    foreach(i -> sim.edges[type_number, i] = Dict{AgentID, Vector{Edge{T}}},
            1:NUM_BUFFERS)
    type_number
#     push!(sim.edges[sim.next],
#           T => Dict{AgentID,  Vector{Edge{T}}}())
end

function add_agents!(sim::Simulation, agent::T) where { T <: Agent }
    ids = sim.agent_type_ids
    nr = ids.type2number[T]
    id = new_agent_id!(ids, nr)
    sim.agents[nr][id] = agent
    id
end

function add_agents!(sim::Simulation, agents)
    [ add_agents!(sim, a) for a in agents ]
end

function add_agents!(sim::Simulation, agents...)
    [ add_agents!(sim, a) for a in agents ]
end

add_agents!(f::Function) = sim -> add_agents!(sim, f(sim))


function add_edge!(sim, edge::Edge{T}) where { T }
    dict = get!(sim.edges[sim.next][T], edge.to, Vector{Edge{T}}())
    push!(dict, edge)
end

function add_edge!(sim, from::AgentID, to::AgentID, state::T) where { T }
    add_edge!(sim, Edge(from, to, state))
end

function finish_init!(sim)
end 
