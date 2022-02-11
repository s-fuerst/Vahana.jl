export Simulation
export add_agenttype!, add_edgetype!
export add_agents!, add_edge!
export typeid
export get_edges
export finish_init!

const MAX_TYPES = typemax(TypeID)

Base.@kwdef struct Simulation
    name::String
    params::Union{Tuple, NamedTuple}

    agents::Vector{AgentCollection} = Vector{AgentCollection}(undef, MAX_TYPES)
    agent_typeids::Dict{DataType, TypeID} = Dict{DataType, TypeID}()

    edges::Array{EdgeCollection} = Array{EdgeCollection}(undef, MAX_TYPES)
    edge_typeids::Dict{DataType, TypeID} = Dict{DataType, TypeID}()
end

function Simulation(name::String,
             params::Union{Tuple, NamedTuple})
    sim = Simulation(name = name,
                     params = params)
    add_edgetype!(sim, StatelessEdge)
    sim
end

function finish_init!(sim)
end 

######################################## Types

function typeid(sim, ::Type{T})::TypeID where { T <: AbstractAgent }
    sim.agent_typeids[T]
end

function typeid(sim, ::Type{T})::TypeID where { T <: AbstractEdge }
    sim.edge_typeids[T]
end

function add_type!(ids::Dict{DataType, TypeID}, T::DataType)
    type_number = length(ids) + 1
    @assert type_number < typemax(TypeID) "Can not add new type, 
                                maximal number of types already registered"
    push!(ids, T => type_number)
    type_number
end

# TODO: when simulation structure is fixed, add type annotations to sim
# and return value
# TODO: add data structure for type
function add_agenttype!(sim, ::Type{T}) where { T <: AbstractAgent } 
    # TODO: check isbitstype incl. ignore optional
    # TODO: check that the same type is not added twice
    type_number = add_type!(sim.agent_typeids, T)

    sim.agents[type_number] = BufferedAgentDict{T}()
    type_number
end


function add_edgetype!(sim, ::Type{T}) where { T <: AbstractEdge } 
    type_number = add_type!(sim.edge_typeids, T)

    sim.edges[type_number] = BufferedEdgeDict{T}()
    type_number
    #     push!(sim.edges[sim.next],
    #           T => Dict{AgentID,  Vector{Edge{T}}}())
end

######################################## Agents

function new_agent_id!(sim, id::TypeID)
    c = sim.agents[id]
    c.id_counter = c.id_counter + 1
    agent_id(id, c.id_counter)
end

function add_agents!(sim::Simulation, agent::T) where { T <: AbstractAgent }
    # TODO add a better error message if type is not registered
    nr = sim.agent_typeids[T]
    id = new_agent_id!(sim, nr)
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

function get_agents(sim, typeid::TypeID)
    coll = sim.agents[typeid]
    coll.containers[coll.read]
end

function get_agents(sim, ::Type{T}) where { T <: AbstractAgent }
    get_agents(sim, typeid(sim, T))
end

######################################## Edges

function add_edge!(sim, edge::T) where { T <: AbstractEdge }
    dict = sim.edges[sim.edge_typeids[T]]
    push!(dict, edge)
    nothing
end

function add_edge!(sim, from::AgentID, to::AgentID)
    add_edge!(sim, StatelessEdge(from, to))
end

function add_edge!(sim, from::AgentID, to::AgentID, state)
    add_edge!(sim, Edge(from, to, state))
end

function get_edges(sim, typeid::TypeID, agent::AgentID)
    sim.edges[typeid][agent]
end

function get_edges(sim, ::Type{T}, agent::AgentID) where { T <: AbstractEdge }
    get_edges(sim, typeid(sim, T), agent)
end

