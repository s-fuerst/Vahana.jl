export Simulation
export add_agenttype!, add_edgetype!
export add_agents!, add_edge!
export agent_typeid
export finish_init!

const NUM_BUFFERS = 2 
const MAX_TYPES = typemax(TypeID)

    
Base.@kwdef mutable struct Simulation
    name::String
    params::Union{Tuple, NamedTuple}
    agents = Vector{AgentCollection}(undef, MAX_TYPES)
    agent_typeids = Dict{DataType, TypeID}()
    # The Values are the Dict{AgentID, Vector{Edges}}
    edges = Array{Dict{AgentID, Any}}(undef, MAX_TYPES, NUM_BUFFERS)
    edge_typeids = Dict{DataType, TypeID}()
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


function Base.show(io::IO, ::MIME"text/plain", sim::Simulation)
    printstyled(io, "Simulation Name: ", sim.name, "\n"; color = :blue)
    println(io, "Parameters: ", sim.params)
    printstyled(io, length(sim.agent_typeids), " Agent Type(s):";
                color = :cyan)
    for (k, v) in sim.agent_typeids
        print(io, "\n\t", k, " (ID: ", agent_typeid(sim, k), ")",
              " with ", length(sim.agents[v]), " Agent(s)")
    end
    # println(io, "Edges:")
    # for (k, v) in sim.edge_typeids
    #     println(io, "\t", k, " with ", length(sim.agents[v]), " Agent(s)")
    # end
end

function add_type!(ids::Dict{DataType, TypeID}, T::DataType)
    type_number = length(ids) + 1
    @assert type_number < typemax(TypeID) "Can not add new type, 
                                maximal number of types already registered"
    push!(ids, T => type_number)
    type_number
end


agent_typeid(sim, type::DataType) = sim.agent_typeids[type]

# TODO: when simulation structure is fixed, add type annotations to sim
# and return value
# TODO: add data structure for type
function add_agenttype!(sim, ::Type{T}) where { T <: Agent } 
    # TODO: check isbitstype incl. ignore optional
    # TODO: check that the same type is not added twice
    type_number = add_type!(sim.agent_typeids, T)

    sim.agents[type_number] = BufferedAgentDict{T}()
    type_number
end


function add_edgetype!(sim, ::Type{T}) where { T } 
    type_number = add_type!(sim.edge_typeids, T)

    foreach(i -> sim.edges[type_number, i] = Dict{AgentID, Vector{Edge{T}}},
            1:NUM_BUFFERS)
    type_number
#     push!(sim.edges[sim.next],
#           T => Dict{AgentID,  Vector{Edge{T}}}())
end

function new_agent_id!(sim, id::TypeID)
    c = sim.agents[id]
    c.id_counter = c.id_counter + 1
    agent_id(id, c.id_counter)
end

function add_agents!(sim::Simulation, agent::T) where { T <: Agent } 
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


function add_edge!(sim, edge::Edge{T}) where { T }
    dict = get!(sim.edges[sim.next][T], edge.to, Vector{Edge{T}}())
    push!(dict, edge)
end

function add_edge!(sim, from::AgentID, to::AgentID, state::T) where { T }
    add_edge!(sim, Edge(from, to, state))
end

function finish_init!(sim)
end 
