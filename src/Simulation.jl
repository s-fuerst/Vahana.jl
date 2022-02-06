export Simulation
export add_agenttype!, add_edgetype!
export add_agents!, add_edge!
export finish_init!

const MAX_STEPS = 2 
const MAX_TYPES = typemax(TypeID)

abstract type AgentContainer end

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
    current
end

######################################## Simulation

Base.@kwdef mutable struct Simulation
    name::String
    params::Union{Tuple, NamedTuple}
    # Any will be something like Vahala_Agent_Collection
    # agents is a matrix with typemax(Typenumber) as first index
    # and MAX_STEPS as second index
    # The Values are the Dict{AgentID, Agent}
    # where Dict will be something like Vahala_Agent_Collection
    agents = Array{Dict{AgentID, Any}}(undef, MAX_TYPES, MAX_STEPS)
    # The Values are the Dict{AgentID, Vector{Edges}}
    edges = Array{Dict{AgentID, Any}}(undef, MAX_TYPES, MAX_STEPS)
    agent_type_ids::TypeIDs = TypeIDs()
    edge_type_ids::TypeIDs = TypeIDs()
    read = fill(1, MAX_TYPES)
    write = fill(1, MAX_TYPES)
    
    next = 1
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

# TODO: when simulation structure is fixed, add type annotations to sim
# and return value
# TODO: add data structure for type
function add_agenttype!(sim, ::Type{T}) where {T} #where { T <: Agent }
    # TODO: check isbitstype incl. ignore optional
    # TODO: was ist mit privaten Daten
    type_number = add_type!(sim.agent_type_ids, T)

    foreach(i -> sim.agents[type_number, i] = Dict{AgentID, T}(),
            1:MAX_STEPS)
    #    push!(sim.agents[sim.next], T => Dict{AgentID, T}())
    type_number
end


function add_edgetype!(sim, ::Type{T}) where { T } 
    type_number = add_type!(sim.edge_type_ids, T)

    foreach(i -> sim.edges[type_number, i] = Dict{AgentID, Vector{Edge{T}}},
            1:MAX_STEPS)
    type_number
#     push!(sim.edges[sim.next],
#           T => Dict{AgentID,  Vector{Edge{T}}}())
end

function add_agents!(sim::Simulation, agent::T) where { T <: Agent }
    ids = sim.agent_type_ids
    nr = ids.type2number[T]
    id = new_agent_id!(ids, nr)
    sim.agents[nr, sim.write[nr]][id] = agent
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
    sim.next = 2
    # sim.agents_t = sim.agents_tp1
    for (k, v) in sim.agents[sim.current]
        sim.agents[sim.next][k] = typeof(v)()
    end

    for (k, v) in sim.edges[sim.current]
        sim.edges[sim.next][k] = typeof(v)()
    end


    # sim.edges_t = sim.edges_tp1
    # for (k, v) in sim.edges_tp1
    #     sim.edges_tp1[k] = typeof(v)()
    # end
end 
