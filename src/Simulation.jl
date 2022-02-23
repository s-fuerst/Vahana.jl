export Simulation
export add_agenttype!, add_edgetype!
export add_agents!, add_edge!
export typeid
export finish_init!
export apply_transition!

const MAX_TYPES = typemax(TypeID)

Base.@kwdef struct Simulation
    name::String
    params::Union{Tuple, NamedTuple}

    # TODO: rename to agents_coll
    agents::Vector{AgentCollection} = Vector{AgentCollection}(undef, MAX_TYPES)
    agent_typeids::Dict{DataType, TypeID} = Dict{DataType, TypeID}()

    edges::Dict{DataType, EdgeCollection} = Dict{DataType, EdgeCollection}()
end

function Simulation(name::String,
             params::Union{Tuple, NamedTuple})
    Simulation(name = name, params = params)
end

function all_agent_colls(sim)
    # TODO: Check if you could/should use genetors instead of comprehensions
    # also for the other cases
    [ sim.agents[i] for i in 1:length(sim.agent_typeids) ]
end

function some_agent_colls(sim, types)
    [ sim.agents[v] for (k, v) in sim.agent_typeids if k in types ]
end

function all_edge_colls(sim)
    values(sim.edges) |> collect
end

function some_edge_colls(sim, types)
    [ v for (k, v) in sim.edges if k in types ]
end

function finish_init!(sim::Simulation)
    foreach(finish_init!, all_agent_colls(sim))
    foreach(finish_init!, all_edge_colls(sim))
end 

######################################## Types

function typeid(sim, ::Type{T})::TypeID where { T <: AbstractAgent }
    sim.agent_typeids[T]
end

# TODO: when simulation structure is fixed, add type annotations to sim
# and return value
# TODO: add data structure for type
function add_agenttype!(sim, ::Type{T}) where { T <: AbstractAgent } 
    # TODO: check isbitstype incl. ignore optional
    # TODO: check that the same type is not added twice
    ids =sim.agent_typeids

    type_number = length(ids) + 1
    @assert type_number < typemax(TypeID) "Can not add new type, 
                                maximal number of types already registered"
    push!(ids, T => type_number)
    
    sim.agents[type_number] = BufferedAgentDict{T}()
    type_number
end

function add_edgetype!(sim, ET::Type{T}, DT::DataType) where { T <: AbstractEdge } 
    push!(sim.edges, DT => BufferedEdgeDict{ET{DT}}())
end

######################################## Agents


function add_agents!(sim::Simulation, agent::T) where { T <: AbstractAgent }
    # TODO add a better error message if type is not registered
    typeid = sim.agent_typeids[T]
    coll = sim.agents[typeid]
    coll.id_counter = coll.id_counter + 1
    agentid = agent_id(typeid, coll.id_counter)
    sim.agents[typeid][agentid] = agent
    agentid
end

function add_agents!(sim::Simulation, agents)
    [ add_agents!(sim, a) for a in agents ]
end

function add_agents!(sim::Simulation, agents...)
    [ add_agents!(sim, a) for a in agents ]
end

add_agents!(f::Function) = sim -> add_agents!(sim, f(sim))

######################################## Edges

function add_edge!(sim, edge::T) where { T <: AbstractEdge }
    push!(sim.edges[statetype(edge)], edge)
    nothing
end

function add_edge!(sim, from::AgentID, to::AgentID)
    add_edge!(sim, StatelessEdge(from, to))
end

function add_edge!(sim, from::AgentID, to::AgentID, state)
    add_edge!(sim, Edge(from, to, state))
end

# function get_edges(sim, T::DataType, agent::AgentID) 
#     sim.edges[T][agent]
# end

######################################## Transition

struct AgentEdges
    sim::Simulation
    id::AgentID
end

function Base.getindex(ae::AgentEdges, key)
    get(read_container(ae.sim.edges[key]),
        ae.id,
        Vector{statetype(ae.sim.edges[key])}())
end


# func(agent, edges, sim)
# where edges is Dict{Type, Edges} for all networks
# must calls add_agents for new agents (assuming agents without edges are
# useless) and also add_edge! (TODO: add an add_edges! method)
# returns agent, which gets the same id as agent input

function apply_transition!(sim,
                    func,
                    compute::Vector{DataType};
                    variant = Vector{DataType}())
    foreach(prepare_write!, some_agent_colls(sim, compute))
    foreach(prepare_write!, some_agent_colls(sim, variant))
    foreach(prepare_write!, some_edge_colls(sim, variant))

    for coll in some_agent_colls(sim, compute)
        for (id,state) in coll
            # the coll[id] writes into another container then
            # we use for the iteration
            coll[id] = func(state, AgentEdges(sim, id), sim)
        end 
    end

    foreach(finish_write!, some_agent_colls(sim, compute))
    foreach(finish_write!, some_agent_colls(sim, variant))
    foreach(finish_write!, some_edge_colls(sim, variant))
end
