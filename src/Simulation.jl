export Simulation
export add_agenttype!, add_edgetype!
export add_globalstatetype!, add_globalseriestype!
export add_agents!, add_edge!
export finish_init!
export typeid
export apply_transition, apply_transition!, apply_transition_params
export agent_from, param
export aggregate
export show_agents, show_network

const MAX_TYPES = typemax(TypeID)

Base.@kwdef struct Simulation
    name::String
    params::Union{Tuple, NamedTuple}

    # TODO: rename to agent_colls
    agents::Vector{AgentCollection} = Vector{AgentCollection}(undef, MAX_TYPES)
    agent_typeids::Dict{DataType, TypeID} = Dict{DataType, TypeID}()

    edges::Dict{DataType, EdgeCollection} = Dict{DataType, EdgeCollection}()

    globals::Dict{DataType, Globals} = Dict{DataType, Globals}()
end

function Simulation(name::String,
             params::Union{Tuple, NamedTuple})
    Simulation(name = name, params = params)
end

function all_agentcolls(sim)
    # TODO: Check if you could/should use genetors instead of comprehensions
    # also for the other cases
    [ sim.agents[i] for i in 1:length(sim.agent_typeids) ]
end

function some_agentcolls(sim, types)
    [ sim.agents[v] for (k, v) in sim.agent_typeids if k in types ]
end

function all_edgecolls(sim)
    values(sim.edges) |> collect
end

function some_edgecolls(sim, types)
    [ v for (k, v) in sim.edges if k in types ]
end

function finish_init!(sim::Simulation)
    foreach(finish_init!, all_agentcolls(sim))
    foreach(finish_init!, all_edgecolls(sim))
end 

######################################## Types

function typeid(sim, ::Type{T})::TypeID where {T <: AbstractAgent}
    sim.agent_typeids[T]
end

function add_agenttype!(sim::Simulation, ::Type{T}) where {T <: AbstractAgent} 
    # TODO: improve assertion error messages (for all adds)
    # TODO: check that the same type is not added twice (for all adds)
    @assert isbitstype(T)
    ids =sim.agent_typeids

    type_number = length(ids) + 1
    @assert type_number < typemax(TypeID) "Can not add new type, 
                                maximal number of types already registered"
    push!(ids, T => type_number)
    
    sim.agents[type_number] = BufferedAgentDict{T}()
    type_number
end

function add_edgetype!(sim::Simulation, ::Type{T}) where { T<: AbstractEdge } 
    @assert isbitstype(T)
    if fieldnames(T) == ()
        push!(sim.edges, T => BufferedEdgeDict{StatelessEdge{T}}())
    else
        push!(sim.edges, T => BufferedEdgeDict{Edge{T}}())
    end
end

function add_globalstatetype!(sim::Simulation, ::Type{T}) where {T}
    @assert isbitstype(T)
    push!(sim.globals, T => EmptyGlobal{GlobalState, T}())
end

function add_globalseriestype!(sim::Simulation, ::Type{T}) where {T}
    @assert isbitstype(T)
    push!(sim.globals, T => EmptyGlobal{GlobalSeries, T}())
end

######################################## Agents


function add_agents!(sim::Simulation, agent::T) where {T <: AbstractAgent}
    # TODO add a better error message if type is not registered
    typeid = sim.agent_typeids[T]
    coll = sim.agents[typeid]
    coll.id_counter = coll.id_counter + 1
    agentid = agent_id(typeid, coll.id_counter)
    sim.agents[typeid][agentid] = agent
    agentid
end

function add_agents!(sim::Simulation, agents::Vector{T}) where { T <: AbstractAgent }
    [ add_agents!(sim, a) for a in agents ]
end

function add_agents!(sim::Simulation, agents::T...) where {T <: AbstractAgent}
    [ add_agents!(sim, a) for a in agents ]
end

add_agents!(f::Function) = sim -> add_agents!(sim, f(sim))


show_agents(sim, ::Type{T}) where {T} =
    show(stdout, MIME"text/plain"(), sim.agents[typeid(sim, T)])

######################################## Edges

function add_edge!(sim::Simulation, edge::T) where {T <: AbstractCompleteEdge}
    push!(sim.edges[statetype(edge)], edge)
    nothing
end

function add_edge!(sim::Simulation, from::AgentID, to::AgentID,
            ::Type{T}) where {T<:AbstractEdge}
    add_edge!(sim, StatelessEdge{T}(from, to))
end

function add_edge!(sim::Simulation, from::AgentID, to::AgentID,
            state::T) where {T<:AbstractEdge}
    # This if is optimized away by the compiler for the concrete types
    # so the comfort to allow also T() for StatelessEdges does not
    # come with a performance peanalty
    if fieldnames(T) == ()
        add_edge!(sim, StatelessEdge{T}(from, to))
    else
        add_edge!(sim, Edge{T}(from, to, state))
    end
end


show_network(sim, ::Type{T}) where {T} =
    show(stdout, MIME"text/plain"(), sim.edges[T])

# function get_edges(sim, T::DataType, agent::AgentID) 
#     sim.edges[T][agent]
# end

######################################## Transition

# TODO: Alternative for fn_access_edges, check performance compared to
# fn_access_edges 

# struct AgentEdges
#     sim::Simulation
#     id::AgentID
# end

# function Base.getindex(ae::AgentEdges, key)
#     get(read_container(ae.sim.edges[key]),
#         ae.id,
#         Vector{statetype(ae.sim.edges[key])}())
# end

fn_access_edges(id) = (sim, edgetype) ->
    get(read_container(sim.edges[edgetype]),
        id,
        Vector{statetype(sim.edges[edgetype])}())
    
agent_from(sim::Simulation, edge::AbstractCompleteEdge) =
    sim.agents[type_nr(edge.from)][edge.from]

param(sim::Simulation, name) = getproperty(sim.params, name)
# agent_from(sim) = edge::AbstractEdge ->
#     sim.agents[type_nr(edge.from)][edge.from]


# func(agent, id, networks, sim)
# where edges is Dict{Type, Edges} for all networks
# must calls add_agents for new agents (assuming agents without edges are
# useless) and also add_edge! (TODO: add an add_edges! method)
# returns agent, which gets the same id as agent input

function maybeadd(coll::AgentCollection{T},
           id::AgentID,
           agent::T) where {T <: AbstractAgent}
    # the coll[id] writes into another container then
    # we use for the iteration
    coll[id] = agent
    nothing
end

function maybeadd(::AgentCollection{T},
           ::AgentID,
           ::Nothing) where {T <: AbstractAgent}
    nothing
end

function apply_transition!(sim,
                    func,
                    compute::Vector{DataType};
                    rebuild = Vector{DataType}())
    foreach(prepare_write!, some_agentcolls(sim, compute))
    foreach(prepare_write!, some_agentcolls(sim, rebuild))
    foreach(prepare_write!, some_edgecolls(sim, rebuild))

    for coll in some_agentcolls(sim, compute)
        for (id,state) in coll
            maybeadd(coll, id, func(state, id, fn_access_edges(id), sim))
        end 
    end

    foreach(finish_write!, some_agentcolls(sim, compute))
    foreach(finish_write!, some_agentcolls(sim, rebuild))
    foreach(finish_write!, some_edgecolls(sim, rebuild))
end

function apply_transition(sim,
                   func,
                   compute::Vector{DataType};
                   kwargs...)
    newsim = deepcopy(sim)
    apply_transition!(newsim, func, compute; kwargs...)
    newsim
end

function apply_transition_params(sim, compute::DataType)
    coll = some_agentcolls(sim, [ compute ])[1]
    (id, state) = coll |> first
    (state, id, fn_access_edges(id))
end


######################################## aggregate

function aggregate(sim::Simulation, ::Type{T}, f, op;
            kwargs...) where {T<:AbstractAgent}
    agents = sim.agents[sim.agent_typeids[T]] |>
        read_container |>
        values
    mapreduce(f, op, agents; kwargs...)
end

function aggregate(sim::Simulation, ::Type{T}, f, op;
            kwargs...) where {T}
    edges = sim.edges[T] |>
        read_container |>
        values |>
        Iterators.flatten |>
        collect |>
        states
    mapreduce(f, op, edges; kwargs...)
end
