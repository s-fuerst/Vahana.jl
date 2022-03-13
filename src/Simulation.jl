export Simulation
export add_agenttype!, add_edgetype!
export add_globalstatetype!, add_globalseriestype!
export add_agent!, add_agents!, add_edge!, add_edges!
export finish_init!
export typeid
export apply_transition, apply_transition!, apply_transition_params
export agentstate, agentstate_from, param
export edges_to
export aggregate
export show_agents, show_network

const MAX_TYPES = typemax(TypeID)

"""
    struct Simulation

The internal structure of a simulation. Model developers should not
access these fields directly.
"""
Base.@kwdef struct Simulation
    name::String
    params::Union{Tuple, NamedTuple}

    # TODO: rename to agent_colls
    agents::Vector{AgentCollection} = Vector{AgentCollection}(undef, MAX_TYPES)
    agent_typeids::Dict{DataType, TypeID} = Dict{DataType, TypeID}()

    edges::Dict{DataType, EdgeCollection} = Dict{DataType, EdgeCollection}()

    globals::Dict{DataType, Globals} = Dict{DataType, Globals}()
end

"""
    Simulation(name::String, params::Union{Tuple, NamedTuple})

Create a new simulation object, which stores the complete state of a simulation.

`name` is used as meta-information about the simulation and has no
effect on the dynamics, since `name` is not accessible in the
transition functions. The `param`eter`s` of the simulation can be
accessed via the [`param`](@ref) function. (Hint: If a NamedTupe has
only one element, it must have a ',' after the value,
e.g. Simulation("Name", (steps = 100,)) )

The simulation starts in an uninitialized state. After registering all
types of the simulation and adding the agents and edges for the
initial state, it is necessary to call [`finish_init!`](@ref) before
applying a transition function for the first time.

See also [`add_agenttype!`](@ref), [`add_edgetype!`](@ref), 
[`add_globalstatetype!`](@ref), [`add_globalseriestype!`](@ref), 
[`add_agent!`](@ref), [`add_agents!`](@ref), [`add_edge!`](@ref), 
[`add_edges!`](@ref) and [`finish_init!`](@ref)
"""
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
    foreach(finishinit!, all_agentcolls(sim))
    foreach(finishinit!, all_edgecolls(sim))
    sim
end 

######################################## Types

function typeid(sim, ::Type{T})::TypeID where {T <: AbstractAgent}
    sim.agent_typeids[T]
end

"""
    add_agenttype!(sim::Simulation, ::Type{T}) where {T <: AbstractAgent}

Register an additional agent type to `sim`. 

An agent type is an struct that define the state for agents of type `T`.
These structs must be a subtype of `AbstractAgent`, and also bits
types, meaning the type is immutable and contains only primitive types
and other bits types.

Can only be called before [`finish_init!`](@ref)

See also [`add_agent!`](@ref) and [`add_agents!`](@ref) 
"""
function add_agenttype!(sim::Simulation, ::Type{T}) where {T <: AbstractAgent} 
    # TODO: improve assertion error messages (for all adds)
    # TODO: check that the same type is not added twice (for all adds)
    # TODO: check that finish_init! is not called
    @assert isbitstype(T)
    ids = sim.agent_typeids

    type_number = length(ids) + 1
    @assert type_number < typemax(TypeID) "Can not add new type, 
                                maximal number of types already registered"
    push!(ids, T => type_number)
    
    sim.agents[type_number] = BufferedAgentDict{T}()
    type_number
end

"""
    add_edgetype!(sim::Simulation, ::Type{T}) where {T <: AbstractEdge}

Register an additional edge type to `sim`. 

An edge type is an struct that define the state for edges of type `T`.
These structs must be a subtype of `AbstractEdge`, and also bits
types, meaning the type is immutable and contains only primitive types
and other bits types.

Can only be called before [`finish_init!`](@ref)

See also [`add_edge!`](@ref) and [`add_edges!`](@ref) 
"""
function add_edgetype!(sim::Simulation, ::Type{T}) where {T <: AbstractEdge}
    @assert isbitstype(T)
    push!(sim.edges, T => BufferedEdgeDict{Edge{T}}())
end

"""
    add_globalstatetype!(sim::Simulation, ::Type{T}) where {T <: AbstractGlobal}

Registration of an additional global type for `sim` that stores a single value.

The values of global types are accessible to all agents. In terms of
the graph structure of Vahana, this can be interpreted as there being
an implicit edge from a single node per global type to all agents.

For global types registered with add_globalstatetype! only one
instance of this type is stored in the simulation. This is often
useful for a global state used by the simulation itself, e.g. the GDP
of an economy. The current value of global types can be updated via
[`add_globalstate`](@ref) and retreived via [`current_state`](@ref).

Global types can also used for (time) series, see [`add_globalseriestype!`](@ref).

These global types must be a subtype of `AbstractGlobal`, and also bits
types, meaning the type is immutable and contains only primitive types
and other bits types.

Can only be called before [`finish_init!`](@ref)

See also [`add_globalseriestype!`](@ref), [`add_globalstate!`](@ref)
and [`current_state`](@ref)

"""
function add_globalstatetype!(sim::Simulation, ::Type{T}) where {T <: AbstractGlobal}
    @assert isbitstype(T)
    push!(sim.globals, T => EmptyGlobal{GlobalState, T}())
end

"""
    add_globalseriestype!(sim::Simulation, ::Type{T}) where {T <: AbstractGlobal}

Register an additional global type to `sim` that stores a series of values.

The values of global types are accessible to all agents. In terms of
the graph structure of Vahana, this can be interpreted as there being
an implicit edge from a single node per global type to all agents.

For global types registered with add_globalseriestype!, all values
added via [`add_globalstate!`](@ref) are stored in a vector of type T in
the simulation. The most recently added value can be retrieved via
[`current_state`](@ref), and the entire vector via
[`all_states`](@ref).

If only the last value is needed, [`add_globalstatetype!`](@ref) can
also be used to register the type.

These global types must be a subtype of `AbstractGlobal`, and also bits
types, meaning the type is immutable and contains only primitive types
and other bits types.

Can only be called before [`finish_init!`](@ref)

See also [`add_globalstatetype!`](@ref), [`add_globalstate!`](@ref),
[`aggregate`](@ref) [`all_states`](@ref) and [`current_state`](@ref)

"""
function add_globalseriestype!(sim::Simulation, ::Type{T}) where {T <: AbstractGlobal}
    @assert isbitstype(T)
    push!(sim.globals, T => EmptyGlobal{GlobalSeries, T}())
end

######################################## Agents

"""
    add_agent!(sim::Simulation, agent::T) -> AgentID

Add a single agent of type T to the simulation `sim`.

T must have been previously registered in the simulation by calling
[`add_agenttype!`](@ref).

`add_agent!` returns a new AgentID, which can be used to create edges
from or to this agent. Do not use the ID for other purposes, they are
not guaranteed to be stable.

See also [`add_agents!`](@ref), [`add_agenttype!`](@ref),
[`add_edge!`](@ref) and [`add_edges!`](@ref)

"""
function add_agent!(sim::Simulation, agent::T) where {T <: AbstractAgent}
    # TODO add a better error message if type is not registered
    typeid = sim.agent_typeids[T]
    coll = sim.agents[typeid]
    coll.id_counter = coll.id_counter + 1
    agentid = agent_id(typeid, coll.id_counter)
    sim.agents[typeid][agentid] = agent
    agentid
end

"""
    add_agents!(sim::Simulation, agents) -> Vector{AgentID}

Add multiple agents at once to the simulation `sim`.

`agents` can be any iterable set of agents, or an arbitrary number of
agents as arguments. 

The types of the agents must have been previously registered in the
simulation by calling [`add_agenttype!`](@ref).

`add_agents!` returns a vector of AgentIDs, which can be used
to create edges from or to this agents. Do not use the ID for other
purposes, they are not guaranteed to be stable.

See also [`add_agent!`](@ref), [`add_agenttype!`](@ref),
[`add_edge!`](@ref) and [`add_edges!`](@ref)

"""
function add_agents!(sim::Simulation, agents) 
    [ add_agent!(sim, a) for a in agents ]
end

function add_agents!(sim::Simulation, agents::T...) where {T <: AbstractAgent}
    [ add_agent!(sim, a) for a in agents ]
end

"""
    show_agents(sim, ::Type{T}) where {T <: AbstractAgent}

Print (some of) the agents of the type T.

In a parallized simulation, only the agents that are in the partition
of the graph associated with the function calling process are printed.

"""
show_agents(sim, ::Type{T}) where {T <: AbstractAgent} =
    show(stdout, MIME"text/plain"(), sim.agents[typeid(sim, T)])

######################################## Edges

"""
    add_edge!(sim::Simulation, edge::Edge{T}) where {T <: AbstractEdge}

Add a single edge to the simulation `sim`. HIER AUCH WEITERMACHEN

See also [`add_edgetype!`](@ref) and [`add_edges!`](@ref)
"""
function add_edge!(sim::Simulation, edge::Edge{T}) where {T <: AbstractEdge }
    push!(sim.edges[statetype(edge)], edge)
    nothing
end

"""
    add_edge!(sim::Simulation, from::AgentID, to::AgentID, state::T) where {T<:AbstractEdge}

Add a single edge to the simulation `sim`.

HIER WEITERMACHEN
"""
function add_edge!(sim::Simulation, from::AgentID, to::AgentID,
            ::Type{T}) where {T<:AbstractEdge}
    add_edge!(sim, Edge{T}(from, to, T()))
end

function add_edge!(sim::Simulation, from::AgentID, to::AgentID,
            state::T) where {T<:AbstractEdge}
    add_edge!(sim, Edge{T}(from, to, state))
end

function add_edges!(sim::Simulation, edges::Vector{Edge{T}}) where {T <: AbstractEdge}
    [ add_edge!(sim, e) for e in edges ]
    nothing
end

function add_edges!(sim::Simulation, edges::Edge{T}...) where {T <: AbstractEdge}
    [ add_edge!(sim, e) for e in edges ]
    nothing
end

show_network(sim, ::Type{T}) where {T} =
    show(stdout, MIME"text/plain"(), sim.edges[T])

######################################## Transition

"""
    edges_to(sim::Simulation, id::AgentID, edgetype::T) -> Vector{Edges{T}}

Returns all incoming edges for agent `id` of network `T`.

Should only be used inside a transition function and only for the ID specified
as a transition function parameter. Calling edges_to outside a transition function
or with other IDs may result in undefined behavior.

See also [`apply_transition!`](@ref), [`states`](@ref) and
[`neighbors`](@ref)
"""
edges_to(sim::Simulation, id::AgentID, edgetype) = 
    get(read_container(sim.edges[edgetype]),
        id,
        Vector{statetype(sim.edges[edgetype])}())

"""
"""
agentstate_from(sim::Simulation, edge::Edge{T}) where {T<:AbstractEdge} =
    sim.agents[type_nr(edge.from)][edge.from]

"""
"""
agentstate(sim::Simulation, id::AgentID) =
    sim.agents[type_nr(id)][id]

"""
"""
param(sim::Simulation, name) = getproperty(sim.params, name)


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

"""
"""
function apply_transition!(sim,
                    func,
                    compute::Vector,
                    networks::Vector,
                    rebuild::Vector)
    writeable = [ compute; rebuild ]
    
    foreach(prepare_write!, some_agentcolls(sim, writeable))
    foreach(prepare_write!, some_edgecolls(sim, writeable))

    for coll in some_agentcolls(sim, compute)
        for (id, state) in coll
            maybeadd(coll, id, func(state, id, sim))
        end 
    end

    foreach(finish_write!, some_agentcolls(sim, writeable))
    foreach(finish_write!, some_edgecolls(sim, writeable))
    sim

    add_agenttype!
end

"""
"""
function apply_transition(sim,
                   func,
                   compute::Vector,
                   networks::Vector,
                   rebuild::Vector) 
    newsim = deepcopy(sim)
    apply_transition!(newsim, func, compute, networks, rebuild)
    newsim
end

function apply_transition_params(sim, compute::DataType, networks::Vector)
    coll = some_agentcolls(sim, [ compute ])[1]
    (id, state) = coll |> first
    (state, id, fn_access_edges(networks, id))
end


######################################## aggregate

"""
"""
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
