export Edge, edgestates
export add_edge!, add_edges!, edges_to
export num_neighbors, has_neighbor
export neighborstates, neighborstates_flexible
export neighborids, neighborids_flexible

"""
    struct Edge{T} 
        from::AgentID
        state::T
    end

An edge between to agents with (optionally) additional state. T can be
also a struct without any field.

To save memory (and reduce cache misses), the AgentID of the agent at
the head of the edge is not a field of `Edge` itself, since this
information is already part of the containers in which the edges are
stored.

See also [`add_edgetype!`](@ref)
"""
struct Edge{T} 
    from::AgentID
    state::T
end

#statetype(::Edge{T}) where T = T

"""
    add_edge!(sim::Simulation, to::AgentID, edge::Edge{T}) where {T <: EdgeState}

TODO DOC

Add a single edge to the simulation `sim`. The edges is directed from
the agent with ID `edge.from` to the agent with ID `to`.

T must have been previously registered in the simulation by calling
[`add_edgetype!`](@ref).

See also [`Edge`](@ref) [`add_edgetype!`](@ref) and [`add_edges!`](@ref)
"""
function add_edge! end

"""
    add_edge!(sim::Simulation, from::AgentID, to::AgentID, state::T) where {T<:EdgeState}

Add a single edge to the simulation `sim`. The edge is directed
from the agent with ID `from` to the agent with ID `to` and has the
state `state`. 

T must have been previously registered in the simulation by calling
[`add_edgetype!`](@ref).

In the case, that the EdgeState type T does not have any fields, it
is also possible to just use T as forth parameter instead of T.

See also [`Edge`](@ref) [`add_edgetype!`](@ref) and [`add_edges!`](@ref)
"""
# function add_edge!(sim, from::AgentID, to::AgentID, ::Type{T}) where T
#     add_edge!(sim, to, Edge{T}(from, T()))
# end

# function add_edge!(sim, from::AgentID, to::AgentID, state::T) where T
#     add_edge!(sim, to, Edge{T}(from, state))
# end

"""
    add_edges!(sim::Simulation, to::AgentID, edges)

Add multiple `edges` at once to the simulation `sim`, with all edges
are directed to `to`.

`edges` can be any iterable set of agents, or an arbitrary number of
edges as arguments. 

T must have been previously registered in the simulation by calling
[`add_edgetype!`](@ref).

See also [`Edge`](@ref) [`add_edgetype!`](@ref) and [`add_edge!`](@ref)
"""
function add_edges!(sim, to::AgentID, edges::Vector{Edge{T}}) where T
    [ add_edge!(sim, to, e) for e in edges ]
    nothing
end

function add_edges!(sim, to::AgentID, edges::Edge{T}...) where T
    [ add_edge!(sim, to, e) for e in edges ]
    nothing
end

"""
    edges_to(sim::Simulation, id::AgentID, edgetype::Val{T}) -> Vector{Edges{T}}

Returns all incoming edges for agent `id` of network `T`.

Should only be used inside a transition function and only for the ID specified
as a transition function parameter. Calling edges_to outside a transition function
or with other IDs may result in undefined behavior.

See also [`apply_transition!`](@ref), [`neighborstates`](@ref),
[`edgestates`](@ref) and [`neighbors`](@ref)
"""
#function edges_to(sim, to, ::Val) @assert false "Did you forgot the Val() for the type?" end
function edges_to end

"""
    neighborids(v::Vector{Edge{T}}) where {T<:EdgeState} -> Vector{AgentID}

Returns all IDs of the agents at the tail of the edges in `v`. 

Used mainly in combination with [`edges_to`](@ref).

See also [`neighborstates`](@ref) 
"""
neighbors(v::Vector{Edge{T}}) where T = map(e -> e.from, v)

# TODO write tests, update doc, move to factory?

function neighbors(sim, to::AgentID, type)
    neighbors(edges_to(sim, to, type))
end


"""
    edgestates(v::Vector{Edge{T}}) where {T<:EdgeState} -> Vector{EdgeState}

Return all states from a vector of edges. 

Used mainly in combination with [`edges_to`](@ref).
"""
edgestates(v::Vector{Edge{T}}) where T = map(e -> e.state, v)

"""
    neighborstates(sim::Simulation, id::AgentID, edgetype::T) -> Vector{Agent}

Returns all incoming neighbors of agent `id` for the network `T`.

Should only be used inside a transition function and only for the ID specified
as a transition function parameter. Calling agents_to outside a transition function
or with other IDs may result in undefined behavior.

In a parallel run, this function can trigger communication between
processes. In the case that the state of ALL agents is not needed in
the transition function, the performance can be likely increased by
using [`edges_to`](@ref) instead and calling [`agentstate`](@ref) only
for the agents whose state is actually used in the transition
function.

See also [`apply_transition!`](@ref), [`edgestates`](@ref) and
[`neighbors`](@ref)
"""
neighborstates(sim, id::AgentID, edgetype::Val, agenttype::Val) =
    map(e -> agentstate(sim, e.from, agenttype), edges_to(sim, id, edgetype))  

# TODO DOC
neighborstates_flexible(sim, id::AgentID, edgetype::Val) =
    map(e -> agentstate_flexible(sim, e.from), edges_to(sim, id, edgetype))  


# TODO DOC
function num_neighbors end
