export Edge, edgestates
export add_edge!, add_edges!, edges_to
export num_neighbors, has_neighbor
export neighborstates, neighborstates_flexible
export neighborids

"""
    struct Edge{T} 
        from::AgentID
        state::T
    end

An edge between to agents with (optionally) additional state. T can be
also a struct without any field.

The AgentID of the agent at the head of the edge is not a field of
`Edge` itself, since this information is already part of the
containers in which the edges are stored.

See also [`add_edgetype!`](@ref)
"""
struct Edge{T} 
    from::AgentID
    state::T
end

"""
    add_edge!(sim, to::AgentID, edge::Edge{T}) 

Add a single edge to the simulation `sim`. The edges is directed from
the agent with ID `edge.from` to the agent with ID `to`.

    add_edge!(sim, from::AgentID, to::AgentID, state::T) 

Add a single edge to the simulation `sim`. The edge is directed
from the agent with ID `from` to the agent with ID `to` and has the
state `state`. 

T must have been previously registered in the simulation by calling
[`add_edgetype!`](@ref).

See also [`Edge`](@ref) [`add_edgetype!`](@ref) and [`add_edges!`](@ref)
"""
function add_edge! end


"""
    add_edges!(sim, to::AgentID, edges)

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
    edges_to(sim, id::AgentID, ::Val{T}) 

If T has the character :SingleEdge
    Returns the incoming edge for agent `id` of network `T`.
else
    Returns all incoming edges for agent `id` of network `T`.

edges_to is not defined if T has the character :IgnoreFrom or :Stateless

Should only be used inside a transition function and only for the ID specified
as a transition function parameter. Calling edges_to outside a transition function
or with other IDs may result in undefined behavior.

See also [`apply_transition!`](@ref), [`neighborstates`](@ref),
[`edgestates`](@ref), [`num_neighbors`](@ref), [`has_neighbor`](@ref)
and [`neighborids`](@ref)
"""
function edges_to end

"""
    neighborids(sim, id::AgentID, ::Val{T}) -> Vector{AgentID}

If T has the character :SingleEdge
    Returns the id of the agent on the incoming side agent `id` of network `T`.
else
    Returns all incoming edges for agent `id` of network `T`.


Returns all IDs of the agents at the tail of the edges in `v`. 

Used mainly in combination with [`edges_to`](@ref).
"""
function neighborsids end

#function edges_to(sim, to, ::Val) @assert false "Did you forgot the Val() for the type?" end

# """
#     neighborids(v::Vector{Edge}) -> Vector{AgentID}

# Returns all IDs of the agents at the tail of the edges in `v`. 

# Used mainly in combination with [`edges_to`](@ref).

# See also [`neighborstates`](@ref) 
# """
# neighborids(v::Vector{Edge{T}}) where T = map(e -> e.from, v)

# # TODO write tests, update doc, move to factory?

# # function neighborids(sim, to::AgentID, type)
# #     neighborids(edges_to(sim, to, type))
# # end


# """
#     edgestates(v::Vector{Edge{T}}) -> Vector{T}

# Return all states from a vector of edges. 

# Used mainly in combination with [`edges_to`](@ref).
# """
# edgestates(v::Vector{Edge{T}}) where T = map(e -> e.state, v)

"""
    neighborstates(sim::Simulation, id::AgentID, edgetype::T) -> Vector{Agent}

Returns the state of all incoming neighbors of agent `id` for the network `T`.

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
[`neighborids`](@ref)
"""
function neighborstates end

neighborstates(sim, id::AgentID, edgetype::Val, agenttype::Val) =
    map(id -> agentstate(sim, id, agenttype), neighborids(sim, id, edgetype))  

"""
TODO DOC
"""
function neighborstates_flexible end

function neighborstates_flexible(sim, id::AgentID, edgetype::Val)
    nids = neighborids(sim, id, edgetype)
    isnothing(nids) ? nothing : map(id -> agentstate_flexible(sim, id), nids)  
end

"""
TODO DOC
"""
function num_neighbors end

"""
TODO DOC
"""
function has_neighbor end
