export Edge, edgestates
export add_edge!, add_edges!, edges
export num_edges, has_edge
export neighborstates, neighborstates_flexible
export neighborids

# For many function declared in this file, the concrete impementation depends
# on the hints of the edge type and the function itself is build via
# the construct_edge_methods function. The declaration here
# are only used for the documentation, that they come with parameters
# improves the experience with tools like LSP.

"""
    struct Edge{T} 
        from::AgentID
        state::T
    end

An edge between to agents with (optionally) additional state. T can be
also a struct without any field.

The AgentID of the agent at the target of the edge is not a field of
`Edge` itself, since this information is already part of the
containers in which the edges are stored.

See also [`register_edgetype!`](@ref)
"""
struct Edge{T} 
    from::AgentID
    state::T
end

"""
    add_edge!(sim, to::AgentID, edge::Edge{T}) 

Add a single edge to the simulation `sim`. The edges is directed from
the agent with ID `edge.from` (the source) to the agent with ID `to`
(the target).

    add_edge!(sim, from::AgentID, to::AgentID, state::T) 

Add a single edge to the simulation `sim`. The edge is directed from
the agent with ID `from` (the source) to the agent with ID `to` (the
target) and has the state `state`.

`T` must have been previously registered in the simulation by calling
[`register_edgetype!`](@ref).

See also [`Edge`](@ref) [`register_edgetype!`](@ref) and [`add_edges!`](@ref)
"""
function add_edge!(::Simulation, to::AgentID, edge::Edge)  end


function add_edge!(::Simulation, from::AgentID, to::AgentID, state::Edge) 
    error("""

`add_edge!(sim, from, to, state)` can not be called with a complete edge as state.
If `edge.from == from`, you can use `add_edge!(sim, to, edge)` instead, otherwise you
must extract the state of the edge by adding `.state` to your current `state` argument.
""")
end

function add_edge!(::Simulation, from::AgentID, to::AgentID, state::T) where T
    error("add_edge! can not be called with a state of type $T")
end

"""
    add_edges!(sim, to::AgentID, edges)

Add multiple `edges` at once to the simulation `sim`, with all edges
are directed to `to`.

`edges` can be any iterable set of agents, or an arbitrary number of
edges as arguments. 

See also [`Edge`](@ref) [`register_edgetype!`](@ref) and [`add_edge!`](@ref)
"""
function add_edges!(sim::Simulation, to::AgentID, edges::Vector{Edge{T}}) where T
    for e in edges
        add_edge!(sim, to, e) 
    end
    nothing
end

function add_edges!(::Simulation, ::AgentID)
    nothing
end

function add_edges!(sim::Simulation, to::AgentID, edges::Edge{T}...) where T
    for e in edges
        add_edge!(sim, to, e)
    end
    nothing
end

"""
    edges(sim, id::AgentID, ::Type{E}) 

Returns the edge of type `E` with agent `id` as target if `E` has
the hint :SingleEdge, or a vector of these edges otherwise.

If there is no edge with agent `id` as target, `edges` returns `nothing`.

edges is not defined if `E` has the hint :IgnoreFrom or :Stateless.

See also [`apply!`](@ref), [`checked`](@ref), [`neighborids`](@ref),
[`edgestates`](@ref), [`num_edges`](@ref), [`has_edge`](@ref)
and [`neighborstates`](@ref)
"""
function edges(::Simulation, id::AgentID, edgetype::Type) end

"""
    neighborids(sim, id::AgentID, ::Type{E}) 

Returns the ID of the agent on the source of the edge of type `E`
with agent `id` as target if `E` has the hint :SingleEdge, or otherwise
a vector of the IDs of the agents on the source side of those edges.

If there is no edge with agent `id` as target, `neighborids` returns `nothing`.

`neighborids` is not defined if `E` has the hint :IgnoreFrom.

See also [`apply!`](@ref), [`checked`](@ref), [`edges`](@ref),
[`edgestates`](@ref), [`num_edges`](@ref), [`has_edge`](@ref)
and [`neighborstates`](@ref)
"""
function neighborids(::Simulation, id::AgentID, edgetype::Type) end

"""
    edgestates(sim, id::AgentID, ::Type{E}) 

Returns the state of the edge of type `E` with agent `id` as target if `E` has
the hint :SingleEdge, or a vector of these states otherwise.

If there is no edge with agent `id` as target, `edgestates` returns `nothing`.

`edgestates` is not defined if `E` has the hint :Stateless.

See also [`apply!`](@ref), [`checked`](@ref), [`edges`](@ref),
[`neighborids`](@ref), [`num_edges`](@ref), [`has_edge`](@ref)
and [`neighborstates`](@ref)
"""
function edgestates(::Simulation, id::AgentID, edgetype::Type) end

"""
    neighborstates(sim::Simulation, id::AgentID, ::Type{E}, ::Type{A}) 

Returns the state of the agent with type `A` on the source of the
edge of type `E` with agent `id` as target if `E` has the hint
:SingleEdge, or a vector of these agent states otherwise.

If there is no edge with agent `id` as target, `neighborstates`
returns `nothing`.

When the agents on the source side of the edges can have different
types, and it is impossible to determine the Type{A} you can use
[`neighborstates_flexible`](@ref) instead.

`neighborstates` is not defined if T has the hint :IgnoreFrom 

See also [`apply!`](@ref), [`checked`](@ref), [`edges`](@ref),
[`neighborids`](@ref), [`num_edges`](@ref), [`has_edge`](@ref)
and [`edgestates`](@ref)
"""
function neighborstates(::Simulation, id::AgentID, edgetype::Type, agenttype::Type) end


"""
    neighborstates_flexible(sim::Simulation, id::AgentID, ::Type{E}) 

Returns the state of the agent on the source of the
edge of type `E` with agent `id` as target if `E` has the hint
:SingleEdge, or a vector of these agent states otherwise.

If there is no edge with agent `id` as target, `neighborstates_flexible`
returns `nothing`.

`neighborstates_flexible` is the type instable version of
[`neighborstates`](@ref) and should be only used in the case that the
type of agent can not be determined.

`neighborstates_flexible` is not defined if T has the hint :IgnoreFrom.

See also [`apply!`](@ref), [`checked`](@ref), [`edges`](@ref),
[`neighborids`](@ref), [`num_edges`](@ref), [`has_edge`](@ref)
and [`edgestates`](@ref)
"""
function neighborstates_flexible(sim::Simulation, id::AgentID, edgetype::Type)
    nids = neighborids(sim, id, edgetype)
    isnothing(nids) ? nothing : map(id -> agentstate_flexible(sim, id), nids)  
end

"""
    num_edges(sim, id::AgentID, ::Type{E}) 

Returns the number of edges of type `E` with agent `id` as target.

`num_edges` is not defined if T has the hint :SingleEdge

See also [`apply!`](@ref), [`edges`](@ref),
[`neighborids`](@ref), [`edgestates`](@ref), [`has_edge`](@ref)
and [`neighborstates`](@ref)
"""
function num_edges(::Simulation, id::AgentID, edgetype::Type) end

"""
    has_edge(sim, id::AgentID, ::Type{E}) 

Returns true if there is at least one edge of type `E` with agent `id` as
target.

`has_edge` is not defined if T has the :SingleEdge and :SingleType
hints, with the exception that it has also the :IgnoreFrom and
:Stateless hints.

See also [`apply!`](@ref), [`edges`](@ref),
[`neighborids`](@ref), [`edgestates`](@ref), [`num_edges`](@ref)
and [`neighborstates`](@ref)
"""
function has_edge(::Simulation, id::AgentID, edgetype::Type) end

"""
    num_edges(sim, ::Type{T}, sum_ranks=false)

If `all_ranks` is `true` this function retrieves the number of edges of type `T`
of the simulation `sim`. When it is set to `false`, the function will only return the
number of edges of type `T` managed by the process.
"""
function num_edges(sim, t::Type{T}, sum_ranks = true; write = nothing) where T
    local_num = if write === nothing
        _num_edges(sim, t, ! sim.initialized)
    else
        _num_edges(sim, t, write)
    end
    if sum_ranks
        MPI.Allreduce(local_num, +, MPI.COMM_WORLD)
    else
        local_num
    end
end
