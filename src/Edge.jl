export EdgeState, Edge, states, neighbors

"""
    abstract type EdgeState

The different network/edges types must be concrete subtypes of
~EdgeState~. These structs define the additional state of an
edge, but not the IDs from the adjacent vertices/agents.

See also [`Edge`](@ref) and [`add_edgetype!`](@ref)
"""
abstract type EdgeState end

"""
    struct Edge{T <: EdgeState} 

An edge between to agents with (optionally) additional state. T can be
also a struct without any field.

To save memory (and reduce cache misses), the AgentID of the agent at
the head of the edge is not a field of `Edge` itself, since this
information is already part of the containers in which the edges are
stored.

See also [`EdgeState`](@ref) and [`add_edgetype!`](@ref)
"""
struct Edge{T <: EdgeState} 
    from::AgentID
    state::T
end

statetype(::Edge{T}) where {T} = T

"""
    neighbors(v::Vector{Edge{T}}) where {T<:EdgeState} -> Vector{AgentID}

Returns all IDs of the agents at the tail of the edges in `v`. 

Used mainly in combination with [`edges_to`](@ref).

See also [`edges_to`](@ref) 
"""
neighbors(v::Vector{Edge{T}}) where {T<:EdgeState} = map(e -> e.from, v)


"""
    states(v::Vector{Edge{T}}) where {T<:EdgeState} -> Vector{EdgeState}

Return all states from a vector of edges. 

Used mainly in combination with [`edges_to`](@ref).

See also [`edges_to`](@ref) 
"""
states(v::Vector{Edge{T}}) where {T<:EdgeState} = map(e -> e.state, v)

# struct CircularEdge{T <: EdgeState} <: AbstractCompleteEdge
#     to::AgentID
#     state::T
# end

# statetype(::CircularEdge{T}) where {T} = T
