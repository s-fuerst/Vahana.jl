export AbstractEdge, Edge, states, neighbors

"""
    abstract type AbstractEdge

The different network/edges types must be concrete subtypes of
~AbstractEdge~. These structs define the additional state of an
edge, but not the IDs from the adjacent vertices/agents.

So a more accurate name would actually be AbstractEdgeState.

See also [`Edge`](@ref) and [`add_edgetype!`](@ref)
"""
abstract type AbstractEdge end

"""
    struct Edge{T <: AbstractEdge} 

An edge between to agents with (optionally) additional state. T can be
also a struct without any field.

See also [`AbstractEdge`](@ref) and [`add_edgetype!`](@ref)
"""
struct Edge{T <: AbstractEdge} 
    from::AgentID
    to::AgentID
    state::T
end

statetype(::Edge{T}) where {T} = T

"""
    neighbors(v::Vector{Edge{T}}) where {T<:AbstractEdge} -> Vector{AgentID}

Returns all IDs of the agents at the tail of the edges in `v`. 

Used mainly in combination with [`edges_to`](@ref).

See also [`edges_to`](@ref) 
"""
neighbors(v::Vector{Edge{T}}) where {T<:AbstractEdge} = map(e -> e.from, v)


"""
    states(v::Vector{Edge{T}}) where {T<:AbstractEdge} -> Vector{AbstractEdge}

Return all states from a vector of edges. 

Used mainly in combination with [`edges_to`](@ref).

See also [`edges_to`](@ref) 
"""
states(v::Vector{Edge{T}}) where {T<:AbstractEdge} = map(e -> e.state, v)

# struct CircularEdge{T <: AbstractEdge} <: AbstractCompleteEdge
#     to::AgentID
#     state::T
# end

# statetype(::CircularEdge{T}) where {T} = T
