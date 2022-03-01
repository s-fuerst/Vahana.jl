export AbstractEdge, StatelessEdge, Edge, CircularEdge, states

abstract type AbstractCompleteEdge end

abstract type AbstractEdge end
# All AbstractEdges must have a to field (but from is optional)

struct StatelessEdge{T <: AbstractEdge} <: AbstractCompleteEdge
    from::AgentID
    to::AgentID
end

statetype(::StatelessEdge{T}) where {T} = T

struct Edge{T <: AbstractEdge} <: AbstractCompleteEdge
    from::AgentID
    to::AgentID
    state::T
end

statetype(::Edge{T}) where {T} = T

states(v::Vector{Edge{T}}) where {T} = map(e -> e.state, v)

struct CircularEdge{T <: AbstractEdge} <: AbstractCompleteEdge
    to::AgentID
    state::T
end

statetype(::CircularEdge{T}) where {T} = T
