export AbstractEdge, StatelessEdge, Edge, CirularEdge

abstract type AbstractEdge end
# All AbstractEdges must have a to field (but from is optional)

struct StatelessEdge{T} <: AbstractEdge
    from::AgentID
    to::AgentID
end

statetype(::StatelessEdge{T}) where {T} = T

struct Edge{T} <: AbstractEdge
    from::AgentID
    to::AgentID
    state::T
end

statetype(::Edge{T}) where {T} = T

struct CircularEdge{T} <: AbstractEdge
    to::AgentID
    state::T
end

statetype(::CircularEdge{T}) where {T} = T
