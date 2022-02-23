export AbstractEdge, StatelessEdge, Edge, CirularEdge

abstract type AbstractEdge end
# All AbstractEdges must have a to field (but from is optional)

struct StatelessEdge{T} <: AbstractEdge
    from::AgentID
    to::AgentID
end

function statetype(::StatelessEdge{T}) where {T}
    T
end

struct Edge{T} <: AbstractEdge
    from::AgentID
    to::AgentID
    state::T
end

function statetype(::Edge{T}) where {T}
    T
end



struct CircularEdge{T} <: AbstractEdge
    to::AgentID
    state::T
end

function statetype(::CircularEdge{T}) where {T}
    T
end

