export AbstractEdge, StatelessEdge, Edge, CirularEdge

abstract type AbstractEdge end
# All AbstractEdges must have a to field (but from is optional)

struct StatelessEdge <: AbstractEdge
    from::AgentID
    to::AgentID
end

struct Edge{T} <: AbstractEdge
    from::AgentID
    to::AgentID
    state::T
end

struct CirularEdge{T} <: AbstractEdge
    to::AgentID
    state::T
end

