export AbstractEdge, Edge, CirularEdge

abstract type AbstractEdge end

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
    agent::AgentID
    state::T
end

