export AbstractEdge, Edge, CirularEdge

abstract type AbstractEdge end

struct StatelessEdge <: AbstractEdge
    from::T_AgentID
    to::T_AgentID
end

struct Edge{T} <: AbstractEdge
    from::T_AgentID
    to::T_AgentID
    state::T
end

struct CirularEdge{T} <: AbstractEdge
    agent::T_AgentID
    state::T
end

