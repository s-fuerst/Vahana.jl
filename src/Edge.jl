export AbstractEdge, StatelessEdge, Edge, CirularEdge

abstract type AbstractEdge end
# All AbstractEdges must have a to field (but from is optional)

struct StatelessEdge <: AbstractEdge
    from::AgentID
    to::AgentID
end

function Base.show(io::IO, mime::MIME"text/plain", edge::StatelessEdge)
    show(io, mime, edge.from)
    print(" -> ")
    show(io, mime, edge.to)
end 


struct Edge{T} <: AbstractEdge
    from::AgentID
    to::AgentID
    state::T
end

function Base.show(io::IO, mime::MIME"text/plain", edge::Edge{T}) where { T }
    show(io, mime, edge.from)
    print(io, " -> ")
    show(io, mime, edge.to)
    print(io, ": ")
    show(io, mime, edge.state)
end 


struct CirularEdge{T} <: AbstractEdge
    to::AgentID
    state::T
end

