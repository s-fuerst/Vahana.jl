export add_graph!
# export edges, edgetype, has_edge, has_vertex, inneighbors, ne, nv, outneighbors, vertices, is_directed
export vahanagraph, vahanasimplegraph, simplegraph

import Graphs:
    edges, edgetype, has_edge, has_vertex, inneighbors, ne, nv, outneighbors, vertices, is_directed

import Base:
    show, eltype

"""
    add_graph!(sim, graph, agent_constructor, edge_constructor) -> Vector{AgentID}

Adds a `graph` from the Graphs.jl package to `sim`, incl. all vertices
of `graph` as new agents.

`graph` must be a Graphs.Graph or a Graphs.DiGraph.

For each vertix of `graph` the `agent_constructor` function is called, with
the Graphs.vertix as argument. For each edge of `graph` the
`edge_constructor` function is called, with the Graphs.edge as argument.

The agent types of agents created by the `agent_constructor` must be
already registered via [`register_agenttype!`](@ref) and vis a vis the edge
type via [`register_edgetype!`](@ref).

!!! info

    add_graph! is only availabe after importing the Graphs.jl package

Returns a vector with the IDs of the created agents.
"""
function add_graph!(sim, graph, agent_constructor, edge_constructor) 
    agents = add_agents!(sim,
                         [ agent_constructor(v) for v in Graphs.vertices(graph) ])

    for e in Graphs.edges(graph)
        add_edge!(sim,
                  agents[Graphs.src(e)],
                  agents[Graphs.dst(e)],
                  edge_constructor(e))
        if !Graphs.is_directed(graph)
            add_edge!(sim,
                      agents[Graphs.dst(e)],
                      agents[Graphs.src(e)],
                      edge_constructor(e))
        end
    end

    agents
end

######################################## VahanaSimpleGraph


struct VahanaSimpleGraph <: Graphs.AbstractSimpleGraph{Integer}
    sim
    agenttypes::Vector{DataType}
    networks::Vector{DataType}
    g2v::Vector{AgentID}
    v2g::Dict{AgentID, Int64}
    # for the AbstractSimpleGraph interface
    vertices::UnitRange{Integer}
    fadjlist::Vector{Vector{Integer}}
    ne::Integer
end


function vahanasimplegraph(sim, agenttypes, networks)
    g2v = Vector{AgentID}()
    v2g = Dict{AgentID, Int64}()

    nv = 0
    for t in agenttypes
        tid = sim.typeinfos.nodes_type2id[t]
        for id in keys(_getread(sim, t))
            nv += 1
            aid = agent_id(tid, AgentNr(id))
            push!(g2v, aid)
            push!(v2g, aid => nv)
        end
    end

    fadjlist = [ Vector{Integer}() for _ in 1:nv ]

    ne = 0
    for t in networks
        for (to, e) in edges_iterator(_getread(sim, t))
            f = get(v2g, e.from, nothing)
            t = get(v2g, to, nothing)
            if f !== nothing && t !== nothing
                push!(fadjlist[f], t)
                ne += 1
            end
        end
    end

    VahanaSimpleGraph(sim, agenttypes, networks, g2v, v2g, 1:nv, fadjlist, ne)
end

### AbstractGraph interface for VahanaSimpleGraph
eltype(_::VahanaSimpleGraph) = Int64

edgetype(vg::VahanaSimpleGraph) = Graphs.SimpleEdge{eltype(vg)}

is_directed(::VahanaSimpleGraph) = true

is_directed(::Type{VahanaSimpleGraph}) = true

Base.show(io::IO, _::MIME"text/plain", vg::VahanaSimpleGraph) =
    println(io, "SimpleSubgraph of $(vg.sim.name) {$(vg.agenttypes), $(vg.networks)}  {$(nv(vg)), $(ne(vg))}")

######################################## VahanaGraph

struct VahanaGraph  <: Graphs.AbstractGraph{Int64}
    sim
    agenttypes::Vector{DataType}
    networks::Vector{DataType}
    g2v::Vector{AgentID}
    v2g::Dict{AgentID, Int64}
    edges::Vector{Graphs.Edge}
end


function vahanagraph(sim, agenttypes, networks)
    g2v = Vector{AgentID}()
    v2g = Dict{AgentID, Int64}()

    nv = 0
    for t in agenttypes
        tid = sim.typeinfos.nodes_type2id[t]
        for id in keys(_getread(sim, t))
            nv += 1
            aid = agent_id(tid, AgentNr(id))
            push!(g2v, aid)
            push!(v2g, aid => nv)
        end
    end

    edges = Vector{Graphs.Edge}()
    for t in networks
        for (to, e) in edges_iterator(_getread(sim, t))
            f = get(v2g, e.from, nothing)
            t = get(v2g, to, nothing)
            if f !== nothing && t !== nothing
                push!(edges, Graphs.Edge(f, t))
            end
        end
    end

    VahanaGraph(sim, agenttypes, networks, g2v, v2g, edges)
end


### AbstractGraph interface for VahanaGraph
edges(vg::VahanaGraph) = vg.edges

Base.eltype(::VahanaGraph) = Int64

edgetype(vg::VahanaGraph) = Graphs.SimpleEdge{eltype(vg)}

has_edge(vg::VahanaGraph, s, d) = Graphs.Edge(s, d) in vg.edges

has_vertex(vg::VahanaGraph, v) = v <= length(vg.g2v)

inneighbors(vg::VahanaGraph, v) =
    map(e -> e.src, filter(e -> e.dst == v, vg.edges)) |> unique
        
ne(vg::VahanaGraph) = length(vg.edges)

nv(vg::VahanaGraph) = length(vg.g2v)

outneighbors(vg::VahanaGraph, v) = 
    map(e -> e.dst, filter(e -> e.src == v, vg.edges)) |> unique

vertices(vg::VahanaGraph) = 1:length(vg.g2v)

is_directed(::VahanaGraph) = true

is_directed(::Type{VahanaGraph}) = true

Base.show(io::IO, mime::MIME"text/plain", vg::VahanaGraph) =
    println(io, "Subgraph of $(vg.sim.name) {$(vg.agenttypes), $(vg.networks)}  {$(nv(vg)), $(ne(vg))}")

