export add_graph!

export vahanagraph, vahanasimplegraph

import Graphs:
    Graphs, edgetype, has_vertex, inneighbors, ne, nv, outneighbors, vertices, is_directed

import Base:
    show, eltype

"""
    add_graph!(sim::Simulation, graph, agent_constructor, edge_constructor) -> Vector{AgentID}

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

    add_graph! is only available when the Graphs.jl package is imported
    by the model implementation. 

Returns a vector with the IDs of the created agents.
"""
function add_graph!(sim::Simulation, graph, agent_constructor, edge_constructor)
    with_logger(sim) do
        @info "<Begin> add_graph!" 
    end
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

    _log_info(sim, "<End> add_graph!")
    
    agents
end

######################################## VahanaSimpleGraph

# VahanaSimpleGraph is a concrete implementation
# of an Graphs.AbstractSimpleGraph. We need this for Metis,
# as it depends on the AbstractSimpleGraph structure instead
# on the official AbstractGraph API.
struct VahanaSimpleGraph <: Graphs.AbstractSimpleGraph{Integer}
    sim::Simulation
    agenttypes::Vector{DataType}
    networks::Vector{DataType}
    g2v::Vector{AgentID}
    v2g::Dict{AgentID, Int64}
    # for the AbstractSimpleGraph interface
    vertices::UnitRange{Integer}
    fadjlist::Vector{Vector{Integer}}
    ne::Integer
end


# like vahanagraph, only for the AbstractSimpleGraph, see also
# the comment of VahanaSimpleGraph above

"""
    vahanasimplegraph(sim::Simulation; [agenttypes::Vector{DataType}, edgetypes::Vector{DataType}, show_ignorefrom_warning = true])

Creates a subgraph with nodes for all agents that have one of the
`agenttypes` types, and all edges that have one of the `edgetypes`
types and whose both adjacent node types are in `agenttypes`.

The default values for `agenttypes` and `edgetypes` are all registered
agents/edgetypes (see [`register_agenttype!`](@ref) and
[`register_edgetype!`](@ref)).

This subgraphs implements the AbstractSimpleGraph interface from the
Graphs.jl package.

The edge types must not have the :IgnoreFrom property. If there are
edge types with this property in the `edgetypes` vector, a warning will
be displayed and these edges will be ignored. The warning can be
suppressed by setting `show_ignorefrom_warning` to false.

!!! warning    

    The AbstractGraph interface allows multiple edges between
    two nodes, but some function (e.g. those that convert the graph
    into a binary (sparse)matrix can produce undefined results
    for those graphs. So use this function with care. 
""" 
function vahanasimplegraph(sim::Simulation;
                    agenttypes::Vector{DataType} = sim.typeinfos.nodes_types,
                    edgetypes::Vector{DataType} = sim.typeinfos.edges_types,
                    show_ignorefrom_warning = true)
    g2v = Vector{AgentID}()
    v2g = Dict{AgentID, Int64}()

    nv = 0
    for t in agenttypes
        for id in 1:length(readstate(sim, t))
            if length(readdied(sim, t)) == 0 || readdied(sim, t)[id] == false
                nv += 1
                aid = agent_id(sim, AgentNr(id), t)
                push!(g2v, aid)
                push!(v2g, aid => nv)
            end
        end
    end

    fadjlist = [ Vector{Integer}() for _ in 1:nv ]

    ne = 0
    for t in edgetypes
        if has_hint(sim, t, :IgnoreFrom)
            if show_ignorefrom_warning
                printstyled("""
    
                Edgetype $t has the :IgnoreFrom hint, therefore edges of this 
                type can not added to the created subgraph

                """; color = :red)
            end
            continue
        end
        for (to, e) in edges_iterator(sim, t)
            if has_hint(sim, t, :Stateless)
                fid = get(v2g, e, nothing)
            else
                fid = get(v2g, e.from, nothing)
            end
            tid = get(v2g, to, nothing)
            if fid !== nothing && tid !== nothing
                push!(fadjlist[fid], tid)
                ne += 1
            end
        end
    end

    VahanaSimpleGraph(sim, agenttypes, edgetypes, g2v, v2g, 1:nv, fadjlist, ne)
end

### AbstractGraph interface for VahanaSimpleGraph
eltype(_::VahanaSimpleGraph) = Int64

edgetype(vg::VahanaSimpleGraph) = Graphs.SimpleEdge{eltype(vg)}

is_directed(::VahanaSimpleGraph) = true

is_directed(::Type{VahanaSimpleGraph}) = true

Base.show(io::IO, _::MIME"text/plain", vg::VahanaSimpleGraph) =
    println(io, "VahanaSimpleGraph of $(vg.sim.name) for {$(vg.agenttypes), $(vg.networks)}  {$(nv(vg)), $(ne(vg))}")

######################################## VahanaGraph

mutable struct VahanaGraph <: Graphs.AbstractGraph{Int64}
    sim
    agenttypes::Vector{DataType}
    edgetypes::Vector{DataType}
    g2v::Vector{AgentID}
    v2g::Dict{AgentID, Int64}
    edges::Vector{Graphs.Edge}
    edgetypeidx::Vector{Int64}
end

# SingleType not supported (und IgnoreFrom sowieso nicht)

"""
    vahanagraph(sim::Simulation; [agenttypes::Vector{DataType}, edgetypes::Vector{DataType}, show_ignorefrom_warning = true, drop_multiedges = false])

Creates a subgraph with nodes for all agents that have one of the
`agenttypes` types, and all edges that have one of the `edgetypes`
types and whose both adjacent agents have are of a type in `agenttypes`.

The default values for `agenttypes` and `edgetypes` are all registered
agents/edgetypes (see [`register_agenttype!`](@ref) and
[`register_edgetype!`](@ref)).

This subgraphs implements the AbstractGraph interface from the
Graphs.jl package, so that e.g. GraphMakie can be used to visualize
the subgraph. See also [`create_graphplot`](@ref).

The AbstractGraph interface allows multiple edges between two nodes,
but some functions (e.g. those that convert the graph to a binary
(sparse) matrix) may produce undefined results for these graphs,
e.g. when graphplot is called from GraphMakie.jl. If the keyword
`drop_multiedges` is true and there are multiple edges, only the edge
of the type that is first in the edgetypes vector is added to the
generated graph.

The edge types must not have the :IgnoreFrom property. If there are
edge types with this property in the `edgetypes` vector, a warning will
be displayed and these edges will be ignored. The warning can be
suppressed by setting `show_ignorefrom_warning` to false.
""" 
function vahanagraph(sim;
              agenttypes::Vector{DataType} = sim.typeinfos.nodes_types,
              edgetypes::Vector{DataType} = sim.typeinfos.edges_types,
              show_ignorefrom_warning = true,
              drop_multiedges = false)
    g2v = Vector{AgentID}()
    v2g = Dict{AgentID, Int64}()
    edgetype = Vector{Int64}()

    existing = Set{Tuple{AgentID, AgentID}}()
    
    nv = 0
    for T in agenttypes
        for id in 1:length(readstate(sim, T))
            if length(readdied(sim, T)) == 0 || readdied(sim, T)[id] == false
                nv += 1
                aid = agent_id(sim, AgentNr(id), T)
                push!(g2v, aid)
                push!(v2g, aid => nv)
            end
        end
    end

    edges = Vector{Graphs.Edge}()
    edgetypeidx = 1
    for T in edgetypes
        if has_hint(sim, T, :IgnoreFrom)
            if show_ignorefrom_warning
                printstyled("""
    
                Edgetype $T has the :IgnoreFrom hint, therefore edges of this 
                type can not added to the created subgraph

                """; color = :red)
            end
            continue
        end
        for (to, e) in edges_iterator(sim, T)
            f = get(v2g, hasproperty(e, :from) ? e.from : e, nothing)
            t = get(v2g, to, nothing)
            if f !== nothing && t !== nothing
                if (f, t) in existing && drop_multiedges
                    println("Edge $(string(f, base=16)) to agent $(string(to, base=16)) will not be shown")
                else
                    if drop_multiedges
                        push!(existing, (f, t))
                    end
                    push!(edges, Graphs.Edge(f, t))
                    push!(edgetype, edgetypeidx)
                end
            end
        end
        edgetypeidx += 1
    end

    VahanaGraph(sim, agenttypes, edgetypes, g2v, v2g, edges, edgetype)
end


### AbstractGraph interface for VahanaGraph
Graphs.edges(vg::VahanaGraph) = vg.edges

Base.eltype(::VahanaGraph) = Int64

edgetype(vg::VahanaGraph) = Graphs.SimpleEdge{eltype(vg)}

Graphs.has_edge(vg::VahanaGraph, s, d) = Graphs.Edge(s, d) in vg.edges

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
    println(io, "VahanaGraph of $(vg.sim.name) for {$(vg.agenttypes), $(vg.edgetypes)}  {$(nv(vg)), $(ne(vg))}")

