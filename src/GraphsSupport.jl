export add_graph!

export vahanagraph, vahanasimplegraph

import Graphs:
    Graphs, edges, edgetype, has_edge, has_vertex, inneighbors, ne, nv, outneighbors, vertices, is_directed

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

    add_graph! is only available when the Graphs.jl package is imported
    by the model implementation. Therefore the "Vahana does not have
    Graphs in its dependencies" warning can be ignored.

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

# VahanaSimpleGraph is a concrete implementation
# of an Graphs.AbstractSimpleGraph. We need this for Metis,
# as it depends on the AbstractSimpleGraph structure instead
# on the official AbstractGraph API.
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


# like vahanagraph, only for the AbstractSimpleGraph, see also
# the comment of VahanaSimpleGraph above
"""
    vahanasimplegraph(sim; agenttypes::Vector{DataType}, edgetypes::Vector{DataType})

Creates a subgraph with nodes for all agents that have one of the
`agenttypes` types, and all edges that have one of the `edgetypes`
types and whose both adjacent node types are in `agenttypes`.

The default values for `agenttypes` and `edgetypes` are all registered
agents/edgetypes (see [`register_agenttype!`](@ref) and
[`register_edgetype!`](@ref)).

This subgraphs implements the AbstractSimpleGraph interface from the
Graphs.jl package.

The edge types must not have the :IgnoreFrom trait.

!!! info 

    `vahanasimplegraph` is only available when the Graphs.jl package is
    imported by the client.

!!! warning    

    The AbstractGraph interface allows multiple edges between
    two nodes, but some function (e.g. those that convert the graph
    into a binary (sparse)matrix can produce undefined results
    for those graphs. So use this function with care. 
""" # TODO write tests, , check :IgnoreFrom and print a warning
function vahanasimplegraph(sim;
                    agenttypes::Vector{DataType} = sim.typeinfos.nodes_types,
                    edgetypes::Vector{DataType} = sim.typeinfos.edges_types,
                    show_ignorefrom_warning = true)
    g2v = Vector{AgentID}()
    v2g = Dict{AgentID, Int64}()

    nv = 0
    for t in agenttypes
        tid = sim.typeinfos.nodes_type2id[t]
        for id in keys(_getread(sim, t))
            nv += 1
            aid = agent_id(sim, tid, AgentNr(id))
            push!(g2v, aid)
            push!(v2g, aid => nv)
        end
    end

    fadjlist = [ Vector{Integer}() for _ in 1:nv ]

    ne = 0
    for t in edgetypes
        if has_trait(sim, t, :IgnoreFrom)
            if show_ignorefrom_warning
                printstyled("""
    
                Edgetype $t has the :IgnoreFrom trait, therefore edges of this 
                type can not added those edges to the created subgraph

                """; color = :red)
            end
            continue
        end
        for (to, e) in edges_iterator(sim, t)
            if has_trait(sim, t, :Stateless)
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

struct VahanaGraph <: Graphs.AbstractGraph{Int64}
    sim
    agenttypes::Vector{DataType}
    edgetypes::Vector{DataType}
    g2v::Vector{AgentID}
    v2g::Dict{AgentID, Int64}
    edges::Vector{Graphs.Edge}
    edgetypeidx::Vector{Int64}
end

# SingleAgentType not supported (und IgnoreFrom sowieso nicht)
"""
    vahanagraph(sim; agenttypes::Vector{DataType}, edgetypes::Vector{DataType})

Creates a subgraph with nodes for all agents that have one of the
`agenttypes` types, and all edges that have one of the `edgetypes`
types and whose both adjacent node types are in `agenttypes`.

The default values for `agenttypes` and `edgetypes` are all registered
agents/edgetypes (see [`register_agenttype!`](@ref) and
[`register_edgetype!`](@ref)).

This subgraphs implements the AbstractGraph interface from the
Graphs.jl package, so that e.g. GraphMakie can be used to visualize
the subgraph. See also [`plot`](@ref).

The edge types must not have the :IgnoreFrom trait.

!!! info 

    `vahanagraph` is only available when the Graphs.jl package is
    imported by the client.

!!! warning    

    The AbstractGraph interface allows multiple edges between
    two nodes, but some function (e.g. those that convert the graph
    into a binary (sparse)matrix can produce undefined results
    for those graphs. So use this function with care. 
""" # TODO write tests, check :IgnoreFrom and print a warning
function vahanagraph(sim;
                     agenttypes::Vector{DataType} = sim.typeinfos.nodes_types,
                     edgetypes::Vector{DataType} = sim.typeinfos.edges_types)
    g2v = Vector{AgentID}()
    v2g = Dict{AgentID, Int64}()
    edgetype = Vector{Int64}()
    
    nv = 0
    for T in agenttypes
        tid = sim.typeinfos.nodes_type2id[T]
        for id in keys(_getread(sim, T))
            nv += 1
            aid = agent_id(sim, tid, AgentNr(id))
            push!(g2v, aid)
            push!(v2g, aid => nv)
        end
    end

    edges = Vector{Graphs.Edge}()
    edgetypeidx = 1
    for T in edgetypes
        for (to, e) in edges_iterator(sim, T)
            f = get(v2g, hasproperty(e, :from) ? e.from : e, nothing)
            t = get(v2g, to, nothing)
            if f !== nothing && t !== nothing
                push!(edges, Graphs.Edge(f, t))
                push!(edgetype, edgetypeidx)
            end
        end
        edgetypeidx += 1
    end

    VahanaGraph(sim, agenttypes, edgetypes, g2v, v2g, edges, edgetype)
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
    println(io, "VahanaGraph of $(vg.sim.name) for {$(vg.agenttypes), $(vg.edgetypes)}  {$(nv(vg)), $(ne(vg))}")

