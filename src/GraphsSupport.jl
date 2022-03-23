import Graphs

export add_graph!, add_grid!

"""
    add_graph!(sim, graph, agent_constructor, edge_constructor) -> Vector{AgentID}

Adds a `graph` from the Graphs.jl package to `sim`, incl. all vertices
of `graph` as new agents.

`graph` must be a Graphs.Graph or a Graphs.DiGraph.

For each vertix of `graph` the `agent_constructor` function is called, with
the Graphs.vertix as argument. For each edge of `graph` the
`edge_constructor` function is called, with the Graphs.edge as argument.

The agent types of agents created by the `agent_constructor` must be
already registered via [`add_agenttype!`](@ref) and vis a vis the edge
type via [`add_edgetype!`](@ref).

Returns a vector with the IDs of the created agents.
"""
function add_graph!(sim::Simulation, graph, agent_constructor, edge_constructor) 
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


"""
    add_grid!(sim::Simulation, dims, agent_constructor, edge_constructor)

Adds a 2-dimensional grid to `sim`, incl. a new agents per cell and
edges between the cells. 

In the current version, edges will be constructed for a Moore
Neighborhood of range 1 with periodic boundaries.

For each cell the `agent_constructor` function is called, with the cell
position as argument. The positions have a range from (1,1) to
(dims[1], dims[2])

For each edge the `edge_constructor` function is called, without any
argument.

The agent types of agents created by the `agent_constructor` must be
already registered via [`add_agenttype!`](@ref) and vis a vis the edge
type via [`add_edgetype!`](@ref).

Returns a vector with the IDs of the created agents.
"""
function add_grid!(sim::Simulation, dims::Tuple{Int64, Int64}, agent_constructor, edge_constructor)
    function calcidx_torus(point::Tuple, diff::Tuple, dims::Tuple)
        ((point[1] + diff[1] + dims[1]) % dims[1]) * dims[2] +
            ((point[2] + diff[2] + dims[2]) % dims[2]) + 1
    end

    numcells = dims[1] * dims[2]
    coord = map(i -> (divrem(i-1, dims[2])), 1:numcells)
    ids = add_agents!(sim, [ agent_constructor(p .+ (1,1)) for p in coord ])

    ec = edge_constructor
    for i in 1:numcells
        add_edge!(sim, ids[i], ids[calcidx_torus(coord[i], (-1,-1), dims)], ec)
        add_edge!(sim, ids[i], ids[calcidx_torus(coord[i], (-1, 0), dims)], ec)
        add_edge!(sim, ids[i], ids[calcidx_torus(coord[i], (-1,+1), dims)], ec)
        add_edge!(sim, ids[i], ids[calcidx_torus(coord[i], ( 0,-1), dims)], ec)
        add_edge!(sim, ids[i], ids[calcidx_torus(coord[i], ( 0,+1), dims)], ec)
        add_edge!(sim, ids[i], ids[calcidx_torus(coord[i], (+1,-1), dims)], ec)
        add_edge!(sim, ids[i], ids[calcidx_torus(coord[i], (+1, 0), dims)], ec)
        add_edge!(sim, ids[i], ids[calcidx_torus(coord[i], (+1,+1), dims)], ec)
    end
end
