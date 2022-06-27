import Graphs

export add_graph!

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


