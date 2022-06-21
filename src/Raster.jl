export add_raster!, calc_raster, calc_raster_flexible, raster_nodeid, move_to!

"""
    add_raster!(sim, dims, agent_constructor, edge_constructor)

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

TODO DOC name

Returns a vector with the IDs of the created agents.

```@repl
using Vahana
struct RasterNode
    pos::Tuple{Int64, Int64} 
end
struct RasterEdge end
const sim = ModelTypes() |> 
    add_agenttype!(RasterNode) |>
    add_edgetype!(RasterEdge) |>
    construct("Raster Example", nothing, nothing)

add_raster!(sim, 
            (10, 8),
            pos -> RasterNode(pos), # or just RasterNode
            RasterEdge();
            name = :grid
            )
```            
"""
function add_raster!(sim,
              dims::Tuple{Int64, Int64},
              agent_constructor,
              edge_constructor;
              name = nothing)
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

    if name !== nothing
        onebased = map(c -> c .+ (1,1), coord)
        grid = Array{AgentID, 2}(undef, dims)
        for i in 1:numcells
            grid[onebased[i][1], onebased[i][2]] = ids[i]
        end
        sim.rasters[name] = grid
    end
end


"""
    calc_raster(sim, name::Symbol, f)

Calculate the values for a raster `name` by applying `f` to each
agentstate of the agents constructed by the `add_raster!` function.

Returns a matrix with those values.

See also [`add_raster!`](@ref)
"""
function calc_raster(sim, name::Symbol, f, t::Type{T}) where T
    map(id -> agentstate(sim, id, t) |> f, sim.rasters[name])
end

function calc_raster_flexible(sim, name::Symbol, f)
    map(id -> agentstate_flexible(sim, id) |> f, sim.rasters[name])
end


"""
    raster_nodeid(sim, name::Symbol, pos)

Returns the ID of the agent (node) from the raster `name` at the
position `pos`, where `pos` must be a Tuple{Int64,Int64}.

See also [`add_raster!`](@ref), [`move_to!`](@ref),
[`add_edge!`](@ref) and [`agentstate`](@ref)
"""
function raster_nodeid(sim, name::Symbol, pos)
    sim.rasters[name][pos[1], pos[2]]
end

"""
    move_to!(sim, name::Symbol, id::AgentID, pos, ::Type{T}) where T

Creates two edges of type `T` between the agent with ID `id` and the agent (node) from the raster `name` at the position `pos`.

`pos` must be a Tuple{Int64,Int64}.

See also [`add_raster!`](@ref), [`raster_nodeid`](@ref) and [`add_edge!`](@ref) 
"""
function move_to!(sim,
           name::Symbol,
           id::AgentID,
           pos,
           ::Type{T}) where T
    posid = raster_nodeid(sim, name, pos)
    add_edge!(sim, id, posid, T())
    add_edge!(sim, posid, id, T())
end

# add_edge for grid position?

#Texte Sarah lesen?
    
