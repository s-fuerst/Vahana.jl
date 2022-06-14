```@meta
CurrentModule = Vahana
```

```@docs
construct

add_agenttype!
add_agent!
add_agents!

add_edgetype!
add_edge!
add_edges!

add_graph!
add_raster!
```
```@example
using Vahana
struct RasterNode
    pos::Tuple{Int64, Int64} 
end
struct RasterEdge end
const sim_raster = ModelTypes() |> 
    add_agenttype!(RasterNode) |>
    add_edgetype!(RasterEdge) |>
    construct("Raster Example", nothing, nothing)

add_raster!(sim_raster, 
            (10, 8),
            pos -> RasterNode(pos), # or just RasterNode
            RasterEdge();
            name = :grid
            )
```

```@docs
move_to!

finish_init!
```
