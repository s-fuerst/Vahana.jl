```@meta
CurrentModule = Vahana
```

# Raster

Spatial information can be added to the simulation in the form of one
or more n-dimensional rasters. 

```@docs
add_raster!
```

Those rasters are implemented as part of the graph structure, with
each cell represented as a node, so the cell types must be registered
like the types of agents via `register_agenttype!`.

To create edges between the cells, or between the cells and agents of
other types via the following two helper functions.

```@docs
connect_raster_neighbors!
move_to!
```

The id of a cell for a given position can be archived via `cellid`

```@docs
cellid
```

cellid id or position of a random cell can be drawn via:

```@docs
random_pos
random_cell
```
Such a raster is only a collection of nodes in the graph incl. an
Vahana internal mapping from the cartesian coordinates to the cell
IDs. Beside this mapping, cells are also just agents, but there are
some Vahana functions that utilize the internal cartesian coordinates
to create a n-dimensional representation of the state space.

```@docs
calc_raster
calc_rasterstate
rastervalues
```

