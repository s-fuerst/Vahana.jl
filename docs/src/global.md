```@meta
CurrentModule = Vahana
```

# Update Globals and Rasters

There is also some part of the simulation state that can not be
changed inside a transition functions and should be therefore modified
between two ref`apply!` calls, namly the globals and rasters.

## Globals

You can add a new or additional value to the `globals` struct with:

```@docs
set_global!
push_global!
modify_global!
Vahana.mapreduce
```

!!! warning

	The state of the globals struct (the `globals` argument of the 
	[`create_simulation`](@ref) function, and of rasters can not be changed 
	inside of a transition function, as a transition function is calculated on a per
	agent basis with many evaluations in parallel, and changes to the
	global layer must be a single, synchronized operation.

## Raster 

Even if it looks like rasters are just nodes in the graph, and the type of those nodes must be registered like the types of agents via `register_agenttype!`, they have a special property: In a parallel simulation the state of the raster nodes are synchronized, so that the complete state of a raster is available on all ranks.  

So to update the values of a raster, you can use one of the following functions:

```@docs
calc_raster
calc_rasterstate
```

Also the id of a cell for a given position can be archived via cellid:

```@docs
cellid
```

