```@meta
CurrentModule = Vahana
```

# Initialization

After we created a simulation by calling [`new_simulation`](@ref), we
must build the initial state of the simulation. As in Vahana the state
of a model is represented as a graph, this means we must add the nodes
(our agents) and edges to the graph.

```@docs
add_agent!
add_agents!
```

!!! warning

	The IDs created by add_agent(s)! contain Vahana internal information, that can change
	after an [`apply_transition!`](@ref) or the [`finish_init!`](@ref) call. This has also 
	the implication, that the IDs can only be used temporary and should not be stored
	in the state of an agent or edge.

```@docs
Edge

add_edge!
add_edges!
```

## Graphs

It is also possible to use [graph
generators](https://juliagraphs.org/Graphs.jl/stable/core_functions/simplegraphs_generators/)
from the Graphs.jl package to construct the initial state. Or parts of
it, since you can combine it with all the other functions described on
this page. Since Graphs.jl has overlapping function names with Vahana,
it is advisable to import only the SimpleGraphs module from Graphs.jl.

```@docs
add_graph!
```

## Grids

Spatial information can be added to the simulation in the form of one
or more two-dimensional rasters/grids. 

```@docs
add_raster!
move_to!
```

## Finish initialization

```@docs
finish_init!
```
