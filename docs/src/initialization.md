```@meta
CurrentModule = Vahana
```

# Initialization

After we created a simulation by calling [`create_simulation`](@ref), we
must build the initial state of the simulation. As in Vahana the state
of a model is represented as a graph, this means we must add the nodes
(our agents) and edges to the graph.

```@docs
add_agent!
add_agents!
```

!!! warning

	The IDs created by `add_agent(s)!` contain Vahana internal information, that can change
	after an [`apply!`](@ref) or the [`finish_init!`](@ref) call. It is even possible that 
	different agents have the same ID at different times. This has the implication, that 
	the IDs can only be used temporary and should not be stored in the state of an agent or edge.

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

There is also two function with works the other way and converts the
underlying graph of an simulation (or a subset of this graph) to a
structure that fulfills the AbstractGraph or AbstractSimpleGraph
interface from the Graphs.jl package.

```@docs
vahanagraph
vahanasimplegraph
```

## Raster 

Spatial information can be added to the simulation in the form of one
or more n-dimensional rasters. 

```@docs
add_raster!
```

Such a raster is only a collection of nodes in the graph incl. an
Vahana internal mapping from the cartesian coordinates to the cell
IDs. Beside this mapping, cells are also just agents, but there are
some Vahana functions like [`calc_raster`](@ref) that utilize the
internal cartesian coordinates to create a n-dimensional
representation of the state space.

The ID and the state of the cells can not be accessed directly,
instead it's necessary to create edges between the cells, or between
the cells and agents of other types via the following two helper
functions.

```@docs
connect_raster_neighbors!
move_to!
```
## Set Parameters

TODO DOC

```@docs
set_param!
```



## Finish initialization

After all the initial state has been built using the functions
described above, `finish_init!` must be called before the first call
of [`apply!`](@ref)

```@docs
finish_init!
```
