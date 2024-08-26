```@meta
CurrentModule = Vahana
```

# Transition Function

After the initialization, the state of the simulation is modified by
so called transition functions. See [Defining Transition
Functions](tutorial1.html#Defining-Transition-Functions) for details.

```@docs
apply!
apply
```

!!! tip 

	The agent types must be immutable, but in many cases the agent
	returned by a transition function will have a different state than the
	agent specified as a parameter. If only one field is changed, we still
	need to copy all other fields. The Setfield.jl package can be very
	useful in this case.

Inside a transition function the following functions can be used to
access the state of the simulation:

## Globals and Parameters
```@docs
param
get_global
```

## Get Agent(state)

```@docs
agentstate
agentstate_flexible
```

To retrieve the states of all agents connected to a target agent
through edges of a specific type, the `neighborstates_iter` functions
can be beneficial. These functions combine the capabilities of
[`neighborids`](@ref) and [`agentstate`](@ref), allowing you to
easy access the desired information.

```@docs
neighborstates
neighborstates_flexible
neighborstates_iter
neighborstates_flexible_iter
```

## Get Edge(state)

```@docs
edges
edgestates
edgestates_iter
neighborids
neighborids_iter
num_edges(::Simulation, id::AgentID, edgetype::Type) 
has_edge
```

For all the function like [`edges`](@ref) that returns `nothing` in
the case that there is no edge with the agent as target, it can be
useful to increase the readability of the code by using the `checked`
function to test for `nothing`.

```@docs
checked
```

## Add Agents/Edges

The following functions from the [Initialization] section
(initialization.md) that add new agents or edges to a simulation can
also be used inside a transition function, namely:

* [`add_agent!`](@ref) and [`add_agents!`](@ref)
* [`add_edge!`](@ref) and [`add_edges!`](@ref)
* [`connect_raster_neighbors!`](@ref)
* [`move_to!`](@ref)

## Remove Edges

Since v1.2 it's also possible to remove edges:

```@docs
remove_edges!
```
