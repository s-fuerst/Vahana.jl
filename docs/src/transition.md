```@meta
CurrentModule = Vahana
```

# Transition Function

As mentioned in the `create_model` documentation, the model struct is
created dynamically incl. a dynamically name for this struct. E.g. in
the "Game of Life" Example this type is `Vahana.var"Game of
Life"{Params, Globals} `. All of this types are subtypes of the
abstract type `Simulation`.

```@docs
apply!
apply
```


!!! tip 

	The agent types must be immutable, but in most cases the agent
	returned by a transition function will have a different state than the
	agent specified as a parameter. If only one field is changed, we still
	need to copy all other fields. The Setfield.jl package can be very
	useful in this case.


Inside a transition function the following functions can be used to access the state of the simulation:

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

To get the states of all agents that are connected to an agent (with
the agent as target) via edges of a specific edge type, the
neighborstates functions can be useful (they are combing
[`neighborids`](@ref) and [`agentstate`](@ref)).

```@docs
neighborstates
neighborstates_flexible
```

## Get Edge(state)

```@docs
edges
edgestates
neighborids
num_edges(::__MODEL__, id::AgentID, edgetype::Type) 
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



