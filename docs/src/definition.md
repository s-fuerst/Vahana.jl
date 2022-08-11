```@meta
CurrentModule = Vahana
```
# Model Definition

This page describes all the functions and structures used to define
the model and to create an uninitialized simulation. In Vahana the
simulation state and the transition functions that changes the state
are decoupled, therefore we do not need to define the transition
functions before we create the simulation. 

The usual workflow is as follows:
- Define all Agent and Edge types
- Create a [`ModelTypes`](@ref) instance
- Register the defined Agent and Edge types 
- Call [`construct_model`](@ref) to create an model (state space) object
- Call [`new_simulation`](@ref) to create a simulation of this model

The simplest example of creating a simulation is:

```
struct Agent end
struct EdgeState end

const sim = ModelTypes() |>
    register_agenttype!(Agent) |>
    register_edgetype!(EdgeState) |>
    construct_model("Minimal Example") |>
    new_simulation(nothing, nothing)
```

After the simulation is created, the next step is its initialization,
which is described on the [next](./initialization.md) page.

## Construct the Model 

Before we can construct a model, we need to register all agent and
edge types. 

```@docs
ModelTypes
register_agenttype!
register_edgetype!
```

!!! tip
	
	That the agent and edge types must be bits types has the implication
	that it is not possible for them to have a `String` field, since Strings have 
	a flexible size. But the InlineStrings.jl package can be used instead, if
	Strings are really necessary.
	
We can then use the `ModelTypes` instance to construct the model.
Which means that optimized methods that can be used to access or
modify the simulation state during the transition function are
generated. See [Performance Tuning](./performance.md) for details.

!!! tip

	As [`construct_model`](@ref) adds new methods, it increments the
	["world age counter"](https://docs.julialang.org/en/v1/manual/methods/#Redefining-Methods).
	This means, that you can not construct the model in the same function
	as you initialize it. But you can call [`new_simulation`](@ref) in the same function so
	
	```
    const model = construct_model(modeltypes, "Minimal Example") 
	
	function create_and_init()
		sim = new_simulation(model, nothing, nothing)
		add_agent!(sim, Agent())
	end
	```
	
	is valid code. 

```@docs
construct_model
Model
```

# Create a Simulation

```@docs
new_simulation
```
