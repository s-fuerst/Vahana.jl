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
- Call [`create_model`](@ref) to create an model (state space) object
- Call [`create_simulation`](@ref) to create an uninitialized simulation of this model

The simplest example for such a workflow is:

```
struct Agent end
struct EdgeState end

const sim = ModelTypes() |>
    register_agenttype!(Agent) |>
    register_edgetype!(EdgeState) |>
    create_model("Minimal Example") |>
    create_simulation()
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

We can then use the `ModelTypes` instance to construct the model.
Which means that optimized methods that can be used to access or
modify the simulation state during the transition function are
generated. See [Performance Tuning](./performance.md) for details.

!!! tip

	As [`create_model`](@ref) adds new methods, it increments the
	["world age counter"](https://docs.julialang.org/en/v1/manual/methods/#Redefining-Methods).
	This means, that you can not construct the model in the same function
	as you initialize it. But you can call [`create_simulation`](@ref) in the same function so
	
	```
    const model = create_model(modeltypes, "Minimal Example") 
	
	function create_and_init()
		sim = create_simulation(model, nothing, nothing)
		add_agent!(sim, Agent())
	end
	```
	
	is valid code. 

```@docs
create_model
Model
```

# Create a Simulation

```@docs
create_simulation
```
