```@meta
CurrentModule = Vahana
```
# Model Definition

This page describes the functions and data structures employed to
define the model and to instantiate an uninitialized simulation. In
Vahana the simulation state and the transition functions that changes
the state are decoupled, eliminating the need to define the
transition functions prior to creating the simulation instance.

The usual workflow is as follows:
- Define the Agent and Edge types
- Create a [`ModelTypes`](@ref) instance
- Register the Agent and Edge types and optionally the Parameters and Globals
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

The `ModelTypes` instance can subsequently be utilized to construct
the model. This involves that optimized methods are generated, enabling
access or modification of the simulation state during the execution of
the transition function. For comprehensive details regarding this
process, please refer to the [Performance Tuning](./performance.md)
documentation.

!!! tip

	As [`create_model`](@ref) adds new methods, it increments the
	["world age counter"](https://docs.julialang.org/en/v1/manual/methods/#Redefining-Methods).
	This means, that you can not construct the model in the same function
	as you initialize it. But you can call [`create_simulation`](@ref) in the same function so
	
	```julia
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

## Register Parameters and Globals

Parameters are constant values used throughout the simulation, while
globals are variables that can change during the simulation run. You
can register them using `register_param!` and `register_global!`, or
by defining custom types and pass instances of this types to
[`create_simulation`](@ref), as shown in the [Predator/Prey
example](./predator.md).

```@docs
register_param!
register_global!
```


# Create a Simulation

Finally, create an uninitialized simulation based on your model using 

```@docs
create_simulation
```

This creates a blank simulation ready for initialization.

Remember, in Vahana, a "model" specifies the possible state space,
while a simulation is a concrete realization within this
space. Transition functions, defined later, determine how the
simulation evolves over time.

By following these steps, you set up the structure of your
simulation. The next phase involves initializing the simulation with
agents and edges.
