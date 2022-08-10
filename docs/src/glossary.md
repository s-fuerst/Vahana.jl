# Glossary

## Agent / Node 

In Vahana, agents are implemented as nodes of a graph. Thus, agent and
node have the same meaning. Whereby one could argue that an agent also
includes its relationship to its environment, in Vahana in the form of
the Edges adjacent to the agent. However, this is not the
interpretation in the documentation, here agent means only the node of
the graph, incl. it's state.

## Cell

A cell is an agent which is also an element of a raster, and has
therefore a position in the space of the raster. See also [`add_raster!`](@ref).

## Global Layer



## Interagent Layer

## Model

In Vahana, a model is a collection of all defined agent and edge types
that describes the state space of the simulation. However, the model
does not know anything about how the state of a simulation is changed.
See also [`construct_model`](@ref).

## Network



## Neighbor(hood)



## Raster / Grid / Spatial Layer

## Simulation

A simulation is an instance of the model. There can exist multiple
simulation in a single Julia session, each simulation with potential
different parameters and state. See also [`new_simulation`](@ref).

## Source / Target

## Transition Function

## Trait

