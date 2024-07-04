```@meta 
CurrentModule = Vahana 
``` 
# Misc

Here are the remaining exported Vahana definitions that do not fit into any of the previous categories:

## Agents

```@docs
ProcessID
AgentID
all_agents
all_agentids
num_agents
```

## Edges

```@docs
all_edges
num_edges(sim, t::Type{T}, sum_ranks = true; write = nothing) where T
```

## REPL

Vahana has some features that support model development via Julia's
interactive command line REPL. In this case, the simulation is not
parallelized, but the same code runs later via parallelization without
requiring any (or minimal) changes.

Beside the pretty-printing of a simulation as shown in the
tutorial(s), there are also additional functions that allows to
investigate the current state of a simulation:

```@docs
show_agent
DataFrames.DataFrame(sim::Simulation, T::DataType)
```

[`apply`](@ref) can be used to calculate a transition function
without modifying the original state. This can be useful in the
development process, but for the final simulation the function should
be replaced with the modifying version `apply!`.

## Simulation

```@docs
copy_simulation
finish_simulation!
```




