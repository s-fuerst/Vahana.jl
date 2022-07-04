```@meta
CurrentModule = Vahana
```

# REPL tools

Vahana has some features that support model development via Julia's
interactive command line REPL. In this case, the simulation is not
parallelized, but the same code runs later via parallelization without
requiring any (or minimal) changes.

Beside the pretty-printing of a simulation as shown in the
tutorial(s), there are also additional functions that allows to
investigate the current state of a simulation:

```@docs
show_agents
show_network

show_agent
show_random_agent
```

`apply_transition` can be used to calculate a transition function
without modifying the original state. This can be useful in the
development process, but for the final simulation the function should
be replaced with the modifying version `apply_transition!`.
