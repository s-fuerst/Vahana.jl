```@meta
CurrentModule = Vahana
```

```@docs
Simulation(name::String, params, globals)

Agent

add_agenttype!
add_agent!
add_agents!

Edge
EdgeState

add_edgetype!
add_edge!
add_edges!

finish_init!
```

<!-- Sometimes it's necessary to calculate some global values before calling the first transition function: -->

<!-- ```@docs -->
<!-- aggregate -->

<!-- setglobal! -->
<!-- pushglobal! -->
<!-- ``` -->
