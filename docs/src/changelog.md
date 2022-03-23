# Change Log

## v0.1.3

#### new features:
- [`add_graph!`](@ref) allows to add a graph constructed by the Graphs.jl
  package as a network to a Vahana simulation
- [`add_grid!`](@ref) allows to a grid to a Vahana simulation 
- [`neighborstates`](@ref) allows to get the state of all
  neighbors in a transition function.
- For the REPL: [`show_agent`](@ref) and [`show_random_agent`](@ref).
#### breaking changes:
- function `states` renamed to [`edgestates`](@ref)
- function `agentstate_from` is removed:
  Instead of `agentstate_from(sim, edge)` write `agentstate(sim, edge.from)`
