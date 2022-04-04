# Change Log

## v0.2
#### new features:
- Support for two dimensional, discrete spatial information (raster).
  The cells of the raster are also Agents, which are created via the
  [`add_raster!`](@ref) function. Other Agents can be assigned to this
  cells via [`move_to!`](@ref) or by manually creating edges, whereby
  the ids of the cells can be retrieved via
  [`raster_nodeid`](@ref). [`calc_raster`](@ref) can be used to convert the
  cell nodes to a ordinary matrix.

#### breaking changes:
- function `add_grid!` renamed to [`add_raster!`](@ref).

## v0.1.3

#### new features:
- [`add_graph!`](@ref) allows to add a graph constructed by the Graphs.jl
  package as a network to a Vahana simulation
- `add_grid!` allows to add a grid to a Vahana simulation (renamed in v0.2 to [`add_raster!`](@ref))
- [`neighborstates`](@ref) allows to get the state of all
  neighbors in a transition function.
- For the REPL: [`show_agent`](@ref) and [`show_random_agent`](@ref).
#### breaking changes:
- function `states` renamed to [`edgestates`](@ref)
- function `agentstate_from` is removed:
  Instead of `agentstate_from(sim, edge)` write `agentstate(sim, edge.from)`
