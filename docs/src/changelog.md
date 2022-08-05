# Change Log

## v0.4
#### new features:
- Huge performance improvement, thanks in part to Agent and Edge
  traits, see [Performance Tuning](performance.md) for details.

- Added [`num_neighbors`](@ref) and [`has_neighbor`](@ref).

- AbstractGraphs and AbstractsSimpleGraphs can be constructed via
  [`vahanagraph`](@ref) and [`vahanasimplegraph`](@ref). This allows
  to use the Graphs.jl package to do graph analysis, the GraphMakie
  package to visualize the graph and internally to use Metis to
  partition the simulation.

### breaking changes:
- Simulation construction, see [`construct_model`](@ref) for details.

- [`agentstate`](@ref) and [`neighborstates`](@ref) need the agent
  type as additional parameter.

- [`neighborids`](@ref) and [`edgestates`](@ref) no longer accept a
  vector of edges, they now work directly with the AgentID.

- The argument order of [`aggregate`](@ref) changed, the agent or edge
  type is now the last argument, as this is also the convention for
  all other Vahana functions.
  
- `add_agenttype!` and `add_edgetype!` renamed to [`register_agenttype!`](@ref)
  and [`register_edgetype!`](@ref).
  
- `show_random_agent` is removed as [`show_agents`](@ref) shows now an
  random agent when no id is given, so.
  
- `raster_nodeid` is renamed to [`cellid`](@ref)  

## v0.2
#### new features:
- Support for two dimensional, discrete spatial information (raster).
  The cells of the raster are also Agents, which are created via the
  [`add_raster!`](@ref) function. Other Agents can be assigned to this
  cells via [`move_to!`](@ref) or by manually creating edges, whereby
  the ids of the cells can be retrieved via `raster_nodeid` (since
  v0.4 [`cellid`])(@ref). [`calc_raster`](@ref) can be used to convert
  the cell nodes to a ordinary matrix.

#### breaking changes:
- Function `add_grid!` renamed to [`add_raster!`](@ref).

## v0.1.3

#### new features:
- [`add_graph!`](@ref) allows to add a graph constructed by the Graphs.jl
  package as a network to a Vahana simulation
- `add_grid!` allows to add a grid to a Vahana simulation (renamed in v0.2 to [`add_raster!`](@ref))
- [`neighborstates`](@ref) allows to get the state of all
  neighbors in a transition function.
- For the REPL: [`show_agent`](@ref) and `show_random_agent`
#### breaking changes:
- Function `states` renamed to [`edgestates`](@ref)
- Function `agentstate_from` is removed:
  Instead of `agentstate_from(sim, edge)` write `agentstate(sim, edge.from)`
