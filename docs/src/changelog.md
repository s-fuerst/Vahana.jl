```@meta
CurrentModule = Vahana
```

# Change Log

## v1.4.5

- Fixed compatibility issues with Julia 1.12. This includes a breaking
  change: the pipeable version of `create_simulation` has been removed
  due to world age problems that occur when piping after `create_model`.
  
- Fixed a bug in `read_edges` that sometimes occurred when the
  following hints were used in `register_edgetype!`: :IgnoreFrom,
  :Stateless, and :SingleType.

## v1.4.4

- Mainly updated compat entries.

## v1.4.3

- Improved `VString` to `String` converter (check
  [`create_string_converter`](@ref)) for context).

- Fixed a potential problem with parallel runs in combination with
  agent types in modules.

## v1.4.2

- Fixed a bug in the `with_edge` keyword parameter of the
  [`apply!`](@ref) function. Previously, the transition function
  failed when the specified edge type connected to multiple different
  agent types at the target position. The function now correctly
  handles edges that connect to heterogeneous target agent types.

## v1.4.1

- [`read_agents`](@ref) and [`read_edges`](@ref) returned wrong
  results when used in parallel simulations.

## v1.4

### Improvements

- [`create_string_converter`](@ref) incl. `VString{N}` definition

- [`set_param!`](@ref) can be called for an unknown parameter (this
  will show a warning)
  
- Enhanced type stability in the implementation of the
  [`neighborstates`](@ref) function

### Fixes

An invalid assertion occured when all of the following conditions are met:
1. The `:Immortal` hint is active for the agent type
2. The agent type is not included in the `read` argument
3. The `with_edge` keyword is being utilized

## v1.3

### New features

- Added the `only_surrounding` keyword argument to [`move_to!`](@ref),
  which restricts edge creation to adjacent cells only.

### Improvements

- [`read_params`](@ref) and [`read_globals`](@ref) no longer require
  predefined struct definitions

- Added automatic simulation cleanup through finalizers that invoke
  [`finish_simulation!`](@ref) (Note: For parallel simulations 
  [`finish_simulation!`](@ref) calls are still mendentory)

- Implemented PrettyTables integration for DataFrames, now displaying
  Agent IDs in hexadecimal format

- Enhanced GraphMakie compatibility and support
  
## v1.2.1

- Added workaround for strings in structs stored to a HDF5-file. Check
  the documentation of [`create_string_converter`](@ref) for details.

## v1.2

### Breaking changes

- The `all_ranks` default value for [`num_agents`](@ref) changed to true.

### New features

- The functions [`all_agentids`](@ref) and [`all_edges`](@ref) allows
  to get the ids of all agents or the edges on the rank or the
  complete simulation.

- The functions [`write_metadata`](@ref),
  [`write_sim_metadata`](@ref), [`read_sim_metadata`](@ref) and
  [`read_metadata`](@ref) allows to attach Metadata to the simulation
  or individual parts of the model.

- Instead of writing structs for Parameters and Globals it is now also possible
  to use [`register_param!`](@ref) and [`register_global!`](@ref).

- Random cells or positions of a grid can be obtained via
  [`random_pos`](@ref) and [`random_cell`](@ref)

- The function [`cellid`](@ref) allows to retrieve the id of a cell on
  a given position.

- The functions [`remove_edges!`](@ref) allows to remove edges between
  to agents, or all edges to an agent.

- The new function [`rastervalues`](@ref) can be sometimes a useful shorter
  version then [`calc_rasterstate`](@ref).

- New functions [`set_log_path`](@ref) allows to set the path for
  log files.

- The keyword `with_edge` of the function [`apply`](@ref) allows to
  restrict the called agents to those agents that have an edge of type
  `with_edge`. - The new `:Independet` hint can be given to agent
  types if agents of this type never access the state of other agents
  of this type. This allows Vahana to directly modify the state of an
  agent in its transition functions without having to copy all agents
  of that type.
  
- The new hint `:Independent` can be given to agent types if agents of
  this type never access the state of other agents of this type. This
  allows Vahana to change the state of an agent directly in the
  corresponding vector in a transition functions without copying all
  agents of this type.
  
- For [`neighborstates`](@ref), [`neighborstates_flexible`](@ref),
  [`neighborids`](@ref) and [`edgestates`](@ref) there are now
  also functions that return an iterator for of states/ids instead
  of a vector these states/ids. They have the same function name,
  extended with `_iter`, e.g. [`neighborstates_iter`](@ref).
  
- The function [`add_agent_per_process!`](@ref) allows you to add a single
  agent of an agent type to each process of a parallel simulation.
  
- New function [`modify_global!`](@ref) which is a combination of
  [`set_global!`](@ref) and [`get_global`](@ref).
  
### Improvements

- [`read_snapshot!`](@ref) can be also used to read data that was
  written via calls to [`write_agents`](@ref), [`write_edges`](@ref)
  etc..

- [`calc_rasterstate`](@ref) tries to retrieve the returned type
  automatically by default.

- When [`show_agent`](@ref) is called with an id, it is no longer
  necessary to give also the agent type.
  
- Improved error messages.

- Julia 1.11 introduced a breaking change in Stateful
  Iterators. Vahana's edge iterators have now been adapted to
  accommodate this breaking change.

### Performance Improvements

- [`move_to!`](@ref) improvements.

- Removed an unnecessary memcpy of edges for the case that the type is
  in the `add_existing` argument, but not in the `read` argument of a
  transition function.

### Fixes

- When edges were removed as a consequence of removing agents, this
  change was not written to the hdf5 file until also other changes to
  the edges of this type happen.

- In a parallel simulation, sometime not all edges were written to the
  hdf5 file.

- Fixed a compability issue with Microsofts MPI implementation.

- log folder was also created when no log file was written.

- [`show_agent`](@ref) didn't work for all agent hint combinations.

- It was necessary (and not documented) that [`add_raster!`](@ref) was
  called from all ranks. Now it is possible to call it only from rank
  0 (like [`add_agent!`](@ref) etc.).

## v1.1

### New features

- New functions [`set_hdf5_path`](@ref) allows to set the path for
  hdf5 files.

### Improvements

- Removed usage of Base.memcpy! do be compatible with Julia 1.10.

- Improved error messages.

### Performance Improvements

- For [`calc_raster`](@ref) and [`calc_rasterstate`](@ref).

### Fixes

- Empty arrays where not supported for parameters or globals when
  they where writted to a hdf5 file.


