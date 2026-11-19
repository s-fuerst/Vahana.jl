# AGENTS.md

Rules for AI agents in this repository.

## Project context

Vahana.jl is a high-performance, agent-based modelling library written in
Julia, designed for large-scale simulations of complex social networks
(graph dynamical systems). Key characteristics:

- **MPI** for distributed-memory parallelism (scales to thousands of
  cores); **HDF5** (optionally parallel HDF5) for data persistence.
- Code lives in `src/` (one file per concern: `Simulation.jl`,
  `AgentMethods.jl`, `MPI.jl`, `HDF5.jl`, ...), tests in `test/`
  (parallel tests in `test/mpi/`), auxiliary tooling in `tools/`.
- MPI-collective: changes to functions called from collective contexts
  must stay valid on all ranks and must not introduce rank-divergent
  code paths or extra collective calls inconsistently.
- Tests are run with `julia --project=. test/<file>.jl`; MPI tests with
  `mpiexecjl -n <ranks> julia --project=. test/mpi/<file>.jl`.
- The development checkout is a `dev/` project; do not assume a
  registered `Vahana` package environment.

## Code changes

- **Only make code changes after an explicit request.** Discussions,
  analyses, and design considerations are not approval for changes. Ask
  explicitly before editing anything in a discussion round.
- Do not overwrite files the user is editing in parallel.
- Show all proposed code, including revisions, in the conversation and discuss
  it before writing it to files. Obtain explicit approval for the shown code;
  agreement on design decisions alone is not permission to write code.

## Code style

- Use explicit `if` / `elseif` / `else` blocks for control flow. Do not use
  short-circuiting `&&` or `||` as substitutes for conditionals (for example,
  `condition || throw(...)` or `condition && return ...`). Boolean expressions
  in conditions are fine.
- Use Julia's implicit return of the final expression for all result types.
  Do not write an explicit `return` on the final line of a function; this also
  applies to `nothing` and constructor expressions such as `new(...)`.
- Avoid early returns. On normal execution, a function should reach its final
  expression. Only use early returns in exceptional cases with a compelling,
  explicitly discussed justification. Throwing an exception for invalid input
  is not a normal return path.

