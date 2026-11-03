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

