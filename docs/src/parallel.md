# Parallel simulations

Vahana supports parallel simulations using MPI (Message Passing
Interface), allowing for significant performance improvements when
running large-scale models. To run a Vahana simulation in parallel,
you can use the mpirun or mpiexec command, specifying the number of
processes with the -np parameter. For example:

```mpirun -np 4 julia your_model.jl```

The `-np` parameter indicates the number of processes/threads to be
used for the simulation.

This approach works without any additional changes in the model
code. However, there are ways to further optimize performance in a
parallel simulation:

## MPI.jl Integration

Vahana utilizes the MPI.jl package for its parallel computing
capabilities. While no specific MPI knowledge is required to use
Vahana's parallel features, users may find it beneficial to review the
[Configuration
section](https://juliaparallel.org/MPI.jl/stable/configuration/) of
the [MPI.jl documentation](https://juliaparallel.org/MPI.jl/stable/)
for a deeper understanding of the underlying parallel computing
framework.  For advanced users or specific scenarios, it's possible to
work directly with MPI.jl functions within your Vahana model. When
using Vahana in parallel mode, MPI is automatically initialized, and
the following MPI-related variables are available:

- `mpi.comm`: The MPI communicator object.
- `mpi.rank`: An integer identifying the current process (0-based).
- `mpi.size`: The total number of processes in the parallel computation.

Remember that while direct use of MPI functions can provide additional
flexibility, it also requires careful handling to maintain consistency
across all processes and avoid potential race conditions or deadlocks.

## Partitioning

The simulation graph is partitioned and distributed in the
[`finish_init!`](@ref) call. By default, the
[Metis.jl](https://github.com/JuliaSparse/Metis.jl) package is used
for this. However, during this process, the information about the
different agent types is lost. As a result, if multiple agent types
are used, it is possible that the number of agents is unevenly
distributed for a single agent type.

To address this issue, an alternative partitioning scheme,
`:EqualAgentNumbers`, is available. However, this scheme ignores the
edges and number of cuts in the graph.

In order to optimize the partitioning for your specific model, it can
be very useful to perform your own partitioning and pass it to
[`finish_init!`](@ref). An example of this can be seen in the
`create_partition` function of the [Vahana Episim
Example](https://git.zib.de/sfuerst/vahana-episim/-/blob/main/src/episim.jl).

## Using @rootonly

It's helpful to understand that in MPI programs, all processes execute
the same program code, but operate on different data. In a typical
Vahana program, up to the [`finish_init!`](@ref) call, all processes
runs the same code that creates the initial graph and therefore, if
there are `n` processes, the complete graph is generated `n`
times. However, at the [`finish_init!`](@ref) step, only the graph
from process 0 (the root process) is considered, and the graphs
constructed on all other processes are discarded.

Although this is not necessarily problematic, it can be beneficial to
construct the graph only on the root processes, especially if files
are being read during the initialization phase.

To execute instructions only on the root process, you can use the
`@rootonly` macro. For example, the [Vahana Episim
example](https://git.zib.de/sfuerst/vahana-episim) includes the
following code before the [`finish_init!`](@ref) call:

```julia
    @rootonly begin
        worldid = add_agent!(sim, World())
        healthauthid = add_agent!(sim, HealthAuthority(0, 0, 0))
        @info "read persons"
        persons = read_persons!(config.synpop_file)
        @info "read events"
        read_events!(sim, config.events_all, persons, worldid, healthauthid)
        @info "finish init"
    end
```

Alternatively you could also check the `mpi.isroot` predicate.

It is important that [`finish_init!`](@ref) is called by all
processes and not only by the root process, and also some other
functions like e.g. [`write_snapshot`](@ref) must be called
collectively from all processes.

## Write a snapshot after `finish_init!`

If the process of generating the initial state is time-consuming and
the process is deterministic, it is advisable to save the state after
the [`finish_init!`](@ref) function completes. This can be achieved by
calling the [`write_snapshot`](@ref) function. Subsequently, instead
of recreating the initial graph for each simulation, you can read this
snapshot using the [`read_snapshot!`](@ref) function instead.

The [Vahana Episim
example](https://git.zib.de/sfuerst/vahana-episim/-/blob/main/src/episim.jl?ref_type=heads) serves as
a demonstration of how to implement this approach effectively.

## The :IgnoreSourceState hint

This hint requires additional explanation about what happens
internally when [`apply!`](@ref) is called.

In a parallel simulation, at the beginning of [`apply!`](@ref), it is
checked whether the transition function needs to read the state of an
agent type that may have changed since the last state read. If this is
the case, it checks all accessible edges (specified in the `read`
argument of `apply!`) and transmits the state of agents that can be
accessed (i.e., the agent type must also be included in the `read`
argument) to the corresponding process. However, there are cases where
it is clear that only the IDs of the agents will be accessed via a
specific edge type, and not the agent state itself. To avoid the
overhead of transmitting state between agents that will never be read,
the `:IgnoreSourceState` hint can be used for those edge types.

This hint only affects parallel simulations, as there is nothing to
transmit in a serial simulation.

## Avoid agentstate calls 

Instead of using [`agentstate`](@ref) to access the state of an agent,
sometimes it is possible for an agent to actively send the required
state associated with an edge to another agent that needs that
information. This can significantly improve performance, especially
when combined with the `:IgnoreSourceState` hint if necessary.

For example, in a Game of Life implementation, active cells can
generate an edge to their neighbors if the cell is active. If this
edge has the `:NumEdgesOnly` hint, it will directly trigger the
counting process of the neighbors without the need to transfer the
full state to other agents/processes.

## HPC Resources for German Researchers

For researchers at German universities interested in running
large-scale Vahana simulations, the NHR (National High Performance
Computing Alliance) offers access to high-performance computing
resources through several project categories.

Of particular interest to those new to HPC is the **NHR-Starter**
category. This one-time opportunity is designed for researchers
without prior experience in HPC resource applications. It provides:

- Simplified application process
- Limited HPC resources for one year
- Dedicated consulting and support
- Preparation for subsequent "Normal" or "Large" project applications

This path is especially valuable for researchers looking to scale up
their agent-based models using Vahana's parallel computing
capabilities without having extensive HPC background.

For more information about NHR resource allocation and application
procedures, visit the [NHR website](https://www.nhr-verein.de/rechnernutzung).
