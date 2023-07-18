# Performance Tuning

This section provides some explanations on how to fine-tune the
simulation performance.

These tips are specific to Vahana. Of course, all the generic
performance tips, such as writing `type-stable` functions, still
apply.

## Optional Assertions

Vahana includes internal consistency checks that can impact runtime
performance. For example, in [`agentstate`](@ref), there are checks to
ensure that the specified agenttype matches the agent's ID, which
incurs some overhead. These assertions that could degrade performance
can be disabled by calling `enable_asserts(false)`.

The recommended approach is to enable assertions during the model
development phase but disable them when the model goes "into
production," such as before starting a parameter space exploration.


## Type Hints

It is possible to provide Vahana with hints about the types of agents
or edges to improve runtime performance and/or reduce memory
requirements.

Apart from the `:SingleEdge` hint, there is generally no need to
change anything in the model code. The interface to Vahana remains the
same, with the restriction that not all functions may be
available. For example, if the `:IgnoreFrom` hint is set for an edge
type, it is not possible to call the [`edgestates`](@ref) function
because Vahana has not stored the ID of the neighbors and therefore
cannot retrieve their state.

In general, a sensible approach is to keep the assertions active and
ignore the hints (except for `:SingleEdge`, as it can make your code
simpler) during the implementation of the model. Once the
implementation is complete, the first step is to check which hints are
set and how they affect performance. After making a choice that does
not trigger any assertions, these can then be deactivated as well.

### Agent Hints

Currently, the only hint available for agent types is the `:Immortal`
hint. This hint informs Vahana that agents of this type will not be
removed during the simulation (for example, by returning `nothing` in
a transition function).

The advantage of using this hint is that Vahana doesn't need to check
or handle situations where these agents may be removed. This
simplifies the internal handling of such agents.

### Edge Hints

There exist four possible hints for the edge types, that can be 
set as optional [`register_edgetype!`](@ref) arguments:

- `:IgnoreFrom`: Omits storage of the source node's ID 
- `:Stateless`: Only stores the ID of the source node 
- `:SingleType`: All target nodes are of the same type (the source

nodes can have different types). This needs also the keyword `target` set (see below).
- `:SingleEdge`: Each agent can be the target of only one edge (of this type).
- `:IgnoreSourceState`: The ID of the source agent is not used to
  access the state of the agent with this ID.

The `:IgnoreSourceState` hint is primarily relevant when running a
simulation in parallel. You can find detailed information about this
hint [below](./performance.md#:IgnoreSourceState).

Almost all hints can be combined in any way you prefer. For example,
the combination of `:IgnoreFrom` and `:Stateless` may seem useless at
first because no concrete edge information will be stored when
[`add_edge!`](@ref) is called. However, Vahana still keeps track of
the number of edges that have the agent as the target node. This
information alone, or combined with the `:SingleEdge` hint, can often
be sufficient. You can refer to the `Die` or `Eat` edges in the
[Predator example](predator.md) for an illustration.

The only combination that is not allowed is `:SingleType` +
`:SingleEdge`, unless you also set `:Stateless` and `:IgnoreFrom`.


#### :SingleType `target` keyword argument 

When the `:SingleType` hint is set, it is necessary to add the
`target` keyword argument to the [`register_edgetype!`](@ref)
function. The value of this argument should be the type of the target
nodes.

#### Defined Functions

The following functions can be used to access the graph within
[`apply!`](@ref), but their availability depends on the edge type
hints:

| function                                  | not available for edge type with the hint (combination) |
|:------------------------------------------|:--------------------------------------------------------|
| [`edges`](@ref)                           | `:IgnoreFrom` or `:Stateless`                           |
| [`neighborids`](@ref)                     | `:IgnoreFrom`                                           |
| [`neighborstates`](@ref)                  | `:IgnoreFrom`                                           |
| [`edgestates`](@ref), [`mapreduce`](@ref) | `:Stateless`                                            |
| [`num_edges`](@ref)                       | `:SingleEdge`                                           |
| [`has_edge`](@ref)                        | always available                                        |

As mentioned earlier, the Vahana API differs slightly when the
`:SingleEdge` hint is used. This specifically affects the functions
listed here, with the exception of `num_edges`, `has_edges`, and
`mapreduce`. Normally, these functions return a vector containing
edges, IDs, or states (or `nothing`). However, when used in
combination with the `:SingleEdge` hint, they return only one edge,
ID, or state (or `nothing`), as the hint implies that there is at most
one edge of that type.

#### Special Hint Combinations

Additionally, there are two property combinations that can be set in
[`register_edgetype!`](@ref) using a single symbol, which directly
expresses the intended combination:

- `:NumEdgesOnly`: This corresponds to the combination of
  `:IgnoreFrom` and `:Stateless`. In this case, only the number of
  edges is counted, so only calls to [`num_edges`](@ref) and
  [`has_edge`](@ref) are possible.

- `:HasEdgeOnly`: This corresponds to the combination of
  `:IgnoreFrom`, `:Stateless`, and `:SingleEdge`. In this case, only
  calls to [`has_edge`](@ref) are possible.

## Parallel simulations

The best performance improvement can be achieved by computing a model
implemented with Vahana in parallel. This can be easily done by
starting the simulation with `mpirun` or `mpiexec`. For example:

```mpirun -np 4 julia hegselmann.jl```

The `-np` parameter indicates the number of processes/threads to be
used for the simulation.

This approach works without any additional changes in the model
code. However, there are ways to further optimize performance in a
parallel simulation:

### Partitioning

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
Example](https://git.zib.de/sfuerst/vahana-episim/-/blob/main/src/init.jl).

### @rootonly

MPI is based on a [single program, multiple data
(SPMD)](https://en.wikipedia.org/wiki/Single_program,_multiple_data)
model. As a consequence, up until the [`finish_init!`](@ref) call, all
processes execute the exact same instructions on the same
data. Therefore, if there are `n` processes, the complete graph is
generated `n` times.

Although this is not necessarily problematic since only the graph of
the root processes is distributed in [`finish_init!`](@ref), it can be
beneficial to construct the graph only on the root processes,
especially if files are being read during the initialization phase.

To execute instructions only on the root process, you can use the
`@rootonly` macro. For example, the Vahana Episim example includes the
following code before the [`finish_init!`](@ref) call:

```
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

But it is important that [`finish_init!`](@ref) is called by all
processes and not only by the root process.

### Write a snapshot after `finish_init!`

If creating the initial state takes time and the process is
deterministic, it is a good idea to save the state after
[`finish_init!`](@ref) with [`write_snapshot`](@ref) and read this snapshot with
[`read_snapshot!`](@ref) instead of recreating the graph
each time. 

Again, the [Vahana Episim
example](https://git.zib.de/sfuerst/vahana-episim/) demonstrates how
to do this.

### :IgnoreSourceState hint

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

### Avoid agentstate calls 

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
