# Performance Tuning

This section provides some explanations on how to fine-tune the
simulation performance.

These tips are specific to Vahana. Of course, all the generic
performance tips, such as writing `type-stable` functions, still
apply.

## Optional Assertions

Vahana uses internal consistency checks that come at the cost of
runtime performance. For example, [`agentstate`](@ref) verifies that 
the specified agent type matches the agent's ID, which creates overhead.
These performance-impacting assertions can be disabled by calling 
`enable_asserts(false)`.

The recommended workflow is to keep assertions enabled during model
development, but disable them for production runs, such as before
starting parameter space explorations.

Due to Julia's world age system, changes made by `enable_asserts()` 
will not take effect within the same function call where it is invoked.
The new assertion behavior becomes active only after returning to the
top level or in subsequently called functions. Call `enable_asserts()`
at the top level or ensure there's a "world age barrier" before the
assertions are expected to work.

## Type Hints

It is possible to provide Vahana with hints about the types of agents
or edges to improve the runtime performance and/or reduce memory
requirements. You can use either hint independently or combine them as
needed for your specific types.

Aside from the `:SingleEdge` hint, there is generally no requirement
to modify the model code when a type hint is introduced to a type. The
interface to Vahana remains consistent, with the restriction that
certain functions may be unavailable. For instance, if the
`:IgnoreFrom` hint is specified for an edge type, there is no
[`edgestates`](@ref) method available for this type, as Vahana has not
retained the identifiers of neighboring entities and, consequently,
cannot retrieve their corresponding states.

When implementing a model, it is a sensible approach to keep the
assertions active and disregard hints. Upon completing the
implementation, the next step should be to activated hints and
evaluate their impact on performance. Subsequently, after selecting
the best hint combination that does not trigger assertions, these can
be deactivated as well.

### Agent Hints

Vahana supports two hints for agent types: `:Immortal` and `:Independent`.
When an agent type is marked as `:Immortal`, it indicates that agents of
this type will never be removed from the simulation. By default,
Vahana assumes that agents can be removed (by returning
nothing in a transition function). The `:Immortal` hint allows Vahana to
skip certain checks and operations related to agent removal,
potentially improving performance.

The `:Independent` hint indicates that agents of this type do not access
the state of other agents of the same type during transition
functions. This information allows Vahana to optimize memory usage and
reduce unnecessary data copying.

### Edge Hints

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


Vahana supports five hints for edge types, which can be specified as
optional arguments in the [`register_edgetype!`](@ref) function:

- `:IgnoreFrom`: Excludes storage of the source node's ID.
- `:Stateless`: Stores only the source node's ID, omitting any
  additional state information.
- `:SingleType`: Indicates that all target nodes are of the same type,
  while source nodes can vary. This hint requires setting the target
  keyword (explained below).
- `:SingleEdge`: Specifies that each agent can be the target of at
  most one edge of this type. In the case that also the `:IgnoreFrom`
  and `:Stateless` hint are also set, multiple edges to an agent of
  this type can be created, but the only information available later
  is that at least one edge exists, as only the
  [has_edge](#Defined-Functions) is defined.
- `:IgnoreSourceState`: Indicates that the source agent's ID is not
  used to access its state. This hint is particularly relevant for
  parallel simulations and is explained
  [here](./performance.md#:IgnoreSourceState).

These hints can be combined flexibly to suit your model's needs. For
instance, combining `:IgnoreFrom` and `:Stateless` might seem
redundant as it stores no explicit edge information when add_edge! is
called. However, Vahana still maintains a count of edges targeting
each agent per type. E.g. the Die and Eat edges in the [Adding Spatial
Information tutorial](performance.md) only needs the information that
such an edge exists for an agent. Therefore for both types the
`:IgnoreFrom`, `:Stateless` and `:SingleEdge` (or the
[`:HasEdgeOnly`](#Special-Hint-Combinations)) hints could be set.

Most hint combinations are permissible, with one exception:
:SingleType and :SingleEdge cannot be used together unless :Stateless
and :IgnoreFrom are also specified.

#### :SingleType `target` keyword argument 

When the `:SingleType` hint is set, it is necessary to add the
`target` keyword argument to the [`register_edgetype!`](@ref)
function. The value of this argument should be the type of the target
agents.

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
`:SingleEdge` hint is set. This specifically affects the functions
listed here, with the exception of `num_edges`, `has_edges`, and
`mapreduce`. Normally, these functions return a vector containing
edges, IDs, or states (or `nothing`). However, when used in
combination with the `:SingleEdge` hint, they return only a single edge,
ID, or state (or `nothing`), as the hint implies that there can be at most
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
  
## Iterator Versions of Edge Functions

Vahana offers iterator versions of several functions used to access
edge and neighbor information, including [`neighborids_iter`](@ref),
[`edgestates_iter`](@ref), [`neighborstates_iter`](@ref), and
[`neighborstates_flexible_iter`](@ref). 

The iterator functions work similarly to their non-iterator
counterparts but return an iterator instead of a vector. This approach
can be more memory-efficient as it doesn't create a full vector in
memory. Instead, values are computed on-demand, which can be faster if
you don't need to process all elements. They work well with Julia's
built-in functions like map, filter, and reduce, allowing for
efficient chaining of operations.

