# Performance Tuning

## Optional Assertions

Vahana comes with some internal consistency checks, at the cost of
run-time performance (e.g., in [`agentstate`](@ref) there are checks
that the specified agenttype matches the agent's ID, which creates of
course some overhead). The assertions that could degrade the run
performance can be disabled by calling `enable_assserts(false)`.

The recommended approach is therefore to leave the assertions enabled
during the development of the model, but to disable them when the
model goes "into production", e.g. before the start of a parameter
space exploration. 

## Type Traits

By default, the data structures used internally to hold the graph in
memory are the most flexible. However, it is possible to give Vahana
some hints about the traits of the agents or edges to improve
runtime performance and/or reduce memory requirements. For example, if
the number of agents in a simulation is fixed, the `:Vector` trait
(TODO: this will be renamed in a later version) can be set in the
[`register_agenttype!`](@ref) call. In this case, Vahana internally uses a
`Vector` to store the agents instead of a `Dict`.

!!! warning

	Currently only the edge traits have a (hopefully) stable implementation,
	the API for agent traits will change in one of the nexts version
	of Vahana

In general, there is no need for other changes to the model code. The
interface to Vahana remains the same, with the restriction that not
all functions may be available. For example, if the `:IgnoreFrom`
trait is set for an edge type, it is not possible to call the
[`edgestates`](@ref) function because Vahana has not stored the id
of the neighbors and therefore cannot retrieve their state.

In general, a sensible approach is to leave the assertions active and
ignore the traits during the model implementation. When the
implementation is finished, the first thing to do is to check which
traits are set and how they affected the performance. After a choice
has been made here that does not cause any assertions, these can then
also be deactivated.

### Agent Traits

TODO DOC: The current implementation will probably change in the parallel
version, so we ignore the agent type properties for now.

### Edge Traits

There exist four possible traits for the edge types, that can be 
set as optional [`register_edgestatetype!`](@ref) arguments:

- `:IgnoreFrom`: The ID of the source node is not stored. 
- `:Stateless`: Store only the ID of the source node. 
- `:SingleType`: All target nodes have the same type (the source nodes can be still have different types).
- `:SingleEdge`: Each agent can be the target node for max. one edge (of
  this type).

All of the traits can be combined in any combination. The combination
`:IgnoreFrom` and `:Stateless` may look useless, as then nothing from
the concrete edge will be stored when [`add_edge!`](@ref) is called,
but in this case Vahana still counts for each agent how many edges
exists with the agent as target node. Often this (or just the
information that such an edge exist) is sufficient information, see
the `Die` or `Eat` edges of the [Predator example](predator.md) for an
example.

!!! danger

	The combination `:SingleType` and `:SingleEdge` is dangerous,
	because in this case Vahana always returns an edge (or part of an
	edge) in the corresponding methods, even if no edge with the agent as
	target node has been added to the graph.
	
	This both traits should only be combined when it is guaranteed that to
	each agent actually exactly one edge leads. Or when also `:IgnoreFrom`
	and `:Stateless` is set, in this case it's only tracked if there exists an
	edge to an agent (see also [Defined Functions](#Defined-Functions) below).
	
	If you combine `:SingleType` and `:SingleEdge` without
	`:IgnoreFrom` and `:Stateless`, you will get a warning when
	`register_edgestatetype!` is called . This warning can be suppressed by
	calling `suppress_warnings(true)` after importing Vahana.

#### :SingleType `target` keyword argument 

When `:SingleType` is set it is necessary to add to
[`register_edgestatetype!`](@ref) the `target` keyword argument. The value of
this argument must be the type of the target nodes. In the case that
it's known how many agents of this type exists, this can be also given
via the optional keyword `size`.

#### Defined Functions

The following functions can be used to access the graph in
[`apply!`](@ref), but the availability of this functions
depends on the edge type traits:

| function                                  | not available for edge type with the trait (combination) |
|:------------------------------------------|:---------------------------------------------------------|
| [`edges`](@ref)                        | `:IgnoreFrom` or `:Stateless`                            |
| [`edgeids`](@ref)                     | `:IgnoreFrom`                                            |
| [`edgestates`](@ref)                  | `:IgnoreFrom` or `:SingleEdge`                           |
| [`edgestates`](@ref), [`mapreduce`](@ref) | `:Stateless`                                             |
| [`num_edges`](@ref)                   | `:SingleEdge`                                            |
| [`has_edge`](@ref)                    | see below                                                |

The function [`has_edge`](@ref) has a complicated rule. This
function is not available for edge types with the trait combination
`:SingleType` and `:SingleEdge`, except that the edge type also
has the traits `:IgnoreFrom` and `:Stateless`. In this case
`has_edge` can be called, and is even more the only available
function from the list above.

#### Special Trait Combinations

Two property combinations can also be set in
[`register_edgestatetype!`](@ref) via a single symbol that directly
expresses the intent of the combination:

- `:NumEdgesOnly`: This corresponds to the combination `:IgnoreFrom`
  and `:Stateless`, in this case only the number of edges is counted
  and therefore only calls to [`num_edges`](@ref) and
  [`has_edge`](@ref) are possible.

- `:HasEdgeOnly`: This corresponds to the combination `:IgnoreFrom`,
  `:Stateless` and `:SingleEdge`, in this case only calls to
  [`has_edge`](@ref) are possible.


## apply! argument keywords

The optional keywords `invariant_compute` and `add_existing` can be
used to reuse already existing instances of agents and/or edges. The
`find_prey` and `try_reproduce` transition functions of the
[Predator/Prey model] are an example of this. Or check the
documentation of [`apply!`](@ref) for more detail.
