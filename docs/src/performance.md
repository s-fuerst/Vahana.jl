# Performance Tuning

TODO DOC Einleitung

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

## Type Properties

By default, the data structures used internally to hold the graph in
memory are the most flexible. However, it is possible to give Vahana
some hints about the properties of the agents or edges to improve
runtime performance and/or reduce memory requirements. For example, if
the number of agents in a simulation is fixed, the `:Vector` property
(TODO: this will be renamed in a later version) can be set in the
[`register_agenttype!`](@ref) call. In this case, Vahana internally uses a
`Vector` to store the agents instead of a `Dict`.

TODO Gute Idee ohne properties anzufangen, und dann properties
dazuzunehmen.  Abgeshen von random gen (agenten können z.b. in
unterschiedlicher reihenfolge aufgerufen werden) sollte dies das
Ergebnis nicht verändert.

<!-- There is no need for additional changes to the model code, the -->
<!-- interface to Vahana will be still the same, with the restriction, that -->
<!-- some functions are not available anymore for edge types with some . E.g. when the -->
<!-- `:IgnoreFrom` property is set for an edge type, it's not possible to -->
<!-- call the [`neighborstates`](@ref) function, as Vahana didn't stored -->
<!-- the id of the neighbors and therefore can not retrive their state. -->

### Agent Types

TODO DOC: The current implementation will probably change in the parallel
version, so we ignore the agent type properties for now.

### Edge Types

TODO DOC: For the edge types gibt es 4 properties, diese haben auch
einen Einfluß auf das Interface
