# Performance Improvments

TODO DOC Einleitung

## Optional Assertions

Vahana comes with some internal consistency checks, at the cost of
run-time performance (e.g., in `agentstate` it checks if the specified
agenttype matches the agent's ID, which creates of course some
overhead as). The assertions that could degrade the run
performance can be disabled by calling `enable_asssertions(false)`.

The recommended approach is therefore to leave assertions enabled
during the development of the model, but then disable them when
the model goes "into production", e.g. before the start of a parameter
space exploration.

## Type Properties

The data structures used internally for keeping the graph in memory are per default the most flexible ones, 

### Agent Types

TODO DOC: The current implementation will probably change in the parallel
version, so we ignore the agent type properties for now.

### Edge Types

