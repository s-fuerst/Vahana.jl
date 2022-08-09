```@meta
CurrentModule = Vahana -->
```

# Introduction

Vahana is a framework for agent-based models where the models are
implemented as an extended graph dynamical system [^1] and
that is designed for use on HPC clusters. There are also similarities
to graph processing systems such as Pregel [^2] and
their vertex-centric approaches. However, Vahana is more flexible in
its application.

[^1]: Adiga, A., Kuhlman, C.J., Marathe, M.V. et al. Graphical dynamical systems and their applications to bio-social systems. [https://doi.org/10.1007/s12572-018-0237-6](https://doi.org/10.1007/s12572-018-0237-6)

[^2]: Grzegorz Malewicz, Matthew H. Austern, Aart J.C Bik, James C. Dehnert, Ilan Horn, Naty Leiser, and Grzegorz Czajkowski. 2010. Pregel: a system for large-scale graph processing. [https://doi.org/10.1145/1807167.1807184](https://doi.org/10.1145/1807167.1807184)

Vahana puts a particular focus on the network structures between
agents and other entities in the modelled system.
Nodes are representing the agents and may be of different types and
have a state that belongs to a type-specific state space.  Edges
between the agents are directed, if a transition of agent ``a``
depends on the state of agent ``b``, an edge from ``b`` to ``a`` is
needed. Edges may also have different types (represent different kinds
of interactions) and may have also a state.

The main graph for the interactions between agents is extended by two
additional layers; one for spatial locations, and one for specific
computational entities that need edges from and/or to (almost) every
node in the main interagent graph. As for the spatial dimension of
models, the spatial layer provides a discrete grid, whereby each cell
of the grid is also a node in the graph.  Therefore the cells of the
grid can have a state and transitions, too, and each agent can, but
need not, be assigned to cell of the grid via edges to the
corresponding node.

To model the system's dynamic evolution, we consider discrete time
steps. At each step ``t``, there is a set of nodes which can change
their own state and also add new nodes and edges.  The node's new
state at time ``t`` is computed based only on information from the
previous step.  Information from step ``t-1`` usually includes the
previous state of the agent itself, and may include states of adjacent
agents in the graph, possibly also the states of the respective edges
themselves. The underlying idea is that of a functional programming
approach: the transition cannot be implicitly affected by any other
mutable state or unintended side effects.

