```@meta
CurrentModule = Vahana -->
```

# Introduction

Vahana.jl is an open-source high-performance software framework for
the development of large-scale agent-based models (ABM) of complex
social systems. Vahana is based on a discrete dynamical systems
formulation referred to as a synchronous graph dynamical system
(SyGDS)[^1], which is a generalization of cellular automata (CA). In a CA
the cells can be interpreted as the agents, and when a cell/agent
updates its state according to a rule, the new state depends only on
the current state of the cell and the states of the cells in its
neighborhood.  In a SyGDS, the vertices of the graph are the agents of
the model, and the (directed) edges determine the neighborhood.

Vahana extends the SyGDSs concept so that even complex models like the
Mobility Transition Model (https://github.com/CoeGSS-Project/motmo) or
MATSim Episim (https://github.com/matsim-org/matsim-episim-libs) can
be expressed as a SyGDS. The vertices are representing the agents and
may be of different types and have a state that belongs to a
type-specific state space.  Edges between the agents are directed, if
a transition of agent ``a`` depends on the state of agent ``b``, an
edge from ``b`` to ``a`` is needed. Edges may also have different
types (represent different kinds of interactions) and may have also a
state.  There can be multiple transition functions (which are the
equivalent of a rule in a CA), and each transition function can act on
a different subgraph.

(Discrete) spatial information can be added using Vahana functions
that insert grid cells as nodes in the graph and have access to a
mapping from the Cartesian index of the underlaying space to the
corresponding node. Since the cells are vertices of the graph, they
can be treated in the same way as other agents

To model the system's dynamic evolution, we consider discrete time
steps. At each step ``t``, there is a set of vertices which can change
their own state and also add new vertices and edges.  The new state of
a node at time ``t`` is computed based only on information from the
previous step.  Information from step ``t-1`` usually includes the
previous state of the agent itself, and may include states of adjacent
agents in the graph, possibly also the states of the respective edges
themselves. The underlying idea is that of a functional programming
approach: the transition cannot be implicitly affected by any other
mutable state or unintended side effects.

So expressing a model as a SyGDS has the advantage that a single
simulation can be computed in parallel. Vahana therefore allows the
simulation to be distributed across multiple nodes of a computer
cluster, enabling the simulation of large-scale models that do not fit
on a single node.

Parallelization is done through the Message Passing Interface (via the
MPI.jl package), but this is hidden to the user. Any model developed
with Vahana can be automatically computed in parallel by simply
starting the simulation via mpirun. The challenge for the user is
mainly to think about how to express the model as a SyGDS, rather than
thinking about technical details of the implementation such as which
data structure is best to use.

[^1]: Adiga, A., Kuhlman, C.J., Marathe, M.V. et al. Graphical dynamical systems and their applications to bio-social systems. [https://doi.org/10.1007/s12572-018-0237-6](https://doi.org/10.1007/s12572-018-0237-6)


