# Vahana - A framework (not only) for large-scale agent-based models 

Vahana is a high-performance, agent-based modelling library developed
in Julia, specifically (but not only) designed for large-scale
simulations of complex social network. It leverages the power of Graph
Dynamical Systems, coupled with advanced parallel computing
methodologies, to provide a robust framework for the exploration of
network-based systems.

## Key Features

- Efficient and Scalable: Vahana uses Message Passing Interface (MPI)
  for distributed memory parallelism, allowing for efficient scaling
  across thousands of cores.

- Expressive Interface: Despite its high-performance capabilities,
  Vahana is designed with an accessible and expressive interface,
  making the construction of complex models intuitive and simple.

- Network-Oriented Approach: Vahana's design focuses on network
  dynamics, making it an excellent tool for simulations involving
  complex interactions and dependencies between agents.

- Graph Dynamical Systems: Leveraging the principles of Graph
  Dynamical Systems, Vahana provides a solid theoretical foundation
  for modelling agent-based systems.

- Integration into the Julia Ecosystem: Vahana interacts seamlessly
  with other Julia libraries, such as DataFrames and Graphs.

- HDF5 Data Persistence: For efficient data storage and access, Vahana
  utilises the Hierarchical Data Format version 5 (HDF5), including
  optional support for parallel HDF5 for more extensive simulations.

## Installation

You can install Vahana in Julia using the following command:

```
using Pkg
Pkg.add("Vahana")
```

## Documentation 

Github Pages are in preperation, but the documentation is already available in the `docs` subfolder.

<!-- Full documentation, including tutorials, guides, and API references, -->
<!-- is available at [Your Documentation Website Link Here]. -->

# License

Vahana is distributed under the MIT license. For more
information, please refer to the LICENSE file in this repository.

