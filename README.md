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
  optional support for parallel HDF5.

## Installation

You can install Vahana in Julia using the following command:

```
using Pkg
Pkg.add("Vahana")
```

If you want to run a simulation in parallel, it is useful to also read
the configuration/installation sections of the
[MPI.jl](https://juliaparallel.org/MPI.jl/stable/configuration/) and
[HDF5.jl](https://juliaio.github.io/HDF5.jl/stable/#Installation)
libraries.

## Documentation 

Full documentation, including tutorials, guides, and API references, 
is available [here](https://s-fuerst.github.io/Vahana.jl/).

A JuliaCon 2023 Pre-recorded Video about Vahana is available [here](https://www.youtube.com/watch?v=-318ec-kCBM).

## Acknowledgments

![mathplus logo](/mathplus.png)

This research has been partially funded under Germany’s Excellence
Strategy, MATH+ : The Berlin Mathematics Research Center (EXC-2046/1),
project no. 390685689

## License

Vahana is distributed under the MIT license. For more
information, please refer to the LICENSE file in this repository.

