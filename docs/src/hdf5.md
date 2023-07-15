```@meta
CurrentModule = Vahana
```

# HDF5 data storage

Vahana uses the Hierarchical Data Format version 5 (HDF5) as file
format to store simulations to disc, utilizing the HDF5.jl
libary. HDF5.jl again uses a C library, which is either installed with
HDF5.jl or can be provided by the system. Please check the [HDF5.jl
documentation](https://juliaio.github.io/HDF5.jl/stable/#Installation)
for details.

If the provided library supports Parallel HDF5, this will be used
automatically. Using Parallel HDF5 has the advantage that all
processes can write to a single file, without Parallel HDF5 multiple
files are created for a single (parallel) simulation (but the Vahana
API is the same in any case, so for the user this difference is only
visible when looking into the h5 directory with a file manager or via
the shell). But in the current Vahana version using Parallel HDF5 has
the disadvantage, that the files are not compressed (see also
[`set_compression`](@ref)).

## Write 

To write into a HDF5 file, they are `attached` to a Vahama
simulation. Normally this happens automatically when the first time a
write function like [`write_snapshot`](@ref) is called. All following
`write_*` calls then add additional datasets to the file.

```@docs
write_snapshot
create_h5file!
close_h5file!
```
Beside `write_snapshot` there exists also some more fine grained write
functions:

```@docs
write_agents
write_edges
write_globals
```

## Read

In the normal use case we call just `write_snapshot(sim, "snapshot
description")` (assuming sim is a Vahama simulation). To read such a
snapshot, we can then run another Script that creates the same model
(see [`create_model`](@ref)) and simulation (see
[`create_simulation`](@ref)) and then call
`read_snapshot!(sim)`. [`read_snapshot!`](@ref) can read also a
parallel simulation into a single (REPL) process, then the distributed
graph is merged into a single one.

```@docs
read_snapshot!
```

Also here exists also some more fine grained read functions:

```
read_params
read_globals
read_agents!
read_edges!
```

## Transition 

All `read_*` functions have a keyword called transition. If this is
not set, the last stored data of a type is always read, but via this
keyword it is also possible to read the previous state of the
simulation (or a part of it) (assuming it was written multiple times,
of course).

Vahana counts internally how many times the function [`apply!`](@ref)
is called (in the current Vahana implementation this is stored in a
field called `num_transitions` of the simulation). When a new dataset
is created by a `write_*` call, this information is stored with the
dataset.

When `read_snapshot!` is called, Vahana looks for the highest
`num_transition` that is less than or equal to the transition keyword
for the types to be read. Since the default value for the argument is
typemax(Int64), the newest dataset is read by default.

For snapshots the `list_snapshots` function returns a list of all
stored snapshots in the file.

```@docs
list_snapshots
```

## Restrictions and Workarounds

The exact datastructs that can be stored and read from a HDF5 depends
in the HDF5.jl implementation. E.g. before v0.16.15 Tuples where not
supported. 

The following functions are workarounds for two current restrictions.

```@docs
create_namedtuple_converter
create_enum_converter
```

## Example Model

TODO DOC link to Vahana Episim

