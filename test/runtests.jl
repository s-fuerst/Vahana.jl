using Vahana
using Test

enable_asserts(true)

suppress_warnings(true)

include("core.jl")

# TODO, when we have MPI-Agents supported:
# - check that addexisting works for the default case
# - and that for immortal agents an exception is throws, when they are added to rebuild
#include("addexisting.jl")

include("edges.jl")
runedgestest()

# edgesiterator depends on edges.jl
include("edgesiterator.jl")

include("globals.jl")

include("raster.jl")

include("graphs.jl")

include("mpi.jl")
