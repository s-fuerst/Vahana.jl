using Vahana
using Test

enable_asserts(true)

suppress_warnings(true)

#include("core.jl")

#include("edges.jl")
#runedgestest()

# edgesiterator depends on edges.jl
#include("edgesiterator.jl")

# include("globals.jl")

# include("raster.jl")

# include("graphs.jl")

include("mpi.jl")
