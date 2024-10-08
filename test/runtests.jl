# for whatever reason there is a pipline error when MPI.Init() is called before
# run(`mpiexec ...`), so first the mpi versions of the tests are called, before
# the single threaded versions. 
#include("mpi.jl")

using Vahana

using Test

enable_asserts(true)

suppress_warnings(true)

include("core.jl")

include("remove_agents.jl")

include("addexisting.jl")

include("edges.jl")
runedgestest()

#edgesiterator depends on edges.jl
include("edgesiterator.jl")

include("parametric_types.jl")

include("independent.jl")

# depends on core
include("globals.jl")

include("raster.jl")

include("graphs.jl")


