using Test

using Vahana

using MPI

enable_asserts(true)

suppress_warnings(true)

MPI.set_errorhandler!(MPI.COMM_WORLD, MPI.ERRORS_RETURN)

include("../core.jl")

