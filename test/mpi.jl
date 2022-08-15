using MPI

using MPIPreferences

# We follow here the strategy of the MPI.jl to test MPI relevant code

nprocs_str = get(ENV, "JULIA_MPI_TEST_NPROCS", "")
nprocs = nprocs_str == "" ? clamp(Sys.CPU_THREADS, 2, 4) : parse(Int, nprocs_str)

testdir = string(@__DIR__) * "/mpi"

istest(f) = endswith(f, ".jl") && startswith(f, "test_")
testfiles = sort(filter(istest, readdir(testdir)))

testdir = "/home/fuerst/.julia/dev/Vahana/test/mpi"

@testset "$f" for f in testfiles
    mpiexec() do mpirun
        cmd(n=nprocs) = `$mpirun -n $n $(Base.julia_cmd()) $(joinpath(testdir, f))`
        run(cmd())
        @test true
    end
end


