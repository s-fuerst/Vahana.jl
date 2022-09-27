using Test

nprocs_str = get(ENV, "JULIA_MPI_TEST_NPROCS", "")
nprocs = nprocs_str == "" ? clamp(Sys.CPU_THREADS, 2, 4) : parse(Int, nprocs_str)

testdir = string(@__DIR__) * "/mpi"

istest(f) = endswith(f, ".jl") && startswith(f, "test_")
testfiles = sort(filter(istest, readdir(testdir)))

@testset "$f" for f in testfiles
    cmd(n=nprocs) = `mpiexec -n $n $(Base.julia_cmd()) $(joinpath(testdir, f))`
    #cmd(n=nprocs) = `mpirun -n 4 ls`
    run(cmd(2))
    @test true
end


