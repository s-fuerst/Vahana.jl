using MPI

export mpi

mutable struct VMPI
    comm
    rank::Int
    size::Int
    isroot::Bool
    active::Bool
end

const mpi = VMPI(nothing, 0, 0, false, false)

function mpiinit() 
    MPI.Init(;threadlevel = :single)

    mpi.comm = MPI.COMM_WORLD
    mpi.rank = MPI.Comm_rank(mpi.comm)
    mpi.size = MPI.Comm_size(mpi.comm)

    mpi.isroot = mpi.rank == 0
    mpi.active = mpi.size > 1
end

