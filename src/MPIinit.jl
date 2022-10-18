using MPI

export mpi

mutable struct VMPI
    comm
    rank::Int64
    size::Int64
    isroot::Bool
    active::Bool
    # shm stands for shared memory
    shmcomm
    shmrank::Int64
    shmsize::Int64
    # node
    node::Int64
end

const mpi = VMPI(nothing, 0, 0, false, false, nothing, 0, 0, 0)

function mpiinit() 
    MPI.Init(;threadlevel = :single)

    mpi.comm = MPI.COMM_WORLD
    mpi.rank = MPI.Comm_rank(mpi.comm)
    mpi.size = MPI.Comm_size(mpi.comm)

    mpi.isroot = mpi.rank == 0
    mpi.active = mpi.size > 1

    mpi.shmcomm = MPI.Comm_split_type(mpi.comm, MPI.COMM_TYPE_SHARED, 0)
    mpi.shmrank = MPI.Comm_rank(mpi.shmcomm)
    mpi.shmsize = MPI.Comm_size(mpi.shmcomm)

    mpi.node = floor(mpi.rank / mpi.shmsize) |> Int64
end

