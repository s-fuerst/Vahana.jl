#!/usr/bin/env bash

find .. -name "*.cov" -type f -delete

echo "Single Threaded"
echo "==============="
julia --code-coverage=user --project  runtests.jl

echo
echo "MPI"
echo "==="
echo "Core"
echo "----"
mpirun -n 4 julia --code-coverage=user --project mpi/test_core.jl 

echo 
echo "Agents"
echo "------"
mpirun -n 4 julia --code-coverage=user --project mpi/test_remove_agents.jl 

echo 
echo "Edgetypes"
echo "---------"
mpirun -n 4 julia --code-coverage=user --project mpi/test_edgetypes.jl 

echo
echo "EdgesIterator"
echo "-------------"
mpirun -n 4 julia --code-coverage=user --project mpi/test_edgesiterator.jl 

echo
echo "Independent"
echo "============="
mpirun -n 4 julia --code-coverage=user --project mpi/test_independent.jl 

echo
echo "Raster"
echo "------"
mpirun -n 4 julia --code-coverage=user --project mpi/test_raster.jl 

echo
echo "Agentstate (non shared memory)"
echo "------------------------------"
mpirun -n 4 julia --code-coverage=user --project mpi/test_agentstate.jl 

echo
echo "HDF5 Snapshot"
echo "-------------"
rm -r h5
mpirun -n 4 julia --code-coverage=user --project hdf5_snapshot.jl 

echo
echo "HDF5 Merge"
echo "----------"
julia --code-coverage=user --project hdf5_merge.jl 

echo
echo "Coverage result"
echo "---------------"
julia --project calc_coverage.jl
