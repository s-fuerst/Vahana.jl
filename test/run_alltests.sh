#!/usr/bin/env bash

echo "Single Threaded"
echo "==============="
julia --project runtests.jl

echo
echo "MPI"
echo "==="
echo "Core"
echo "----"
mpiexecjl -n 4 julia --project mpi/test_core.jl 

echo 
echo "Agents"
echo "------"
mpiexecjl -n 4 julia --project mpi/test_remove_agents.jl 

echo 
echo "Edgetypes"
echo "---------"
mpiexecjl -n 4 julia --project mpi/test_edgetypes.jl 

echo
echo "EdgesIterator"
echo "-------------"
mpiexecjl -n 4 julia --project mpi/test_edgesiterator.jl 

echo
echo "Independent"
echo "-----------"
mpiexecjl -n 4 julia --project mpi/test_independent.jl 

echo
echo "Raster"
echo "------"
mpiexecjl -n 4 julia --project mpi/test_raster.jl 

echo
echo "Agentstate (non shared memory)"
echo "------------------------------"
mpiexecjl -n 4 julia --project mpi/test_agentstate.jl 

echo
echo "HDF5 Snapshot"
echo "-------------"
rm -r h5
mpiexecjl -n 4 julia --project hdf5_snapshot.jl 

echo
echo "HDF5 Merge"
echo "----------"
julia --project hdf5_merge.jl 
