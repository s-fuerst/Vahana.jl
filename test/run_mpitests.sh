#!/bin/bash

echo "Core"
echo "===="
mpirun -n 4 julia mpi/test_core.jl 

echo 
echo "Agents"
echo "======"
mpirun -n 4 julia mpi/test_remove_agents.jl 

echo 
echo "Edgetypes"
echo "========="
mpirun -n 4 julia mpi/test_edgetypes.jl 

echo
echo "EdgesIterator"
echo "============="
mpirun -n 4 julia mpi/test_edgesiterator.jl 

echo
echo "Raster"
echo "======"
mpirun -n 4 julia mpi/test_raster.jl 

echo
echo "HDF5 Snapshot"
echo "============="
mpirun -n 4 julia hdf5_snapshot.jl 

echo
echo "HDF5 Merge"
echo "============="
julia hdf5_merge.jl 
