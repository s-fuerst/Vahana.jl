#!/bin/bash

echo "Core"
echo "===="
mpirun -n 4 julia --project mpi/test_core.jl 

echo 
echo "Agents"
echo "======"
mpirun -n 4 julia --project mpi/test_remove_agents.jl 

echo 
echo "Edgetypes"
echo "========="
mpirun -n 4 julia --project mpi/test_edgetypes.jl 

echo
echo "EdgesIterator"
echo "============="
mpirun -n 4 julia --project mpi/test_edgesiterator.jl 

echo
echo "Independent"
echo "============="
mpirun -n 4 julia --project mpi/test_independent.jl 

echo
echo "Raster"
echo "======"
mpirun -n 4 julia --project mpi/test_raster.jl 

echo
echo "HDF5 Snapshot"
echo "============="
mpirun -n 4 julia --project hdf5_snapshot.jl 

echo
echo "HDF5 read agents/edges"
echo "======================"
mpirun -n 4 julia --project hdf5_read_into_vector.jl 

echo
echo "HDF5 Merge"
echo "============="
julia --project hdf5_merge.jl 

