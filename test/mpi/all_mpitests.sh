#!/bin/bash

mpirun -n $1 julia test_core.jl &
mpirun -n $1 julia test_edgesiterator.jl &
mpirun -n $1 julia test_edgetypes.jl &
mpirun -n $1 julia test_raster.jl 
