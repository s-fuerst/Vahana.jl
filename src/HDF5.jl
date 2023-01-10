using MPI

import  HDF5


# this is called in new_simulation
function h5open(sim::Simulation, filename = sim.name, readwrite::String = "w")
    if HDF5.has_parallel()
        _log_info(sim, "Create hdf5 file in parallel mode")
        HDF5.h5open(filename, readwrite, mpi.comm, MPI.Info())
    else
        _log_info(sim, "Create hdf5 file without parallel mode")
        HDF5.h5open(filename, readwrite)
    end
end
    
