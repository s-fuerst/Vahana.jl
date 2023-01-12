using MPI

using HDF5


# this is called in new_simulation
function open_h5file(sim::Simulation, filename = sim.name, readwrite::String = "w")
    fid = if HDF5.has_parallel()
        _log_info(sim, "Create hdf5 file in parallel mode")
        HDF5.h5open(filename, readwrite, mpi.comm, MPI.Info())
    else
        _log_info(sim, "Create hdf5 file without parallel mode")
        HDF5.h5open(filename, readwrite)
    end

    HDF5.attributes(fid)["modelname"] = sim.model.name
    HDF5.attributes(fid)["modelhash"] = hash(sim.model)
    HDF5.attributes(fid)["simulationname"] = sim.name
    
    # write the parameters
    pid = create_group(fid, "params")
    for k in fieldnames(typeof(sim.params))
        pid[string(k)] = getfield(sim.params, k)
    end
end
    
