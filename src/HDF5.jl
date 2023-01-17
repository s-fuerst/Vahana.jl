using MPI

using HDF5

export write_globals, write_agents, write_snapshot

import Base.convert

convert(::Type{NamedTuple{(:x, :y), Tuple{Int64, Int64}}}, ci::CartesianIndex{2}) =
    (x = ci[1], y = ci[2])


# this is called in new_simulation
function open_h5file(sim::Simulation, filename = sim.name, readwrite::String = "w")
    fid = if HDF5.has_parallel()
        _log_info(sim, "Create hdf5 file in parallel mode")
        HDF5.h5open(filename * ".h5", readwrite, mpi.comm, MPI.Info())
    else
        _log_info(sim, "Create hdf5 file without parallel mode")
        HDF5.h5open(filename * "_" * string(mpi.rank) * ".h5", readwrite)
    end

    HDF5.attributes(fid)["modelname"] = sim.model.name
    HDF5.attributes(fid)["modelhash"] = hash(sim.model)
    HDF5.attributes(fid)["simulationname"] = sim.name
    
    # write the parameters
    pid = create_group(fid, "params")
    for k in fieldnames(typeof(sim.params))
        if mpi.isroot
            pid[string(k)] = getfield(sim.params, k)
        end
    end

    gid = create_group(fid, "globals")
    for k in fieldnames(typeof(sim.globals))
        create_group(gid, string(k))
    end
    
    create_group(fid, "rasters")
    aid = create_group(fid, "agents")
    foreach(T -> create_group(aid, string(T)), sim.typeinfos.nodes_types)
    
    eid = create_group(fid, "edges")
    foreach(T -> create_group(eid, string(T)), sim.typeinfos.edges_types)

    fid
end

function write_globals(sim::Simulation, fields = fieldnames(typeof(sim.globals)))
    _log_time(sim, "write globals") do
        t = "t_" * string(sim.num_transitions)

        for k in fieldnames(typeof(sim.globals))
            if k in fields
                gid = sim.h5file["globals"][string(k)]
                HDF5.attributes(gid)["last_transition"] = sim.num_transitions
                gid[t] = getfield(sim.globals, k)
            end
        end
    end

    nothing
end

function write_agents(sim::Simulation, types::Vector{DataType} = sim.typeinfos.nodes_types)
# foreigndied    last_change    mpiwindows     read           shmstate
# foreignstate   last_transmit  nextid         shmdied        write

# read: died reuseable state

    _log_time(sim, "write agents") do
        t = "t_" * string(sim.num_transitions)

        for T in types
            field = getproperty(sim, Symbol(T))
            num_agents = if !has_trait(sim, T, :Immortal, :Agent)
                field.nextid - 1
            else
                length(field.read.died)
            end

            vec_space = dataspace((num_agents,))

            
            tid = create_group(sim.h5file["agents"][string(T)], t)
            for pe in 0:(mpi.size-1)
                peid = create_group(tid, "pe_" * string(pe))
                if ! has_trait(sim, T, :Immortal, :Agent)
                    if pe == mpi.rank 
                        peid["died"] = field.read.died
                    elseif HDF5.has_parallel()
                        create_dataset(peid, "died", HDF5.Datatype(Bool), vec_space)
                    end
                end
                if ! has_trait(sim, T, :Stateless, :Agent)
                    if pe == mpi.rank 
                        peid["state"] = field.read.state
                    elseif HDF5.has_parallel()
                        dset = create_dataset(peid, "state",
                                              HDF5.Datatype(HDF5.hdf5_type_id(T)),
                                              vec_space)
                    end
                end
            end
        end
    end

    nothing
end


function write_snapshot(sim::Simulation)
    fid = sim.h5file

    HDF5.attributes(fid)["last_snapshot"] = sim.num_transitions
end



