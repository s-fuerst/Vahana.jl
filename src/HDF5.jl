using MPI

using HDF5

export create_h5file!, close_h5file!
export write_globals, write_params, write_agents, write_edges, write_snapshot

import Base.convert

convert(::Type{NamedTuple{(:x, :y), Tuple{Int64, Int64}}}, ci::CartesianIndex{2}) =
    (x = ci[1], y = ci[2])

convert(::Type{NamedTuple{(:x, :y), Tuple{Int64, Int64}}}, ci::CartesianIndex{3}) =
    (x = ci[1], y = ci[2], z = ci[3])


# this is called in new_simulation
function create_h5file!(sim::Simulation, filename = sim.name, readwrite::String = "w")
    fid = if HDF5.has_parallel()
        _log_info(sim, "Create hdf5 file in parallel mode")
        HDF5.h5open(filename * ".h5", readwrite, mpi.comm, MPI.Info())
    else
        _log_info(sim, "Create hdf5 file without parallel mode")
        HDF5.h5open(filename * "_" * string(mpi.rank) * ".h5", readwrite)
    end

    sim.h5file = fid

    attrs(fid)["modelname"] = sim.model.name
    attrs(fid)["modelhash"] = hash(sim.model)
    attrs(fid)["simulationname"] = sim.name
    attrs(fid)["fileformat"] = 1
    
    # write the parameters
    pid = create_group(fid, "params")
    for k in fieldnames(typeof(sim.params))
        create_dataset(pid, string(k), typeof(getfield(sim.params, k)), 1)
    end
    write_params(sim)
    
    gid = create_group(fid, "globals")
    for k in fieldnames(typeof(sim.globals))
        create_group(gid, string(k))
    end

    create_group(fid, "rasters")
    write_rasters(sim)

    aid = create_group(fid, "agents")
    foreach(sim.typeinfos.nodes_types) do T
        tid = create_group(aid, string(T))
        attrs(tid)[":Immortal"] = has_trait(sim, T, :Immortal, :Agent)
    end    

    eid = create_group(fid, "edges")
    foreach(sim.typeinfos.edges_types) do T
        tid = create_group(eid, string(T))
        attrs(tid)[":Stateless"] = has_trait(sim, T, :Stateless)
        attrs(tid)[":IgnoreFrom"] = has_trait(sim, T, :IgnoreFrom)
        attrs(tid)[":SingleEdge"] = has_trait(sim, T, :SingleEdge)
        attrs(tid)[":SingleAgentType"] =
            has_trait(sim, T, :SingleAgentType)
        attrs(tid)[":IgnoreSourceState"] =
            has_trait(sim, T, :IgnoreSourceState)

    end

    fid
end

function close_h5file!(sim::Simulation)
    if sim.h5file === nothing
        return
    end

    _log_time(sim, "close h5file") do
        close(sim.h5file)
    end

    nothing
end

function write_params(sim::Simulation)
    if sim.h5file !== nothing && sim.params !== nothing
        for k in fieldnames(typeof(sim.params))
            write(sim.h5file["params"][string(k)], getfield(sim.params, k))
        end
    end
end

function write_globals(sim::Simulation,
                fields = sim.globals === nothing ?
                    nothing : fieldnames(typeof(sim.globals)))
    if sim.h5file === nothing || fields === nothing
        return
    end
    
    _log_time(sim, "write globals") do
        t = "t_" * string(sim.num_transitions)

        for k in fieldnames(typeof(sim.globals))
            if k in fields
                gid = sim.h5file["globals"][string(k)]
                attrs(gid)["last_transition"] = sim.num_transitions
                gid[t] = getfield(sim.globals, k)
            end
        end
    end

    nothing
end

function write_agents(sim::Simulation, types::Vector{DataType} = sim.typeinfos.nodes_types)
    if sim.h5file === nothing
        return
    end

    _log_time(sim, "write agents") do
        t = "t_" * string(sim.num_transitions)

        for T in types
            gid = sim.h5file["agents"][string(T)]
            attrs(gid)["last_transition"] = sim.num_transitions

            field = getproperty(sim, Symbol(T))
            num_agents = Int64(field.nextid - 1)
            vec_num_agents = MPI.Allgather(num_agents, mpi.comm)

            tid = create_group(gid, t)
            for pe in 0:(mpi.size-1)
                peid = create_group(tid, "pe_" * string(pe))
                if ! has_trait(sim, T, :Immortal, :Agent)
                    create_dataset(peid, "died", Bool, vec_num_agents[pe+1])
                    if pe == mpi.rank 
                        peid["died"][:] = field.read.died
                    end
                end
                if fieldcount(T) > 0
                    create_dataset(peid, "state",
                                   HDF5.Datatype(HDF5.hdf5_type_id(T)),
                                   dataspace((Int64(vec_num_agents[pe+1]),)))
                    if pe == mpi.rank 
                        peid["state"][:] = field.read.state
                    end
                end
                if pe == mpi.rank
                    attrs(peid)["array_size"] = num_agents
                end
            end
        end
    end

    nothing
end

# We need to convert the Edges depending of the edge traits to different structs
# that will be then saved to the HDF5 file

struct CompleteEdge{T}
    to::AgentID
    edge::Edge{T}
end

struct StatelessEdge
    to::AgentID
    from::AgentID
end

struct EdgeCount
    to::AgentID
    count::Int64
end

function write_edges(sim::Simulation, types::Vector{DataType} = sim.typeinfos.edges_types)
    if sim.h5file === nothing
        return
    end

    neighbors_only(T) = (has_trait(sim, T, :Stateless) || fieldcount(T) == 0) &&
        has_trait(sim, T, :IgnoreFrom)
    
    _log_time(sim, "write edges") do
        t = "t_" * string(sim.num_transitions)

        for T in types
            gid = sim.h5file["edges"][string(T)]
            attrs(gid)["last_transition"] = sim.num_transitions

            # prepare_read! triggers the distributions of the edges to the
            # correct nodes
            prepare_read!(sim, Vector{DataType}(), T)

            edges = if neighbors_only(T)

                getproperty(sim, Symbol(T)).read
            else
                edges_iterator(sim, T) |> collect
            end

            num_edges = length(edges)
            vec_num_edges = MPI.Allgather(num_edges, mpi.comm)

            tid = create_group(gid, t)
            for pe in 0:(mpi.size-1)
                pename = "pe_" * string(pe)
                # what we write depends on :Stateless and :IgnoreFrom,
                # so we have four different cases 
                peid = if neighbors_only(T)
                    if has_trait(sim, T, :SingleAgentType)
                        converted = edges
                        create_dataset(tid, pename, eltype(edges), vec_num_edges[pe+1])
                    else
                        converted = [ EdgeCount(to, count) for (to, count) in edges ]
                        create_dataset(tid, pename,
                                       HDF5.Datatype(HDF5.hdf5_type_id(EdgeCount)),
                                       dataspace((Int64(vec_num_edges[pe+1]),)))
                    end
                elseif fieldcount(T) == 0
                    converted = map(e -> StatelessEdge(e[1], e[2].from), edges)
                    create_dataset(tid, pename,
                                   HDF5.Datatype(HDF5.hdf5_type_id(StatelessEdge)),
                                   dataspace((Int64(vec_num_edges[pe+1]),)))
                elseif has_trait(sim, T, :IgnoreFrom)
                    converted = map(e -> e[2], edges)
                    create_dataset(tid, pename,
                                   HDF5.Datatype(HDF5.hdf5_type_id(T)),
                                   dataspace((Int64(vec_num_edges[pe+1]),)))
                else     
                    converted = map(e -> CompleteEdge(e[1], e[2]), edges)
                    create_dataset(tid, pename,
                                   HDF5.Datatype(HDF5.hdf5_type_id(CompleteEdge{T})),
                                   dataspace((Int64(vec_num_edges[pe+1]),)))
                end
                if pe == mpi.rank 
                    tid[pename][:] = converted
                end
            end

            finish_read!(sim, T)
        end
    end
end

function write_rasters(sim::Simulation,
                rasters = sim.rasters === nothing ? nothing : keys(sim.rasters))
    if sim.h5file === nothing
        return
    end

    _log_time(sim, "write rasters") do
        for raster in rasters
            sim.h5file["rasters"][string(raster)] = sim.rasters[raster]
        end
    end       
end

function write_snapshot(sim::Simulation)
    if sim.h5file === nothing
        return
    end

    fid = sim.h5file

    attrs(fid)["last_snapshot"] = sim.num_transitions

    if sim.globals !== nothing
        write_globals(sim)
    end
    
    write_agents(sim)
    write_edges(sim)
end
