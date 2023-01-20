using MPI

using Base: typesof
using HDF5

export create_h5file!, close_h5file!
export write_globals, write_params, write_agents, write_edges, write_snapshot
export open_h5file, read_agents!

import Base.convert

convert(::Type{NamedTuple{(:x, :y), Tuple{Int64, Int64}}}, ci::CartesianIndex{2}) =
    (x = ci[1], y = ci[2])

convert(::Type{NamedTuple{(:x, :y), Tuple{Int64, Int64}}}, ci::CartesianIndex{3}) =
    (x = ci[1], y = ci[2], z = ci[3])

import Base.hash
hash(model::Model) = hash(model.types.edges_attr) +
    hash(model.types.edges_types) +
    hash(model.types.nodes_attr) +
    hash(model.types.nodes_types) +
    hash(model.types.nodes_type2id) 

# this is called in new_simulation
function create_h5file!(sim::Simulation, filename = sim.filename)
    if endswith(filename, ".h5")
        filename = filename[1, end-3]
    end
    
    fid = if HDF5.has_parallel()
        _log_info(sim, "Create hdf5 file in parallel mode")
        HDF5.h5open(filename * ".h5", "w", mpi.comm, MPI.Info())
    else
        _log_info(sim, "Create hdf5 file without parallel mode")
        HDF5.h5open(filename * "_" * string(mpi.rank) * ".h5", "w")
    end

    sim.h5file = fid

    attrs(fid)["simulationname"] = sim.name
    attrs(fid)["modelname"] = sim.model.name
    attrs(fid)["modelhash"] = hash(sim.model)
    attrs(fid)["fileformat"] = 1
    attrs(fid)["mpisize"] = mpi.size
    attrs(fid)["mpirank"] = mpi.rank
    attrs(fid)["HDF5parallel"] = HDF5.has_parallel()
    
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
    if sim.h5file === nothing
        create_h5file!(sim)
    end

    if sim.params !== nothing
        for k in fieldnames(typeof(sim.params))
            write(sim.h5file["params"][string(k)], getfield(sim.params, k))
        end
    end
end

function write_globals(sim::Simulation,
                fields = sim.globals === nothing ?
                    nothing : fieldnames(typeof(sim.globals)))
    if sim.h5file === nothing 
        create_h5file!(sim)
    end

    if fields === nothing
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

function write_agents(sim::Simulation,
               types::Vector{DataType} = sim.typeinfos.nodes_types)
    if sim.h5file === nothing 
        create_h5file!(sim)
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
       create_h5file!(sim)
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
       create_h5file!(sim)
    end

    _log_time(sim, "write rasters") do
        for raster in rasters
            sim.h5file["rasters"][string(raster)] = sim.rasters[raster]
        end
    end       
end

function write_snapshot(sim::Simulation)
    if sim.h5file === nothing
       create_h5file!(sim)
    end

    fid = sim.h5file

    attrs(fid)["last_snapshot"] = sim.num_transitions

    if sim.globals !== nothing
        write_globals(sim)
    end
    
    write_agents(sim)
    write_edges(sim)
end

function open_h5file(sim::Simulation, filename)
    # first we sanitize the filename
    if endswith(filename, ".h5")
        filename = filename[1:end-3]
    end
    if endswith(filename, "_0")
        filename = filename[1:end-2]
    end
    # we can have a single file written in parallel mode, or mpi.size files
    # written without parallel mode. In the later case, the filenames
    # end with _0, _1 ... _mpi.size-1
    parallel = isfile(filename * ".h5")
    filenames::Vector{String} = if parallel
        [ filename * ".h5" ]
    else
        # first we open file _0 to get mpi.size
        fid = HDF5.h5open(filename * "_0.h5", "r")
        h5mpisize = attrs(fid)["mpisize"]
        close(fid)
        [ filename * "_$(i).h5" for i in 0:(h5mpisize-1) ]
    end

    # In the case that we have seperate files for each process, we don't
    # need to open the files in parallel mode
    fids = if HDF5.has_parallel() && parallel && mpi.size > 1
        _log_info(sim, "Open hdf5 file in parallel mode")
        [ HDF5.h5open(f, "r", mpi.comm, MPI.Info()) for f in filenames ]
    else
        _log_info(sim, "Open hdf5 file without parallel mode")
        [ HDF5.h5open(f, "r") for f in filenames ]
    end

    @assert hash(sim.model) == attrs(fids[1])["modelhash"] """
       The file was written for a different model than that of the given simulation.     
    """
    h5mpisize = attrs(fids[1])["mpisize"]
    if mpi.size > 1
        @assert mpi.size == h5mpisize """
            The file can only be read with 1 or $(h5mpisize) processes
        """
    end

    length(fids) > 1 ? fids : fill(fids[1], h5mpisize)
end


function find_transition_group(ids, transition::Int)
    if length(ids) == 0
        return (nothing, nothing)
    end
    ts = filter(<=(transition), map(s -> parse(Int64, s[3:end]), keys(ids)))
    if length(ts) == 0
        return (nothing, nothing)
    end
    last_transition = maximum(ts)
    (ids["t_$(last_transition)"], last_transition)
end

function find_agent_peid(fid, transition::Int, rank, T::DataType)
    (trid, last_transition) =
        find_transition_group(fid["agents"][string(T)], transition)
    if trid === nothing
        (nothing, nothing)
    else
        (trid["pe_$(rank)"], last_transition)
    end
end

function _read_agents_restore!(sim::Simulation, fids, transition, T::DataType)
    (peid, last_transition) =
        find_agent_peid(fids[mpi.rank + 1], transition, mpi.rank, T)

    if peid === nothing
        @info "Found no data for $T which was written before transition $(transition)"
        return sim
    end

    with_logger(sim) do
        @debug("<Begin> _read_agents_restore!",
               agenttype=T, transition=transition)
    end
    
    field = getproperty(sim, Symbol(T))

    for ET in sim.typeinfos.edges_types
        field.last_transmit[ET] = -1
    end
    
    size = attrs(peid)["array_size"]
    field.nextid = size + 1

    if ! has_trait(sim, T, :Immortal, :Agent)
        if size > 0
            field.write.died = peid["died"][]
        else
            field.write.died = Vector{Bool}()
        end
    end
    if fieldcount(T) > 0
        if size > 0
            field.write.state = Vector{T}(undef, size)
            filestate = peid["state"][]
            @assert sizeof(filestate[1]) == sizeof(T)
            # we know that T is a bitstype, so this allows to "cast" the type
            # from the tuple we get from the HDF5 library to the type T,
            # assuming that each field is stored and read with the correct type
            Base._memcpy!(field.write.state, filestate, size * sizeof(T))
        end    
    end

    # this move the state to the shared memory (and to read) 
    finish_write!(sim, T)

    empty!(field.read.reuseable)
    if ! has_trait(sim, T, :Immortal, :Agent)
        for i in 1:size
            if field.read.died[i]
                push!(field.read.reuseable, i)
            end
        end
    end
    
    _log_debug(sim, "<End> _read_agents_restore!")

    sim
end


# merge distributed graph into a single one

function _read_agents_merge!(sim::Simulation, fids, transition, T::DataType)                    
end


function read_agents!(sim::Simulation,
               fids::Vector{HDF5.File},
               transition = typemax(Int64),
               types::Vector{DataType} = sim.typeinfos.nodes_types)
    _log_time(sim, "read agents") do
        merge::Bool = length(fids) > mpi.size
        @assert !merge || mpi.size == 1
        
        for T in types
            if merge
                _read_agents_merge!(sim, fids, transition, T)
            else
                _read_agents_restore!(sim, fids, transition, T)
            end
        end
    end
end
