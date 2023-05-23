using MPI

using HDF5
using H5Zblosc

import NamedTupleTools: ntfromstruct, structfromnt

export create_h5file!, open_h5file, close_h5file!
export write_globals, write_agents, write_edges, write_snapshot
export read_params, read_globals, read_agents!, read_edges!, read_snapshot!
export list_snapshots
export Pos, Pos2D, Pos3D
export create_namedtuple_struct_converter
# hdf5 does not allow to store (unnamed) tuples, but the CartesianIndex
# are using those. The Pos2D/3D types can be used to add a Cell position
# to an agent state, with automatical conversion from an Cartesianindex.
Pos2D = NamedTuple{(:x, :y), Tuple{Int64, Int64}}
Pos3D = NamedTuple{(:x, :y, :z), Tuple{Int64, Int64, Int64}}

Pos(x, y) = (x = x, y = y)
Pos(x, y, z) = (x = x, y = y, z = z)

import Base.convert
convert(::Type{Pos2D}, ci::CartesianIndex{2}) = (x = ci[1], y = ci[2])

convert(::Type{Pos3D}, ci::CartesianIndex{3}) = (x = ci[1], y = ci[2], z = ci[3])

convert(::Type{CartesianIndex{2}}, pos::Pos2D) = CartesianIndex(pos[1], pos[2])

convert(::Type{CartesianIndex{3}}, pos::Pos3D) = CartesianIndex(pos[1], pos[2], pos[3])

# HDF5.jl does not support enumerations. We convert them to their
# integer type. As we (unsafe) cast the read data to the struct while
# reading, it is not necessary to convert the integer back to an
# enumeration explicitly.
import HDF5.hdf5_type_id
hdf5_type_id(::Type{T}, ::Val{false}) where {I, T <: Enum{I}} = hdf5_type_id(I)

# TODO: this does not work as expected, write a stable hash function
import Base.hash
hash(model::Model) = hash(model.types.edges_attr) +
    hash(model.types.edges_types) +
    hash(model.types.nodes_attr) +
    hash(model.types.nodes_types) +
    hash(model.types.nodes_type2id) 

transition_str(sim) = "t_$(sim.num_transitions-1)"

parallel_write() = HDF5.has_parallel() && mpi.active

mpio_mode() = parallel_write() ? Dict(:dxpl_mpio => :collective) : Dict()

function create_h5file!(sim::Simulation, filename = sim.filename; overwrite = sim.overwrite_file)
    #in the case that the simulation is already attached to a h5file, we relase it first
    close_h5file!(sim)
    
    @assert sim.initialized "You can only write initialized simulations"
    if endswith(filename, ".h5")
        filename = filename[1, end-3]
    end
    
    filename = mkpath("h5") * "/" * filename

    if ! overwrite
        filename = add_number_to_file(filename)
        # to avoid that rank 0 creates a file before other ranks check this
        MPI.Barrier(MPI.COMM_WORLD)
    end
    
    fid = if parallel_write()
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
    # @info "wrote hash" attrs(fid)["modelhash"] hash(sim.model)
    attrs(fid)["fileformat"] = 1
    attrs(fid)["mpisize"] = mpi.size
    attrs(fid)["mpirank"] = mpi.rank
    attrs(fid)["HDF5parallel"] = HDF5.has_parallel()
    attrs(fid)["initialized"] = sim.initialized
    
    pid = create_group(fid, "params")
    if sim.params !== nothing
        for k in fieldnames(typeof(sim.params))
            field = getfield(sim.params, k)
            if typeof(field) <: Array
                pid[string(k)] = field
                attrs(pid[string(k)])["array"] = true
            else
                pid[string(k)] = [ field ]
                attrs(pid[string(k)])["array"] = false
            end
        end
    end
    
    create_group(fid, "globals")

    rid = create_group(fid, "rasters")
    _log_time(sim, "write rasters") do
        for raster in keys(sim.rasters)
            rid[string(raster)] = sim.rasters[raster]
        end
    end       

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
        attrs(tid)[":SingleType"] =
            has_trait(sim, T, :SingleType)
        attrs(tid)[":IgnoreSourceState"] =
            has_trait(sim, T, :IgnoreSourceState)
    end

    create_group(fid, "snapshots")
    
    fid
end

function close_h5file!(sim::Simulation)
    if sim.h5file === nothing
        return
    end

    _log_time(sim, "close h5file") do
        close(sim.h5file)
    end

    sim.h5file = nothing

    nothing
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

    gid = sim.h5file["globals"]
    if haskey(HDF5.attributes(gid), "last_change") &&
        sim.globals_last_change == attrs(gid)["last_change"]
        return
    end
    attrs(gid)["last_change"] = sim.globals_last_change
    
    _log_time(sim, "write globals") do
        t = transition_str(sim)
        tid = create_group(gid, t)

        for k in fieldnames(typeof(sim.globals))
            if k in fields
                field = getfield(sim.globals, k)
                if typeof(field) <: Array
                    tid[string(k)] = field
                    attrs(tid[string(k)])["array"] = true
                else
                    tid[string(k)] = [ field ]
                    attrs(tid[string(k)])["array"] = false
                end
            end
        end
    end

    flush(sim.h5file)
    
    nothing
end

function new_dset(gid, name, T, sum_size, converted, create_datatype = true)
    dt = if create_datatype
        HDF5.Datatype(HDF5.hdf5_type_id(T))
    else
        T
    end

    # We have seperate files, so we don't need the size of all agents/edges
    # but only the size of the array we want to write into this file
    if ! parallel_write()
        sum_size = length(converted)
    end
    
    ds = if create_datatype
        dataspace((sum_size,))
    else
        sum_size
    end

    if config.compression_level == 0 || config.no_parallel_compression
        create_dataset(gid, name, dt, ds; mpio_mode()...)
    else
        chunk_size = if length(converted) == 0
            1
        else
            HDF5.heuristic_chunk(converted)[1]
            # On a parallel system I did run into problems with the
            # heuristic chunk but for whatever reason it worked with
            # half the size. As I had additional esoteric problems
            # like this I disabled the compression by default,
            # see also VahanaConfig.no_parallel_compression.
            # Int(ceil(HDF5.heuristic_chunk(converted)[1]/2))
        end

        create_dataset(gid,
                       name,
                       dt,
                       ds;
                       chunk=(chunk_size,),
                       shuffle=(),
                       compress=config.compression_level,
                       mpio_mode()...)
    end
end

function calc_displace(num)
    vec_num = MPI.Allgather(num, mpi.comm)
    vec_displace = 
        if parallel_write()
            # we use pop to remove the overall sum of agents, which
            # is not used as an offset
            d = [0; cumsum(vec_num)]
            pop!(d)
            d
        else
            # in the non parallel case there are no offsets at all
            # as each rank write into an own file
            fill(0, mpi.size)
        end
    (vec_num, vec_displace)
end

function write_agents(sim::Simulation,
               types::Vector{DataType} = sim.typeinfos.nodes_types)
    if sim.h5file === nothing 
        create_h5file!(sim)
    end

    _log_time(sim, "write agents") do
        t = transition_str(sim)

        for T in types
            gid = sim.h5file["agents"][string(T)]

            field = getproperty(sim, Symbol(T))
            if haskey(attrs(gid), "last_change") &&
                field.last_change == attrs(gid)["last_change"]
                continue
            end
            attrs(gid)["last_change"] = field.last_change

            num_agents = Int64(field.nextid - 1)
            (vec_num_agents, vec_displace) = calc_displace(num_agents)
            sum_num_agents = sum(vec_num_agents)

            tid = create_group(gid, t)
            attrs(tid)["last_change"] = field.last_change
            attrs(tid)["size_per_rank"] = vec_num_agents
            attrs(tid)["displace"] = vec_displace

            if sum_num_agents > 0
                if parallel_write()
                    start = vec_displace[mpi.rank + 1] + 1
                    last = start + vec_num_agents[mpi.rank + 1] - 1
                else
                    start = 1
                    last = num_agents
                end
                if ! has_trait(sim, T, :Immortal, :Agent)
                    dset = new_dset(tid, "died", Bool,
                                    sum_num_agents, field.read.died, false)
                    dset[start:last] = field.read.died
                end
                if fieldcount(T) > 0
                    dset = new_dset(tid, "state", T,
                                    sum_num_agents, field.read.state)
                    dset[start:last] = field.read.state
                end
            end
        end
    end

    flush(sim.h5file)

    nothing
end

# We need to convert the Edges depending of the edge traits to
# different structs that will be then saved to the HDF5 file

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

struct IgnoreFromEdge{T}
    to::AgentID
    state::T
end

_neighbors_only(sim, T) = (has_trait(sim, T, :Stateless) ||
    fieldcount(T) == 0) && has_trait(sim, T, :IgnoreFrom)


function write_edges(sim::Simulation,
              types::Vector{DataType} = sim.typeinfos.edges_types)

    if sim.h5file === nothing
        create_h5file!(sim)
    end

    _log_time(sim, "write edges") do
        t = transition_str(sim)

        for T in types
            gid = sim.h5file["edges"][string(T)]

            field = getproperty(sim, Symbol(T))
            if haskey(attrs(gid), "last_change") &&
                field.last_change == attrs(gid)["last_change"]
                continue
            end
            attrs(gid)["last_change"] = field.last_change

            # prepare_read! triggers the distributions of the edges to the
            # correct nodes
            prepare_read!(sim, Vector{DataType}(), T)

            edges = if _neighbors_only(sim, T)
                getproperty(sim, Symbol(T)).read
            else
                edges_iterator(sim, T) |> collect
            end

            num_edges = length(edges)
            (vec_num_edges, vec_displace) = calc_displace(num_edges)
            sum_num_edges = sum(vec_num_edges)

            # what we write depends on :Stateless, :IgnoreFrom
            # and :SingleType
            (converted, DT, create_dt) = if _neighbors_only(sim, T)
                if has_trait(sim, T, :SingleType)
                    (edges, eltype(edges), false)
                else
                    ([EdgeCount(to, count) for (to, count) in edges], EdgeCount, true)
                end
            elseif fieldcount(T) == 0
                if has_trait(sim, T, :Stateless)
                    (map(e -> StatelessEdge(e[1], e[2]), edges),
                     StatelessEdge, true)
                else
                    (map(e -> StatelessEdge(e[1], e[2].from), edges),
                     StatelessEdge, true)
                end
            elseif has_trait(sim, T, :IgnoreFrom)
                (map(e -> IgnoreFromEdge(e[1], e[2]), edges),
                 IgnoreFromEdge{T}, true)
            else     
                (map(e -> CompleteEdge(e[1], e[2]), edges),
                 CompleteEdge{T}, true)
            end

            tid = if length(converted) > 0
                new_dset(gid, t, DT, sum_num_edges, converted, create_dt)
            else
                new_dset(gid, t, DT, sum_num_edges, Vector{DT}(), create_dt)
            end
            
            attrs(tid)["last_change"] = field.last_change
            attrs(tid)["size_per_rank"] = vec_num_edges
            attrs(tid)["displace"] = vec_displace

            if parallel_write()
                start = vec_displace[mpi.rank + 1] + 1
                last = start + vec_num_edges[mpi.rank + 1] - 1
            else
                start = 1
                last = num_edges
            end

            if length(converted) > 0
                tid[start:last] = converted
            else
                tid[start:last] = Vector{DT}()
            end

            finish_read!(sim, T)
        end
    end

    flush(sim.h5file)

    nothing
end

function write_snapshot(sim::Simulation, comment::String = ""; ignore = [])
    ignore = applicable(iterate, ignore) ? ignore : [ ignore ]

    if sim.h5file === nothing
        create_h5file!(sim)
    end

    fid = sim.h5file
    gid = create_group(fid["snapshots"], transition_str(sim))
    attrs(gid)["comment"] = comment
    attrs(gid)["ignored"] = string(ignore)

    attrs(fid)["last_snapshot"] = sim.num_transitions

    if sim.globals !== nothing
        write_globals(sim)
    end

    write_agents(sim, filter(t -> !(t in ignore), sim.typeinfos.nodes_types))
    write_edges(sim, filter(t -> !(t in ignore), sim.typeinfos.edges_types))
end

# Returns a vector of fids (h5 file handles). The size of the vector
# is equal to mpi.size of the simulation run that created the file(s).
# In the case, that the file was stored using h5parallel, all elements
# of the vector point to the same file. So
# fid[mpi.rank][...][pe_mpi.rank] access always the data that was stored
# by mpi.rank, independent if h5parallel was used or not.
function open_h5file(sim::Union{Simulation, Nothing}, filename)
    # first we sanitize the filename
    if endswith(filename, ".h5")
        filename = filename[1:end-3]
    end
    if endswith(filename, "_0")
        filename = filename[1:end-2]
    end
    if ! (isfile(filename * ".h5") || isfile(filename * "_0.h5"))
        filename = mkpath("h5") * "/" * filename
        if ! (isfile(filename * ".h5") || isfile(filename * "_0.h5"))
            println("""
                No hdf5 file(s) for $(filename) found in $(pwd()) or $(pwd())/h5
            """)
            return []
        end
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
        if sim !== nothing
            _log_info(sim, "Open hdf5 file in parallel mode")
        end
        [ HDF5.h5open(f, "r+", mpi.comm, MPI.Info()) for f in filenames ]
    else
        if sim !== nothing
            _log_info(sim, "Open hdf5 file without parallel mode")
        end
        [ HDF5.h5open(f, "r+") for f in filenames ]
    end

    # TODO improve hash function
    # @assert hash(sim.model) == attrs(fids[1])["modelhash"] """
    # The file was written for a different model than that of the given simulation.
    # """
    h5mpisize = attrs(fids[1])["mpisize"]
    if mpi.size > 1
        @assert mpi.size == h5mpisize """
            The file can only be read with 1 or $(h5mpisize) processes
        """
    end

    length(fids) > 1 ? fids : fill(fids[1], h5mpisize)
end

function find_transition_nr(ids, transition::Int)
    if length(ids) == 0
        return nothing
    end
    ts = filter(<=(transition), map(s -> parse(Int64, s[3:end]), keys(ids)))
    if length(ts) == 0
        return nothing
    end
    maximum(ts)
end

function find_transition_group(ids, transition::Int)
    trnr = find_transition_nr(ids, transition)
    if trnr === nothing
        nothing
    else
        ids["t_$(trnr)"]
    end
end

function _read_agents_restore!(sim::Simulation, field, tid, T::DataType)
    with_logger(sim) do
        @debug("<Begin> _read_agents_restore!", agenttype=T)
    end
    
    vec_num_agents = attrs(tid)["size_per_rank"]
    vec_displace = attrs(tid)["displace"]
    size = vec_num_agents[mpi.rank + 1]
    start = vec_displace[mpi.rank + 1] + 1
    last = start + vec_num_agents[mpi.rank + 1] - 1
    
    field.nextid = size + 1

    if ! has_trait(sim, T, :Immortal, :Agent)
        if size > 0
            field.write.died = HDF5.read(tid["died"],
                                         Bool,
                                         start:last)
        else
            field.write.died = Vector{Bool}()
        end
    end
    if fieldcount(T) > 0
        if size > 0
            field.write.state = HDF5.read(tid["state"],
                                          T,
                                          start:last)
        else
            field.write.state = Vector{T}()
        end    
    else
        field.write.state = fill(T(), size)
    end

    # this move the state to the shared memory (and to read) 
    finish_write!(sim, T)

    # We must also refresh the list of reuseable ids
    empty!(field.read.reuseable)
    if ! has_trait(sim, T, :Immortal, :Agent)
        for i in 1:size
            if field.read.died[i]
                push!(field.read.reuseable, i)
            end
        end
    end
    
    _log_debug(sim, "<End> _read_agents_restore!")
end

# merge distributed graph into a single one.
function _read_agents_merge!(sim::Simulation, tid, T::DataType, fidx, parallel)
    with_logger(sim) do
        @debug("<Begin> _read_agents_merge!", agenttype=T, fidx)
    end
    idmapping = Dict{AgentID, AgentID}()

    typeid = sim.typeinfos.nodes_type2id[T]

    vec_num_agents = attrs(tid)["size_per_rank"]
    vec_displace = attrs(tid)["displace"]
    
    size = parallel ? sum(vec_num_agents) : vec_num_agents[fidx]

    died = if ! has_trait(sim, T, :Immortal, :Agent) 
        HDF5.read(tid["died"], Bool)
    else
        fill(false, size)
    end
    state = if fieldcount(T) > 0
        HDF5.read(tid["state"], T)
    else
        fill(T(), size)
    end

    if parallel
        # we must reconstruct the original indices of the agents as we
        # have them now in a long list.
        for rank in 1:length(vec_num_agents)
            agents_on_rank = vec_num_agents[rank]
            offset = vec_displace[rank]
            for i in 1:agents_on_rank
                if !died[i + offset]
                    newid = add_agent!(sim, state[i + offset])
                    push!(idmapping,
                          agent_id(typeid, rank-1, AgentNr(i)) => newid)
                end
            end
        end
    else
        for i in 1:size
            if !died[i]
                newid = add_agent!(sim, state[i])
                push!(idmapping,
                      agent_id(typeid, fidx-1, AgentNr(i)) => newid)
            end
        end
    end

    _log_debug(sim, "<End> _read_agents_merge!")
    
    idmapping
end

function _read_globals_or_params(hid, T)
    unsorted = Dict(map(keys(hid)) do k
                        k => if attrs(hid[k])["array"]
                            hid[k][]
                        else
                            hid[k][1]
                        end
                    end)
    
    map(n -> unsorted[string(n)], fieldnames(T))
end

function read_params(name::String, T::DataType)
    fids = open_h5file(nothing, name)
    if length(fids) == 0
        return
    end

    values = _read_globals_or_params(fids[1]["params"], T)
    
    foreach(close, fids)

    T(values...)
end

read_params(sim, T::DataType) =
    read_params(sim.filename, T)

read_params(sim, nr::Int64, T::DataType) =
    read_params(add_number_to_file(sim.filename, nr), T)

function read_globals(name::String, T::DataType; transition = typemax(Int64))
    fids = open_h5file(nothing, name)
    if length(fids) == 0
        return
    end

    values = _read_globals_or_params(find_transition_group(fids[1]["globals"],
                                                           transition),
                                     T)

    foreach(close, fids)

    T(values...)
end

read_globals(sim, T::DataType; transition = typemax(Int64)) =
    read_globals(sim.filename, T; transition)

read_globals(sim, nr::Int64, T::DataType; transition = typemax(Int64)) =
    read_globals(add_number_to_file(sim.filename, nr), T; transition)

function read_agents!(sim::Simulation,
               name::String = sim.filename;
               transition = typemax(Int64),
               types::Vector{DataType} = sim.typeinfos.nodes_types)
    fids = open_h5file(sim, name)
    if length(fids) == 0
        return
    end

    original_size = attrs(fids[1])["mpisize"]
    parallel = attrs(fids[1])["HDF5parallel"]
    merge = original_size > mpi.size
    @assert !merge || mpi.size == 1
    
    fidxs = merge && ! parallel ? (1:original_size) : (mpi.rank+1:mpi.rank+1)

    sim.intransition = true

    idmapping = Dict{AgentID, AgentID}()

    _log_time(sim, "read agents") do
        for T in types
            field = getproperty(sim, Symbol(T))

            if merge
                # in prepare_write we have an immutable check that we
                # must disable by setting sim.initialized temporary to false
                was_initialized = sim.initialized
                sim.initialized = false
                prepare_write!(sim, false, T) 
                sim.initialized = was_initialized
            end

            for fidx in fidxs
                gid = fids[fidx]["agents"]
                
                tid = find_transition_group(gid[string(T)], transition)

                if tid === nothing && fidx == 1
                    @rootonly @info """
                        for $T nothing was written before transition $(transition)
                    """ 
                    continue
                end
                
                if merge
                    merge!(idmapping,
                           _read_agents_merge!(sim, tid, T, fidx, parallel))
                else
                    _read_agents_restore!(sim, field, tid, T)
                end
            end

            if merge
                finish_write!(sim, T)
            end
            
            # must be after the _read calls, as they are modifying this also
            field.last_change = find_transition_nr(fids[1]["agents"][string(T)],
                                                   transition)
            for ET in sim.typeinfos.edges_types
                field.last_transmit[ET] = -1
            end
        end
    end
    
    foreach(close, fids)
    sim.intransition = false

    
    idmapping
end

read_agents!(sim::Simulation,
             nr::Int64;
             transition = typemax(Int64),
             types::Vector{DataType} = sim.typeinfos.nodes_types) = 
                 read_agents!(sim, add_number_to_file(sim.filename, nr);
                              transition, types)

function read_edges!(sim::Simulation,
              name::String = sim.filename;
              idmapfunc = identity,
              transition = typemax(Int64),
              types::Vector{DataType} = sim.typeinfos.edges_types)
    fids = open_h5file(sim, name)
    if length(fids) == 0
        return
    end
    @assert length(fids) == mpi.size || idmapfunc !== identity """
      read_edges! can be only merging a distributed graph when an idmap is given
    """

    original_size = attrs(fids[1])["mpisize"]
    parallel = attrs(fids[1])["HDF5parallel"]
    merge = original_size > mpi.size
    @assert !merge || mpi.size == 1
    
    fidxs = merge ? (1:original_size) : (mpi.rank+1:mpi.rank+1)
    
    # we call add_edge! but there is a check that this is only done
    # at initialization time or in a transition function
    sim.intransition = true
    
    with_logger(sim) do
        @info("<Begin> read edges", edgetype = T, transition = transition)
    end
    for T in types
        prepare_write!(sim, false, T)

        for fidx in fidxs
            tid = find_transition_group(fids[fidx]["edges"][string(T)],
                                        transition)
            
            if tid === nothing
                @rootonly @info """
                    for $T nothing was written before transition $(transition)
                """ 
                continue
            end

            _read_edges!(sim, tid, idmapfunc, T, fidx)
        end
        finish_write!(sim, T)

        trnr = find_transition_nr(fids[mpi.rank+1]["edges"][string(T)], transition)
        if trnr === nothing
            getproperty(sim, Symbol(T)).last_change = 0
        else
            getproperty(sim, Symbol(T)).last_change = trnr
        end
    end
    
    _log_info(sim, "<End> read edges")

    sim.intransition = false
    
    foreach(close, fids)
end

read_edges!(sim::Simulation,
            nr::Int64;
            idmapfunc = identity,
            transition = typemax(Int64),
            types::Vector{DataType} = sim.typeinfos.nodes_types) = 
                read_edges!(sim, add_number_to_file(sim.filename, nr);
                            idmapfunc, transition, types)


function _read_edges!(sim::Simulation, tid, idmapfunc, T, fidx)
    field = getproperty(sim, Symbol(T))
    vec_num_edges = attrs(tid)["size_per_rank"]
    vec_displace = attrs(tid)["displace"]
    size = sum(vec_num_edges)
    start = vec_displace[fidx] + 1
    last = start + vec_num_edges[fidx] - 1

    if _neighbors_only(sim, T)
        if has_trait(sim, T, :SingleType)
            AT = sim.typeinfos.edges_attr[T][:target]
            typeid = sim.typeinfos.nodes_type2id[AT]
            data = if has_trait(sim, T, :SingleEdge)
                HDF5.read(tid, Bool, start:last)
            else
                HDF5.read(tid, Int64, start:last)
            end
            for (idx, num_edges) in enumerate(data)
                newid = idmapfunc(agent_id(typeid, fidx - 1, AgentNr(idx)))
                newidx = agent_nr(newid)
                # TODO: we know the size at the beginning, or?
                if length(field.read) < newidx
                    resize!(field.write, newidx)
                end
                field.write[newidx] = num_edges
            end
        else
            # EdgeCount struct
            for ec in HDF5.read(tid, EdgeCount, start:last)
                field.write[idmapfunc(ec.to)] = ec.count
            end
        end
    elseif fieldcount(T) == 0
        # StatelessEdge struct
        for se in HDF5.read(tid, StatelessEdge, start:last)
            add_edge!(sim, idmapfunc(se.from), idmapfunc(se.to), T())
        end
    elseif has_trait(sim, T, :IgnoreFrom)
        # T struct
        # for add_edge! we need always a from id, even when it's ignored
        dummy = AgentID(0)
        for edge in HDF5.read(tid, IgnoreFromEdge{T}, start:last)
            add_edge!(sim, dummy, idmapfunc(edge.to), edge.state)
        end
    else
        if has_trait(sim, T, :SingleType)
            totype = sim.typeinfos.edges_attr[T][:target]
            totypeid = sim.typeinfos.nodes_type2id[totype]
            for ce in HDF5.read(tid, CompleteEdge{T}, start:last)
                add_edge!(sim,
                          idmapfunc(ce.edge.from),
                          idmapfunc(agent_id(totypeid,
                                             fidx - 1,
                                             ce.to)),
                          ce.edge.state)
            end   
        else
            for ce in HDF5.read(tid, CompleteEdge{T}, start:last)
                add_edge!(sim, idmapfunc(ce.edge.from), idmapfunc(ce.to),
                          ce.edge.state)
            end
        end
    end
end

function read_rasters!(sim::Simulation,
                name::String = sim.filename;
                idmapping)
    # the rasters are immutable arrays of agents_ids. So there
    # is no need to read them more then once
    if length(sim.rasters) > 0
        return
    end
    
    fids = open_h5file(sim, name)
    if length(fids) == 0
        return
    end

    rid = first(fids)["rasters"]

    for raster in keys(rid)
        sim.rasters[Symbol(raster)] =
            length(idmapping) == 0 ? rid[raster][] :
            map(id -> get(idmapping, id, 0), rid[raster][]) 
    end
    
    foreach(close, fids)
end

read_rasters!(sim::Simulation,
              nr::Int64;
              idmapping) = 
                  read_rasters!(sim, add_number_to_file(sim.filename, nr);
                                idmapping)

# returns false when snapshot not found
function read_snapshot!(sim::Simulation,
                 name::String = sim.filename;
                 transition = typemax(Int64),
                 writeable = false,
                 ignore_params = false)
    # First we free the memory allocated by the current state of sim
    _free_memory!(sim)

    fids = open_h5file(sim, name)
    if length(fids) == 0
        return false
    end
    sim.initialized = attrs(fids[1])["initialized"]
    sim.num_transitions = transition == typemax(Int64) ?
        attrs(fids[1])["last_snapshot"] : transition
    globals_last_change = find_transition_nr(fids[1]["globals"], transition)
    if globals_last_change !== nothing
        sim.globals_last_change = globals_last_change
    end
    foreach(close, fids)

    if ignore_params == false && sim.params !== nothing
        sim.params = read_params(name, typeof(sim.params))
    end

    if sim.globals !== nothing
        sim.globals = read_globals(name, typeof(sim.globals); transition)
    end
    
    idmapping = read_agents!(sim, name; transition = transition)

    if length(idmapping) > 0
        read_edges!(sim, name; transition = transition,
                    idmapfunc = (key) -> idmapping[key])
        read_rasters!(sim, name; idmapping)
    else
        read_edges!(sim, name; transition = transition)
        read_rasters!(sim, name; idmapping)
    end

    if writeable
        fids = open_h5file(sim, name)
        sim.h5file = fids[mpi.rank+1]
    end
    
    true
end

read_snapshot!(sim::Simulation,
               nr::Int64;
               transition = typemax(Int64),
               writeable = false,
               ignore_params = false) =
                   read_snapshot!(sim, add_number_to_file(sim.filename, nr);
                                  transition, writeable, ignore_params)


function list_snapshots(name::String)
    fids = open_h5file(nothing, name)
    sid = fids[1]["snapshots"]
    
    if length(keys(sid)) == 0
        return nothing
    end
    snapshots =
        map(t -> (parse(Int64, t[3:end]), attrs(sid[t])["comment"]), keys(sid))

    foreach(close, fids)

    snapshots
end

list_snapshots(sim::Simulation) = list_snapshots(sim.filename)

list_snapshots(sim::Simulation, nr::Int64) =
    list_snapshots(add_number_to_file(sim.filename, nr))


import Base.convert
function create_namedtuple_struct_converter(T::DataType)
    NT = NamedTuple{fieldnames(T), Tuple{fieldtypes(T)...}}
    @eval convert(::Type{$NT}, st::$T) = ntfromstruct(st)
    @eval convert(::Type{$T}, nt::$NT) = structfromnt($T, nt)
end
