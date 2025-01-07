using MPI, HDF5, StaticArrays

import NamedTupleTools: ntfromstruct, structfromnt
import Dates: format, now

export create_h5file!, close_h5file!
export write_globals, write_agents, write_edges, write_snapshot
export read_params, read_globals, read_agents!, read_edges!, read_snapshot!
export read_agents, read_edges
export list_snapshots
export create_namedtuple_converter, create_enum_converter
export VString, create_string_converter
export write_metadata, read_metadata, write_sim_metadata, read_sim_metadata

# hdf5 does not allow to store (unnamed) tuples, but the CartesianIndex
# are using those. The Pos2D/3D types can be used to add a Cell position
# to an agent state, with automatical conversion from an Cartesianindex.

import Base.convert
import HDF5.hdf5_type_id
"""
    create_enum_converter()

The HDF5.jl library does not support Enums as fields of structs that
should be stored. This function add this support but as this involves
type piracy, this support must be enabled explicitly.
"""
function create_enum_converter()
    @eval hdf5_type_id(::Type{T}, ::Val{false}) where {I, T <: Enum{I}} = hdf5_type_id(I)

    @eval convert(::Type{T}, i::Int32) where { T <: Enum } = T(i)
end


VString{N} = SVector{N, UInt8}

"""
    create_string_converter(add_show_method::Bool = true)

The HDF5.jl library (version 0.17.2) does not support `InlineStrings`
or `StaticStrings` in structs. Standard `String`s are also not suitable
for agent and edge types, as these must be bits types.

To address this limitation, `create_string_converter` generates
gconversion methods between `String` and `SVector{N, UInt8}` instances
(from the `StaticArrays` package).  Here, N represents the maximum
number of bytes that can be stored, which for Unicode strings may
exceed the character count due to variable-length encoding.

For example, you could create a Person struct with a fixed-size name field:

```julia
struct Foo
    foo::SVector{20, UInt8}
end
```

You can then construct a `Foo` instance using a regular string:
`Foo("abc")`.

If `add_show_method` is set to `true` (the default), `show` methods are
also defined for these `SVector`s. To avoid confusion while working
with Julia's REPL, where the value might appear as a `String` while
actually being an `SVector`, the output includes "(as UInt8-Vector)"
after the value itself.

When `add_show_method` is set to `true` (which is the default behavior),
`show` methods are automatically defined for the `SVector` types. To
prevent potential confusion arising from the display of `SVector`
values as `String`s, the output is formatted to include the annotation
"::UInt8[]" following the value itself. 
"""
function create_string_converter(add_show_method::Bool = true)
    @eval function Base.convert(::Type{SVector{N, UInt8}}, str::String) where N
        cu = codeunits(str)
        if length(cu) > N
            cu = first(cu, N)
        end
        SVector{N, UInt8}([cu; fill(UInt8(0), N - length(cu))])
    end

    @eval function SVector{N, UInt8}(str::String) where N
        convert(SVector{N, UInt8}, str)
    end
    
    @eval Base.promote_rule(::Type{SVector{N, UInt8}}, ::Type{String}) where N =
        SVector{N, UInt8}

    @eval function Base.convert(::Type{String}, vec::SVector{N, UInt8}) where N
        String(vec)
    end

    @eval function Base.convert(::Type{String}, vec::SVector{N, UInt8}) where N
        rstrip(String(collect(vec)))
    end

    @eval Base.String(vec::SVector{N, UInt8}) where N = convert(String, vec)

    if add_show_method
        @eval function Base.show(io::IO, mime::MIME"text/plain", x::SVector{N, UInt8}) where N
            print(io, String(x) * "::UInt8[]")
        end
        @eval function Base.show(io::IO, x::SVector{N, UInt8}) where N
            print(io, String(x) * "::UInt8[]")
        end
        @eval function Base.print(io::IO, x::SVector{N, UInt8}) where N
            print(io, String(x))
        end
    end
end

# import Base.hash
# hash(model::Model) = hash(model.types.edges_attr) +
#     hash(model.types.edges_types) +
#     hash(model.types.nodes_attr) +
#     hash(model.types.nodes_types) +
#     hash(model.types.nodes_type2id) 

transition_str(sim) = "t_$(sim.num_transitions-1)"

parallel_write() = HDF5.has_parallel() && mpi.active

mpio_mode() = parallel_write() ? Dict(:dxpl_mpio => :collective) : Dict()

# since fileformat 2, empty_array and last_change starts with an _
empty_array_string(fids) = attrs(fids[1])["fileformat"] == 1 ?
    "empty_array" : "_empty_array"

last_change_string(fids) = attrs(fids[1])["fileformat"] == 1 ?
    "last_change" : "_last_change"


# in the case that write_metadata is called before the file is created
# we store the information in the vector, so that write_metadata
# can be also called before the simulation is initialized.
_preinit_meta = Tuple{Union{Symbol, DataType}, Symbol, Symbol, Any}[]
# the same for the simulation
_preinit_meta_sim = Tuple{Symbol, Any}[]
"""
    create_h5file!(sim::Simulation, [filename = sim.filename; overwrite = sim.overwrite_file])

The canonical way to create an HDF5 file is to call one of the
`write_` functions like [`write_snapshot`](@ref). If `sim` does not
already have an HDF5 file attached, such a file will then be created
automatically using the filename specified as keyword in
[`create_simulation`](@ref) or, if this keyword was not given, the
model name. But sometime it can be useful to control this manually,
e.g. after a call to [`copy_simulation`](@ref).

The `filename` argument can be used to specify a filename other than
`sim.filename`. If `overwrite` is true, existing files with this name
will be overwritten. If it is false, the filename is automatically
extended by an increasing 6-digit number, so that existing files are
not overwritten.

By default, the files are created in an `h5` subfolder, and this is
created in the current working directory. However, the path can also be
set with the function `set_hdf5_path`.

In the case that an HDF5 file was already created for the simulation
`sim`, this will be closed.

`create_h5file!` can be only called after [`finish_init!`](@ref)

See also [`close_h5file!`](@ref), [`write_agents`](@ref),
[`write_edges`](@ref), [`write_globals`](@ref),
[`read_agents!`](@ref), [`read_edges!`](@ref), [`read_globals`](@ref),
[`read_snapshot!`](@ref) and [`list_snapshots`](@ref)
"""
function create_h5file!(sim::Simulation, filename = sim.filename; overwrite = sim.overwrite_file)
    #in the case that the simulation is already attached to a h5file, we relase it first
    close_h5file!(sim)
    
    @assert sim.initialized "You can only write initialized simulations"
    if endswith(filename, ".h5")
        filename = filename[1, end-3]
    end

    filename = if config.hdf5_path !== nothing
        mkpath(config.hdf5_path)  * "/" * filename
    else
        mkpath("h5") * "/" * filename
    end

    if ! overwrite
        filename = add_number_to_file(filename)
        # to avoid that rank 0 creates a file before other ranks check this
        MPI.Barrier(MPI.COMM_WORLD)
    end

    fid = if parallel_write()
        _log_info(sim, "Create hdf5 file in parallel mode")
        filename = filename * ".h5"
        rm(filename; force = true)
        HDF5.h5open(filename, "w", mpi.comm, MPI.Info())
    else
        _log_info(sim, "Create hdf5 file without parallel mode")
        filename = if mpi.active
            filename * "_" * string(mpi.rank) * ".h5"
        else
            filename * ".h5"
        end
        rm(filename; force = true)
        HDF5.h5open(filename, "w")
    end

    sim.h5file = fid

    # attrs(fid)["modelhash"] = hash(sim.model)
    # @info "wrote hash" attrs(fid)["modelhash"] hash(sim.model)
    attrs(fid)["fileformat"] = 2
    attrs(fid)["mpisize"] = mpi.size
    attrs(fid)["mpirank"] = mpi.rank
    attrs(fid)["HDF5parallel"] = parallel_write() 
    attrs(fid)["initialized"] = sim.initialized
    attrs(fid)["last_snapshot"] = -1

    # we use this to attach metadata to a simulation
    mid = create_group(fid, "_meta")
    attrs(mid)["simulation_name"] = sim.name
    attrs(mid)["model_name"] = sim.model.name
    attrs(mid)["date"] = Dates.format(Dates.now(), "yyyy-mm-dd HH:MM:SS")
    
    pid = create_group(fid, "params")
    create_group(pid, "_meta")
    # See comment in write_globals.
    eid = create_group(pid, "_empty_array")
    
    if sim.params !== nothing
        for k in fieldnames(typeof(sim.params))
            field = getfield(sim.params, k)
            if typeof(field) <: Array
                if length(field) > 0
                    pid[string(k)] = field
                    attrs(pid[string(k)])["array"] = true
                else
                    attrs(eid)[string(k)] = true
                end
            else
                pid[string(k)] = [ field ]
                attrs(pid[string(k)])["array"] = false
            end
        end
    end
    
    gid = create_group(fid, "globals")
    create_group(gid, "_meta")
    # # we create a group for each global field so that it's possible
    # #
    # if sim.globals !== nothing
    #     for k in fieldnames(typeof(sim.globals))
    #         create_group(gid, k)
    #     end
    # end

    rid = create_group(fid, "rasters")
    _log_time(sim, "write rasters") do
        for raster in keys(sim.rasters)
            rid[string(raster)] = sim.rasters[raster]
        end
    end       

    aid = create_group(fid, "agents")
    # also here we have the problem that the hdf5 library doesn't like
    # vectors of size 0 in parallel mode and we must track those.
    # The group have a subgroup for each transition, and this subgroup
    # the agent types as attributes with a boolean value.
    create_group(aid, "_empty_array")
    foreach(sim.typeinfos.nodes_types) do T
        tid = create_group(aid, string(T))
        attrs(tid)[":Immortal"] = has_hint(sim, T, :Immortal, :Agent)
        create_group(tid, "_meta")
    end    

    eid = create_group(fid, "edges")
    # see comment for agent
    create_group(eid, "_empty_array")
    foreach(sim.typeinfos.edges_types) do T
        tid = create_group(eid, string(T))
        attrs(tid)[":Stateless"] = has_hint(sim, T, :Stateless)
        attrs(tid)[":IgnoreFrom"] = has_hint(sim, T, :IgnoreFrom)
        attrs(tid)[":SingleEdge"] = has_hint(sim, T, :SingleEdge)
        attrs(tid)[":SingleType"] =
            has_hint(sim, T, :SingleType)
        attrs(tid)[":IgnoreSourceState"] =
            has_hint(sim, T, :IgnoreSourceState)
        create_group(tid, "_meta")
    end

    create_group(fid, "snapshots")

    if mpi.rank == 0
        MPI.Bcast!(Ref(length(_preinit_meta)), MPI.COMM_WORLD)
    else
        len = Ref{Int}()
        MPI.Bcast!(len, MPI.COMM_WORLD)
        if mpi.rank == 1
            @assert length(_preinit_meta) == len[] """
            You must call write_metadata always on all ranks!
            """
        end
    end

    for (type, field, key, value) in _preinit_meta
        write_metadata(sim, type, field, key, value)
    end
    empty!(_preinit_meta)

    if mpi.rank == 0
        MPI.Bcast!(Ref(length(_preinit_meta_sim)), MPI.COMM_WORLD)
    else
        len = Ref{Int}()
        MPI.Bcast!(len, MPI.COMM_WORLD)
        if mpi.rank == 1 
            @assert length(_preinit_meta_sim) == len[] """
            You must call write_sim_metadata always on all ranks!
            """
        end
    end

    for (key, value) in _preinit_meta_sim
        write_sim_metadata(sim, key, value)
    end
    empty!(_preinit_meta_sim)

    fid
end

"""
    close_h5file!(sim::Simulation)

Closes the HDF5 file attached to the simulation `sim`.

Be aware that a following call to one of the `write_` functions like
[`write_snapshot`](@ref) will automatically create a new file and,
depending on the `overwrite_file` argument of
[`create_simulation`](@ref) also overwrites to closed file.
"""
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

"""
    write_globals(sim::Simulation, [fields])

Writes the current global values to the attached HDF5 file. If only a
subset of the fields is to be written, this subset can be specified
via the optional `fields` argument.
"""
function write_globals(sim::Simulation,
                fields = sim.globals === nothing ?
                    nothing : fieldnames(typeof(sim.globals)))
    if sim.h5file === nothing 
        create_h5file!(sim)
    end

    if fields === nothing
        return
    end

    lcstr = last_change_string([sim.h5file])
    gid = sim.h5file["globals"]
    if haskey(HDF5.attributes(gid), lcstr) &&
        sim.globals_last_change == attrs(gid)[lcstr]
        return
    end
    attrs(gid)[lcstr] = sim.globals_last_change
    
    _log_time(sim, "write globals") do
        t = transition_str(sim)
        tid = create_group(gid, t)
        # for whatever reason parallel mpi does not like to write (and read)
        # nothing and throws a "unable to modify constant message"
        # in this case. Even worse, we can not access attributes of the emtpy
        # dataset. So we need an additional group to store this information.
        # Don't know what to say about this.
        eid = create_group(tid, "_empty_array")

        for k in fieldnames(typeof(sim.globals))
            if k in fields
                field = getfield(sim.globals, k)
                if typeof(field) <: Array
                    if length(field) > 0
                        tid[string(k)] = field
                        attrs(tid[string(k)])["array"] = true
                    else
                        attrs(eid)[string(k)] = true
                    end
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

function new_dset(gid, name, T, sum_size, data, create_datatype = true)
    dt = if create_datatype
        HDF5.Datatype(HDF5.hdf5_type_id(T))
    else
        T
    end

    # We have seperate files, so we don't need the size of all agents/edges
    # but only the size of the array we want to write into this file
    if ! parallel_write()
        sum_size = length(data)
    end
    
    ds = if create_datatype
        dataspace((sum_size,))
    else
        sum_size
    end

    if config.compression_level == 0 ||
        (parallel_write() && config.no_parallel_compression)
        create_dataset(gid, name, dt, ds; mpio_mode()...)
    else
        chunk_size = if length(data) == 0
            1
        else
            HDF5.heuristic_chunk(data)[1]
            # On a parallel system I did run into problems with the
            # heuristic chunk but for whatever reason it worked with
            # half the size. As I had additional esoteric problems
            # like this I disabled the compression by default,
            # see also VahanaConfig.no_parallel_compression.
            # Int(ceil(HDF5.heuristic_chunk(data)[1]/2))
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

"""
    write_agents(sim::Simulation, [types])

Writes the current agent state to the attached HDF5 file. If only the
agents of a subset of agent types are to be written, this subset can
be specified via the optional `types` argument.
"""
function write_agents(sim::Simulation,
               types::Vector{DataType} = sim.typeinfos.nodes_types)
    if sim.h5file === nothing 
        create_h5file!(sim)
    end

    lcstr = last_change_string([sim.h5file])
    
    _log_time(sim, "write agents") do
        t = transition_str(sim)
        eid = create_group(sim.h5file["agents"]["_empty_array"], t)

        for T in types
            gid = sim.h5file["agents"][string(T)]
            
            field = getproperty(sim, Symbol(T))
            if haskey(attrs(gid), lcstr) &&
                field.last_change == attrs(gid)[lcstr]
                continue
            end
            attrs(gid)[lcstr] = field.last_change

            num_agents = Int64(field.nextid - 1)
            (vec_num_agents, vec_displace) = calc_displace(num_agents)
            sum_num_agents = sum(vec_num_agents)

            tid = create_group(gid, t)
            attrs(tid)[lcstr] = field.last_change
            attrs(tid)["size_per_rank"] = vec_num_agents
            attrs(tid)["displace"] = vec_displace

            attrs(eid)[string(T)] = sum_num_agents == 0

            if sum_num_agents > 0
                if parallel_write()
                    start = vec_displace[mpi.rank + 1] + 1
                    last = start + vec_num_agents[mpi.rank + 1] - 1
                else
                    start = 1
                    last = num_agents
                end
                if ! has_hint(sim, T, :Immortal, :Agent)
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

# We need to convert the Edges depending of the edge hints to
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

_neighbors_only(sim, T) = (has_hint(sim, T, :Stateless) ||
    fieldcount(T) == 0) && has_hint(sim, T, :IgnoreFrom)


"""
    write_edges(sim::Simulation, [types])

Writes the current edge states to the attached HDF5 file. If only the
edges of a subset of edge types are to be written, this subset can
be specified via the optional `types` argument.
"""
function write_edges(sim::Simulation,
              types::Vector{DataType} = sim.typeinfos.edges_types)

    if sim.h5file === nothing
        create_h5file!(sim)
    end

    lcstr = last_change_string([sim.h5file])

    _log_time(sim, "write edges") do
        t = transition_str(sim)
        eid = create_group(sim.h5file["edges"]["_empty_array"], t)

        for T in types
            gid = sim.h5file["edges"][string(T)]

            field = getproperty(sim, Symbol(T))
            if haskey(attrs(gid), lcstr) &&
                field.last_change == attrs(gid)[lcstr]
                continue
            end
            attrs(gid)[lcstr] = field.last_change

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
                if has_hint(sim, T, :SingleType)
                    (edges, eltype(edges), false)
                else
                    ([EdgeCount(to, count) for (to, count) in edges], EdgeCount, true)
                end
            elseif fieldcount(T) == 0
                if has_hint(sim, T, :Stateless)
                    (map(e -> StatelessEdge(e[1], e[2]), edges),
                     StatelessEdge, true)
                else
                    (map(e -> StatelessEdge(e[1], e[2].from), edges),
                     StatelessEdge, true)
                end
            elseif has_hint(sim, T, :IgnoreFrom)
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
            
            attrs(tid)[lcstr] = field.last_change
            attrs(tid)["size_per_rank"] = vec_num_edges
            attrs(tid)["displace"] = vec_displace

            if parallel_write()
                start = vec_displace[mpi.rank + 1] + 1
                last = start + vec_num_edges[mpi.rank + 1] - 1
            else
                start = 1
                last = num_edges
            end

            attrs(eid)[string(T)] = sum_num_edges == 0
            
            if sum_num_edges > 0
                if length(converted) > 0
                    tid[start:last] = converted
                else
                    tid[start:last] = Vector{DT}()
                end
            end

            finish_read!(sim, T)
        end
    end

    flush(sim.h5file)

    nothing
end

"""
    write_snapshot(sim::Simulation, [comment::String = "", ignore = []])

Writes the current state of the simulation `sim` to the attached HDF5
file. `comment` can be used to identify the snapshot via
[`list_snapshots`](@ref), and to read this snapshot via the
[`read_snapshot!`](@ref) function by utilizing the `comment` keyword of
this function.

`ignore` is a list of agent and/or edge types, that should not be written.

See also [`create_h5file!`](@ref), [`read_snapshot!`](@ref)
"""
function write_snapshot(sim::Simulation, comment::String = ""; ignore = [], step = -1)
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
#
# IMPORTANT: the files must be closed by the caller of open_h5file
function open_h5file(sim::Union{Simulation, Nothing}, filename)
    # first we sanitize the filename
    if endswith(filename, ".h5")
        filename = filename[1:end-3]
    end
    if endswith(filename, "_0")
        filename = filename[1:end-2]
    end
    
    if config.hdf5_path !== nothing
        filename = mkpath(config.hdf5_path)  * "/" * filename
    end

    if ! (isfile(filename * ".h5") || isfile(filename * "_0.h5"))
        filenameh5 = mkpath("h5") * "/" * filename
        if ! (isfile(filenameh5 * ".h5") || isfile(filenameh5 * "_0.h5"))
            println("""
                No hdf5 file(s) for $(filename) or $(filenameh5) found in $(pwd()) or $(pwd())/h5
            """)
            return []
        else
            filename = filenameh5
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
    only_numbers = filter(s -> s[1:2] == "t_", keys(ids))
    ts = filter(<=(transition), map(s -> parse(Int64, s[3:end]), only_numbers))
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

    if ! has_hint(sim, T, :Immortal, :Agent)
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
    if ! has_hint(sim, T, :Immortal, :Agent)
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

    died = if ! has_hint(sim, T, :Immortal, :Agent) 
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

function _read_globals_or_params(hid, empty_array_str)
    eid = hid[empty_array_str]
    all_keys = map(string,
                   filter(k -> k[1] != '_',
                          [keys(hid); keys(attrs(eid))]))

    Dict(map(all_keys) do k
             k => if attrs(hid[k])["array"] 
                 hid[k][]
             else
                 hid[k][1]
             end
         end)
end

function _read_globals_or_params(hid, T, empty_array_str)
    eid = hid[empty_array_str]
    all_keys = map(string,
                   filter(k -> k[1] != '_',
                          [keys(hid); keys(attrs(eid))]))

    unsorted = Dict(map(all_keys) do k
                        k => if haskey(HDF5.attributes(eid), k)
                            Vector{fieldtype(T, Symbol(k))}()
                        elseif attrs(hid[k])["array"] 
                            hid[k][]
                        else
                            hid[k][1]
                        end
                    end)
    
    map(n -> unsorted[string(n)], fieldnames(T))
end


"""
    read_params(filename::String, T::DataType)
    read_params(sim::Simulation, T::DataType)
    read_params(sim::Simulation, nr::Int64, T::DataType)
    read_params(filename::String)

Read the parameters from an HDF5 file. If `filename` is given, the
parameters are read from the file with this filename from the `h5`
subfolder of the current working directory.

If a simulation `sim` is given instead, the filename from the
[`create_simulation`](@ref) call is used.

If the `overwrite_file` argument of [`create_simulation`](@ref) is set
to true, and the file names are supplemented with a number, the number of
the meant file can be specified via the `nr` argument.

In the case that the DataType `T` of the argument `globals` of
[`create_simulation`](@ref) used for the simulation is specified, the
result will be an instance of T, elsewhere it will be a Dict with
the fields of the written Globals type as keys.
"""
function read_params(filename::String, T::DataType)
    fids = open_h5file(nothing, filename)
    if length(fids) == 0
        return
    end

    values = _read_globals_or_params(fids[1]["params"],
                                     T,
                                     empty_array_string(fids))
    
    foreach(close, fids)

    T(values...)
end

read_params(sim::Simulation, T::DataType) =
    read_params(sim.filename, T)

read_params(sim::Simulation, nr::Int64, T::DataType) =
    read_params(add_number_to_file(sim.filename, nr), T)

function read_params(filename)
    fids = open_h5file(nothing, filename)
    if length(fids) == 0
        return
    end

    values = _read_globals_or_params(fids[1]["params"],
                                     empty_array_string(fids))

    foreach(close, fids)

    values
end

function _transition_from_comment(sim_or_filename, comment)
    if comment == ""
        return nothing
    end
    
    snaps = list_snapshots(sim_or_filename)
    tnr = filter(r -> r[2] == "$(comment)", snaps)
    if length(tnr) == 0
        println("Found no snapshot with comment $(comment)")
        nothing
    else
        tnr[1][1]
    end
end

"""
    read_globals(filename::String, T::DataType; [ transition = typemax(Int64), comment = "" ])
    read_globals(sim::Simulation, T::DataType; [ transition = typemax(Int64), comment = "" ])
    read_globals(sim::Simulation, nr::Int64, T::DataType; [ transition = typemax(Int64), comment = "" ])
    read_globals(filename::String; [ transition = typemax(Int64), comment = "" ])

Read the global values from an HDF5 file. If `filename` is given, the
parameters are read from the file with this filename from the `h5`
subfolder of the current working directory.

If a simulation `sim` is given instead, the filename from the
[`create_simulation`](@ref) call is used.

If the `overwrite_file` argument of [`create_simulation`](@ref) is set
to true, and the file names are supplemented with a number, the number of
the meant file can be specified via the `nr` argument.

In the case that the DataType `T` of the argument `globals` of
[`create_simulation`](@ref) used for the simulation is specified, the
result will be an instance of T, elsewhere it will be a Dict with
the fields of the written Globals type as keys.

Per default, the last written globals are read. The `transition`
keyword allows to read also earlier versions. Alternatively, the
comment that was specified when write_snapshot was called can be
specified with the `comment` keyword to read the corresponding
snapshot.

See also [File storage](./hdf5.md) for details.
"""
function read_globals(name::String, T::DataType;
               transition = typemax(Int64),
               comment = "")
    fids = open_h5file(nothing, name)
    if length(fids) == 0
        return
    end

    if _transition_from_comment(name, comment) !== nothing
        transition = _transition_from_comment(name, comment)
    end

    values = _read_globals_or_params(find_transition_group(fids[1]["globals"],
                                                           transition),
                                     T,
                                     empty_array_string(fids))

    foreach(close, fids)

    T(values...)
end

read_globals(sim, T::DataType; transition = typemax(Int64)) =
    read_globals(sim.filename, T; transition)

read_globals(sim, nr::Int64, T::DataType; transition = typemax(Int64)) =
    read_globals(add_number_to_file(sim.filename, nr), T; transition)

function read_globals(filename; transition = typemax(Int64), comment = "")
    fids = open_h5file(nothing, filename)
    if length(fids) == 0
        return
    end

    if _transition_from_comment(filename, comment) !== nothing
        transition = _transition_from_comment(filename, comment)
    end
    
    values = _read_globals_or_params(find_transition_group(fids[1]["globals"],
                                                           transition),
                                      empty_array_string(fids))

    foreach(close, fids)

    values
end

"""
    read_agents(filename::String, type; transition = typemax(Int64))

Read the agentstates of `type` (which can be a DataType, String or
Symbol) from an HDF5 file with the name `filename`. The agent are read
from the `h5` subfolder of the current working directory, or from the
subfolder set with [`set_hdf5_path`](@ref).

Per default, the last written agents are read. The `transition`
keyword allows to read also earlier versions. Alternatively, the
comment that was specified when write_snapshot was called can be
specified with the `comment` keyword to read the corresponding
snapshot.

Returns a vector of agentstates.
"""
function read_agents(filename::String, type;
              transition = typemax(Int64),
              comment = "")
    type = string(type)
    fids = open_h5file(nothing, filename)
    if length(fids) == 0
        return
    end

    if _transition_from_comment(filename, comment) !== nothing
        transition = _transition_from_comment(filename, comment)
    end
    
    aid = fids[1]["agents"][type]
    t = find_transition_nr(aid, transition)
    immortal = attrs(aid)[":Immortal"]

    state = map(fids) do fid
        fid["agents/$(type)/t_$t/state"][]
    end |> Iterators.flatten 

    if ! immortal
        died = map(fids) do fid
            fid["agents/$(type)/t_$t/died"][]
        end |> Iterators.flatten
    end
    
    foreach(close, fids)

    if immortal
        state |> collect
    else
        map(z -> z[1], filter(z -> !z[2], zip(state, died) |> collect))
    end
end


"""
    read_agents!(sim::Simulation, [name::String = sim.filename; transition = typemax(Int64), types::Vector{DataType}, comment = "" ])
    read_agents!(sim::Simulation, nr::Int64; [transition = typemax(Int64), types::Vector{DataType}, comment = "" ])

Read the agents from an HDF5 file into the simulation `sim`. If `name`
is given, the agent are read from the file with this filename from the
`h5` subfolder of the current working directory, or from the subfolder
set with [`set_hdf5_path`](@ref). In the other case the filename from
the [`create_simulation`](@ref) call is used.

If the `overwrite_file` argument of [`create_simulation`](@ref) is set
to true, and the file names are supplemented with a number, the number of
the meant file can be specified via the `nr` argument.

Per default, the last written agents are read. The `transition`
keyword allows to read also earlier versions. Alternatively, the
comment that was specified when write_snapshot was called can be
specified with the `comment` keyword to read the corresponding
snapshot.

If only the agents of a subset of agent types are to be read, this
subset can be specified via the optional `types` argument.

When the agents from a distributed simulation is read into a single
threaded simulation, the IDs of the agents are modified. 
`read_agents!` returns a dictory that contains the ID mapping.
"""
function read_agents!(sim::Simulation,
               name::String = sim.filename;
               transition = typemax(Int64),
               types::Vector{DataType} = sim.typeinfos.nodes_types,
               comment = "")
    fids = open_h5file(sim, name)
    if length(fids) == 0
        return
    end

    if _transition_from_comment(name, comment) !== nothing
        transition = _transition_from_comment(name, comment)
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

            if ! haskey(fids[1]["agents"], string(T))
                continue
            end

            t = find_transition_nr(fids[1]["agents"][string(T)], transition)
            if attrs(fids[1]["agents"][empty_array_string(fids)]["t_$t"])[string(T)]
                if ! has_hint(sim, T, :Immortal, :Agent)
                    field.write.died = Vector{Bool}()
                end
                if fieldcount(T) > 0
                    field.write.state = Vector{T}()
                end
                continue
            end

            if merge
                # in prepare_write we have an immutable check that we
                # must disable by setting sim.initialized temporary to false
                was_initialized = sim.initialized
                sim.initialized = false
                prepare_write!(sim, [], false, T) 
                sim.initialized = was_initialized
            end

            for fidx in fidxs
                gid = fids[fidx]["agents"]
                
                tid = find_transition_group(gid[string(T)], transition)

                if tid === nothing && fidx == 1
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


"""
    read_edges(filename::String, type; transition = typemax(Int64), comment = "")

Read the edgestates of `type` (which can be a DataType, String or
Symbol) from an HDF5 file with the name `filename`. 
The edgestates are read from the `h5` subfolder of the
current working directory, or from the subfolder set with
[`set_hdf5_path`](@ref).

Per default, the last written edgestates are read. The `transition`
keyword allows to read also earlier versions. Alternatively, the
comment that was specified when write_snapshot was called can be
specified with the `comment` keyword to read the corresponding
snapshot.

Returns a vector of edgestates.
"""
function read_edges(filename::String, type;
             transition = typemax(Int64),
             comment = "")
    type = string(type)
    fids = open_h5file(nothing, filename)
    if length(fids) == 0
        return
    end

    if _transition_from_comment(filename, comment) !== nothing
        transition = _transition_from_comment(filename, comment)
    end
    
    eid = fids[1]["edges"][type]
    t = find_transition_nr(eid, transition)
    stateless = attrs(eid)[":Stateless"]

    if stateless
        error("Can not read the state of stateless edges")
    end

    state = map(fids) do fid
        fid["edges/$(type)/t_$t"][]
    end |> Iterators.flatten 

    
    foreach(close, fids)

    state |> collect
end

"""
    read_edges!(sim::Simulation, [name::String = sim.filename; idmapfunc = identity, transition = typemax(Int64), types::Vector{DataType}, comment = "" ])
    read_edges!(sim::Simulation, nr::Int64; [ idmapfunc = identity, transition = typemax(Int64), types::Vector{DataType}, comment = "" ])

Read the edges from an HDF5 file into the simulation `sim`. If
`name` is given, the edges are read from the file with this filename
from the `h5` subfolder of the current working directory.  In the
other case the filename from the [`create_simulation`](@ref) call is
used.

If the `overwrite_file` argument of [`create_simulation`](@ref) is set
to true, and the file names are supplemented with a number, the number of
the meant file can be specified via the `nr` argument.

Per default, the last written edges are read. The `transition`
keyword allows to read also earlier versions. Alternatively, the
comment that was specified when write_snapshot was called can be
specified with the `comment` keyword to read the corresponding
snapshot.

If only the edges of a subset of edge types are to be read, this
subset can be specified via the optional `types` argument.

When the agents from a distributed simulation is read into a single
threaded simulation, the IDs of the agents are modified.  The
`idmapfunc` must be a function that must return the new agent id for a
given old agent id. [`read_agents!`](@ref) returns a `Dict{AgentID,
AgentID}` that can be used for this via: `idmapfunc = (key) ->
idmapping[key]`, where `idmapping` is such a `Dict`.
"""
function read_edges!(sim::Simulation,
              name::String = sim.filename;
              idmapfunc = identity,
              transition = typemax(Int64),
              types::Vector{DataType} = sim.typeinfos.edges_types,
              comment = "")
    fids = open_h5file(sim, name)
    if length(fids) == 0
        return
    end
    @assert length(fids) == mpi.size || idmapfunc !== identity """
      read_edges! can be only merging a distributed graph when an idmap is given
    """

    if _transition_from_comment(name, comment) !== nothing
        transition = _transition_from_comment(name, comment)
    end

    original_size = attrs(fids[1])["mpisize"]
    merge = original_size > mpi.size
    @assert !merge || mpi.size == 1
    
    fidxs = merge ? (1:original_size) : (mpi.rank+1:mpi.rank+1)
    
    # we call add_edge! but there is a check that this is only done
    # at initialization time or in a transition function
    sim.intransition = true
    
    for T in types
        if ! haskey(fids[1]["edges"], string(T))
            continue
        end
        
        with_logger(sim) do
            @info("<Begin> read edges", edgetype = T, transition = transition)
        end
        prepare_write!(sim, [], false, T)

        trnr = find_transition_nr(fids[1]["edges"][string(T)], transition)
        if trnr === nothing
            continue
        end
        if attrs(fids[1]["edges"][empty_array_string(fids)]["t_$(trnr)"])[string(T)]
            _log_info(sim, "<End> read edges")
            finish_write!(sim, T)
            continue
        end
        
        for fidx in fidxs
            tid = find_transition_group(fids[fidx]["edges"][string(T)],
                                        transition)

            _read_edges!(sim, tid, idmapfunc, T, fidx)
        end
        finish_write!(sim, T)

        if trnr === nothing
            getproperty(sim, Symbol(T)).last_change = 0
        else
            getproperty(sim, Symbol(T)).last_change = trnr
        end
        _log_info(sim, "<End> read edges")
    end
    

    sim.intransition = false
    
    foreach(close, fids)
end

read_edges!(sim::Simulation,
            nr::Int64;
            idmapfunc = identity,
            transition = typemax(Int64),
            types::Vector{DataType} = sim.typeinfos.nodes_types,
            comment = "") = 
                read_edges!(sim, add_number_to_file(sim.filename, nr);
                            idmapfunc, transition, types, comment)


function _read_edges!(sim::Simulation, tid, idmapfunc, T, fidx)
    field = getproperty(sim, Symbol(T))
    vec_num_edges = attrs(tid)["size_per_rank"]
    vec_displace = attrs(tid)["displace"]
    start = vec_displace[fidx] + 1
    last = start + vec_num_edges[fidx] - 1

    if _neighbors_only(sim, T)
        if has_hint(sim, T, :SingleType)
            AT = sim.typeinfos.edges_attr[T][:target]
            typeid = sim.typeinfos.nodes_type2id[AT]
            data = if has_hint(sim, T, :SingleEdge)
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
    elseif fieldcount(T) == 0 # StatelessEdge 
        for se in HDF5.read(tid, StatelessEdge, start:last)
            add_edge!(sim, idmapfunc(se.from), idmapfunc(se.to), T())
        end
    elseif has_hint(sim, T, :IgnoreFrom)
        # for add_edge! we need always a from id, even when it's ignored
        dummy = AgentID(0)
        for edge in HDF5.read(tid, IgnoreFromEdge{T}, start:last)
            add_edge!(sim, dummy, idmapfunc(edge.to), edge.state)
        end
    else
        if has_hint(sim, T, :SingleType)
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


"""
    read_snapshot!(sim::Simulation, [name::String; transition = typemax(Int64), comment = "", writeable = false, ignore_params = false])
    read_snapshot!(sim::Simulation, nr::Int64; [transition = typemax(Int64), comment = "", writeable = false, ignore_params = false])

Read a complete snapshot from a file into the simulation `sim`. If
`name` is given, the snapshot is read from the file with this filename
from the `h5` subfolder of the current working directory.  In the
other case the filename from the [`create_simulation`](@ref) call is
used.

If the `overwrite_file` argument of [`create_simulation`](@ref) is set
to true, and the file names are supplemented with a number, the number of
the meant file can be specified via the `nr` argument.

Per default, the last written snapshot is read. The `transition`
keyword allows to read also earlier versions. Alternatively, the
comment that was specified when write_snapshot was called can be
specified with the `comment` keyword to read the corresponding
snapshot.

If `writeable` is set to true, the file is also attached to the simulation
and following `write_` functions like [`write_snapshot`](@ref) will be
append to the file.

If `ignore_params` is set to true, the parameters of `sim` will not be
changed.

Returns false when no snapshot was found 
"""
function read_snapshot!(sim::Simulation,
                 name::String = sim.filename;
                 transition = typemax(Int64),
                 comment = "",
                 writeable = false,
                 ignore_params = false)
    # First we free the memory allocated by the current state of sim
    _free_memory!(sim)

    fids = open_h5file(sim, name)
    if length(fids) == 0
        return false
    end

    if _transition_from_comment(name, comment) !== nothing
        transition = _transition_from_comment(name, comment)
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

    if sim.globals !== nothing && globals_last_change !== nothing
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
               comment = "",
               writeable = false,
               ignore_params = false) =
                   read_snapshot!(sim, add_number_to_file(sim.filename, nr);
                                  transition, comment,
                                  writeable, ignore_params)


"""
    list_snapshots(name::String)
    list_snapshots(sim::Simulation)
    list_snapshots(sim::Simulation, nr::Int64)

List all snapshots of a HDF5 file. If `name` is given, the snapshots
from the file with this filename is returned.  In the other case the
filename from the [`create_simulation`](@ref) call is used.

If the `overwrite_file` argument of [`create_simulation`](@ref) is set
to true, and the file names are supplemented with a number, the number of
the meant file can be specified via the `nr` argument.

Returns a vector of tuples, where the first element is the transition
number for which a snapshot was saved, and the second element is the
comment given in the [`write_snapshot`](@ref) call.
"""
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
"""
    create_namedtuple_converter(T::DataType)

The HDF5.jl library does not support the storage of nested structs,
but structs can have `NamedTuples` as fields. This function creates a
convert function from a struct to a corresponding `NamedTuple` (and
also the other way around), so after calling this for a type `T`, `T`
can be the type of an agent/edge/param/global field.
"""
function create_namedtuple_converter(T::DataType)
    NT = NamedTuple{fieldnames(T), Tuple{fieldtypes(T)...}}
    @eval convert(::Type{$NT}, st::$T) = ntfromstruct(st)
    @eval convert(::Type{$T}, nt::$NT) = structfromnt($T, nt)
end

"""
    write_metadata(sim::Simulation, type::Union{Symbol, DataType}, field::Symbol, key::Symbol, value)

Attach metadata to a `field` of an agent- or edgetype or the
`globals` or `params` struct (see [`create_simulation`](@ref)) or to a
raster (in that case `field` must be the name of the raster). `type`
must be an agent- or edgetype, :Global, :Param or :Raster. Metadata is
stored via `key`, `value` pairs, so that multiple data of different
types can be attached to a single field.

See also: [`read_metadata`](@ref)
"""
function write_metadata(sim::Simulation,
                 type::Union{Symbol, DataType},
                 field::Symbol,
                 key::Symbol,
                 value)
    if sim.h5file === nothing
        if ! ((type, field, key, value) in _preinit_meta)
            push!(_preinit_meta, (type, field, key, value))
        end
        return
    end

    if value === nothing
        value = "nothing"
    end
    
    field_str = String(field)
    
    mid = if type == :Param
        sim.h5file["params"]["_meta"]
    elseif type == :Global
        sim.h5file["globals"]["_meta"]
    elseif type == :Raster
        sim.h5file["rasters"]
    elseif type in sim.typeinfos.nodes_types ||
        type in map(Symbol, sim.typeinfos.nodes_types)
        sim.h5file["agents"][String(type)]["_meta"]
    elseif type in sim.typeinfos.edges_types ||
        type in map(Symbol, sim.typeinfos.edges_types)
        sim.h5file["edges"][String(type)]["_meta"]
    else
        @error """
        Can not write metadata, `type` must be an agenttype or edgetype 
        or one of the following symbols: :Param, :Global, :Raster.
        """
    end

    if ! (field_str in keys(mid))
        create_group(mid, field_str)
    end
    write_attribute(mid[field_str], String(key), value)
    flush(sim.h5file)
end

"""
    read_metadata(sim::Simulation, type::Union{Symbol, DataType}[, field::Symbol, key::Symbol ])
    read_metadata(filename::String, type::Union{Symbol, DataType}[, field::Symbol, key::Symbol ])

Read metadata for a `field` of an agent- or edgetype or the `globals` or
`params` struct (see [`create_simulation`](@ref)) or to a raster (in that
case `field` must be the name of the raster). `type` must be an agent-
or edgetype :Global, :Param or :Raster. Metadata is stored via `key`,
value pairs. Multiple data of different types can be attached to a
single field, a single piece of the metadata can be retrived via the
`key` parameter. If this is not set (or set to Symbol()), a
Dict{Symbol, Any} with the complete metadata of this field is
returned.

See also: [`write_metadata`](@ref)
"""
function read_metadata(sim::Simulation,
                type::Union{Symbol, DataType},
                field::Symbol = Symbol(),
                key::Symbol = Symbol())
    if field == Symbol()
        _read_metadata_all_fields(open_h5file(sim, sim.filename), type)
    else
        _read_metadata(open_h5file(sim, sim.filename), type, field, key, true)
    end
end

function read_metadata(filename::String,
                type::Union{Symbol, DataType},
                field::Symbol = Symbol(),
                key::Symbol = Symbol())
    if field == Symbol()
        _read_metadata_all_fields(open_h5file(nothing, filename), type)
    else
        _read_metadata(open_h5file(nothing, filename), type, field, key, true)
    end
end

function _read_metadata_all_fields(fids, type::Symbol)
    all = Dict()

    for f in _read_metadata(fids, type, Symbol(), Symbol(), false)
        all[Symbol(f)] = _read_metadata(fids, type, f, Symbol(), false)
    end

    foreach(close, fids)

    all
end

function _read_metadata(fids, type, field, key, close_file)
    if length(fids) == 0
        return
    end

    mid = if type == :Param
        fids[1]["params"]["_meta"]
    elseif type == :Global
        fids[1]["globals"]["_meta"]
    elseif type == :Raster
        fids[1]["rasters"]
    elseif type in keys(fids[1]["agents"]) ||
        type in map(Symbol, keys(fids[1]["agents"]))
        fids[1]["agents"][String(type)]["_meta"]
    elseif type in keys(fids[1]["edges"]) ||
        type in map(Symbol, keys(fids[1]["edges"]))
        fids[1]["edges"][String(type)]["_meta"]
    else
        @error """
        Can not read metadata, `type` must be an agenttype or edgetype 
        or one of the following symbols: :Param, :Global, :Raster.
        """
    end

    if field == Symbol()
        r = keys(mid)
        if close_file
            foreach(close, fids)
        end
        return r
    end

    field_str = String(field)
    
    r = if field_str in keys(mid)
        if key == Symbol()
            Dict([ (Symbol(d[1]), d[2]) for d in Dict(attrs(mid[field_str])) ])
        else
            get(attrs(mid[field_str]), String(key), nothing)
        end
    else
        nothing
    end

    if close_file 
        foreach(close, fids)
    end

    r
end

"""
    write_sim_metadata(sim::Simulation, key::Symbol, value)

Attach additional metadata to a simulation. 

See also: [`read_sim_metadata`](@ref)
"""
function write_sim_metadata(sim::Simulation, key::Symbol, value)
    if sim.h5file === nothing
        push!(_preinit_meta_sim, (key, value))
        return
    end

    # to ensure that the user written meta data does not
    # interfere with the metadata from Vahana, we
    # also store this into a seperate group
    write_attribute(sim.h5file["_meta"], String(key), value)

    flush(sim.h5file)
end    

"""
    read_sim_metadata(sim::Simulation, [ key::Symbol = Symbol() ])
    read_sim_metadata(filename::String, [ key::Symbol = Symbol() ])

Read metadata for a simulation or from the file `filename`. Metadata is
stored via `key`, value pairs. If `key` is not set (or set to
Symbol()), a Dict{Symbol, Any} with the complete metadata of the
simulation is returned.

The following metadata is stored automatically:
- simulation_name
- model_name
- date (in the format "yyyy-mm-dd hh:mm:ss")

See also: [`write_metadata`](@ref)
"""
function read_sim_metadata(sim::Simulation,
                    key::Symbol = Symbol())
    read_sim_metadata(open_h5file(sim, sim.filename), key)
end

function read_sim_metadata(filename::String,
                    key::Symbol = Symbol())
    read_sim_metadata(open_h5file(nothing, filename), key)
end

function read_sim_metadata(fids,
                    key::Symbol = Symbol())
    if length(fids) == 0
        return
    end
    
    mid = fids[1]["_meta"]
    
    r = if key == Symbol()
        Dict([ (Symbol(d[1]), d[2]) for d in Dict(attrs(mid)) ])
    else
        get(attrs(mid), String(key), nothing)
    end

    foreach(close, fids)

    r
end

