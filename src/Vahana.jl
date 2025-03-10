module Vahana

using MPI, Metis, Requires

export enable_asserts, suppress_warnings, detect_stateless
export set_hdf5_path, set_log_path

function __init__()
    @require Makie="ee78f7c6-11fb-53f2-987a-cfe4a2b5a57a" begin
        include("optional/MakieSupport.jl")
    end
    @require GraphMakie="1ecd5474-83a3-4783-bb4f-06765db800d2" begin
        include("optional/GraphMakieSupport.jl")
    end
    @require DataFrames="a93c6f00-e57d-5684-b7b6-d8193f3e46c0" begin
        include("optional/DataFrames.jl")
    end

    mpiinit()
end

"""
    enable_asserts(enable::Bool)

Vahana comes with some internal consistency checks, at the cost of
run-time performance (e.g., in [`agentstate`](@ref) there are checks
that the specified agenttype matches the agent's ID, which creates of
course some overhead). The assertions that could degrade the run
performance can be disabled by calling `enable_asssertions(false)`.

The recommended approach is therefore to leave the assertions enabled
during the development of the model, but to disable them when the
model goes "into production", e.g. before the start of a parameter
space exploration.

"""
function enable_asserts(enable::Bool)
    config.asserts_enabled = enable
    if enable 
        @eval asserting() = true
    else
        @eval asserting() = false
    end
end

# making this a function results in code being invalidated and recompiled when
# this gets changed
asserting() = true 

macro mayassert(test)
  esc(:(if $(@__MODULE__).asserting()
    @assert($test)
   end))
end

macro mayassert(test, msgs)
  esc(:(if $(@__MODULE__).asserting()
    @assert($test, $msgs)
   end))
end

####################

Base.@kwdef mutable struct VahanaConfig
    quiet = false
    detect_stateless = false
    check_readable = true
    asserts_enabled = true
    compression_level = 3
    # compression is implemented (see HDF5.jl/new_dset), but causes a lot of
    # strange problem in combination with HDF5 Parallel. So this is disabled
    # by default. Activate this only after careful testing (e.g.
    # run the run_mpitests script in Vahana's test folder.
    no_parallel_compression = true
    hdf5_path = nothing
    log_path = nothing
end

const config = VahanaConfig()

"""
    suppress_warnings(suppress::Bool)

In some cases Vahana print some hints or warnings to the stdout. This
can be suppressed by calling the `suppress_warnings(true)` function after
importing Vahana.
"""
suppress_warnings = (suppress::Bool) -> config.quiet = suppress

####################

"""
    detect_stateless(detect::Bool = true)

Per default, Vahana expects that the :Stateless hint is set manually.

This design decision was made so as not to confuse users, since then,
for example, the [`edges`](@ref) is not available.

This behaviour can be customized by calling `detect_stateless` before
calling [`register_edgetype!`](@ref).
"""
detect_stateless = (detect::Bool = true) -> config.detect_stateless = detect

"""
    set_compression(level::Int, parallel_hdf5_compression = false)

Set the compression level of HDF5 files. 

However, by default, HDF5 datasets are not compressed when the used
HDF5 library supports Parallel HDF5 due to encountered issues. While
the necessary code to compress datasets in the Parallel HDF5 case is
implemented, its activation via the parallel_hdf5_compression argument
is considered experimental and should be used with awareness of
potential risks and the specificities of the HDF5 implementation in
use.
"""
function set_compression(level::Int, parallel_hdf5_compression = false)
    config.compression_level = level
    config.no_parallel_compression = ! parallel_hdf5_compression
end

"""
    set_hdf5_path(path::String)

Specify the path that is used to save and read the hdf5 files. If the
directory does not exist, it is created the first time a file is
written or tried to read.

See also [`create_h5file!`](@ref)
"""
set_hdf5_path = (path::String) -> config.hdf5_path = path

"""
    set_log_path(path::String)

Specify the path that is used to save the log files. If the directory
does not exist, it is created the first time a log file is written.

Must be called before [`create_simulation`](@ref). If this is not
done, the log files are written to a subfolder `log` of the current
working directory.
"""
set_log_path = (path::String) -> config.log_path = path


######################################## include all other files

abstract type Simulation end

include("Helpers.jl")

# MPIInit must be included before Agent/Edge, and Agent/Edge before MPI
include("MPIinit.jl")

include("Agent.jl")
include("Edge.jl")

include("MPI.jl")

include("ModelTypes.jl")

include("HDF5.jl")

include("EdgeMethods.jl")
include("AgentMethods.jl")

include("Simulation.jl")
include("Global.jl")

include("EdgeIterator.jl")

include("REPL.jl")

include("Raster.jl")

include("GraphsSupport.jl")

include("Logging.jl")

end

