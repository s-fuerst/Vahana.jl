module Vahana

using Requires, MPI, Metis

export enable_asserts, suppress_warnings, detect_stateless_trait

function __init__()
    @require Makie="ee78f7c6-11fb-53f2-987a-cfe4a2b5a57a" begin
        include("optional/MakieSupport.jl")
    end
    @require GraphMakie="1ecd5474-83a3-4783-bb4f-06765db800d2" begin
        include("optional/GraphMakieSupport.jl")
    end

    mpiinit()
end

# This is a dummy struct to define the stubs of functions like add_agent
# with a type for the simulation. The real type of the simulation is
# constructed in construct_model (simulations are an instance of a model).
# Adding the dummy type to the struct has the advantage, that e.g. LSP will
# not omit a warning about a possible false call of the function without catching
# calls to the function with a wrong type for the simulation parameter.

"""
    __MODEL__ is a documentation placeholder for the type of the
simulation that is returned from the [`new_simulation`](@ref)
call. The concrete type depends on the [`ModelTypes`](@ref) of the
[`construct_model`](@ref) call, the concrete type name is the name of
the model.
"""
struct __MODEL__ end

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
    detect_stateless_trait(detect::Bool)

Per default, Vahana expects that the :Stateless trait is set manually.

This design decision was made so as not to confuse users, since then,
for example, the [`edges_to`](@ref) is not available.

This behaviour can be customized by calling `detect_stateless_trait` before
calling [`register_edgetype!`](@ref).
"""
detect_stateless_trait = (detect::Bool) -> config.detect_stateless = detect

######################################## include all other files

# TODO DOC
disable_transition_checks = false

include("Helpers.jl")

# MPIInit must be included before Agent/Edge, and Agent/Edge before MPI
include("MPIinit.jl")

include("Agent.jl")
include("Edge.jl")

include("MPI.jl")

include("ModelTypes.jl")

include("EdgeMethods.jl")
include("AgentMethods.jl")

include("Simulation.jl")
include("Global.jl")

include("EdgeIterator.jl")

include("REPL.jl")

include("Raster.jl")

include("GraphsSupport.jl")
end
