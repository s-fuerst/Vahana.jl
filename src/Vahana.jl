module Vahana

export enable_asserts

# This is a dummy struct to define the stubs of functions like add_agent
# with a type for the simulation. The real type of the simulation is
# constructed in construct_model (simulations are an instance of a model).
# Adding the dummy type to the struct has the advantage, that e.g. LSP will
# not omit a warning about a possible false call of the function without catching
# calls to the function with a wrong type for the simulation parameter.

"""
    __SIMULATION__ is a documentation placeholder for type of the
simulation that is returned from the [`new_simulation`](@ref)
call. The concrete type depends on the [`ModelTypes`](@ref) of the
[`construct_model`](@ref) call, the concrete type name is the name of
the model.
"""
struct __SIMULATION__ end

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

######################################## include all other files

include("Agent.jl")
include("Edge.jl")

include("ModelTypes.jl")

include("FactoryHelpers.jl")
include("EdgeFieldFactory.jl")
include("NodeFieldFactory.jl")

include("Simulation.jl")
include("Global.jl")

include("EdgesIterator.jl")

include("PrettyPrinting.jl")
include("REPL.jl")

include("GraphsSupport.jl")
include("Raster.jl")

include("Helpers.jl")
end
