module Vahana

export enable_asserts

asserting() = true #making this a function results in code being invalidated and recompiled when this gets changed

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
end
