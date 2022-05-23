module Vahana

export enable_asserts

asserting() = false #making this a function results in code being invalidated and recompiled when this gets changed

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

# include("PrettyPrinting.jl")

# include("REPL.jl")

include("GraphsSupport.jl")
include("Raster.jl")
end
