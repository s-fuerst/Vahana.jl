using Vahana
using Test

struct AgentFoo
    foo::Int64
end

struct AgentBar 
    bar::Int64
end

struct FooEdgeState 
    foo::Int64
end

struct StatelessEdgeType end

include("initialization.jl")

# include("aggregate.jl")
# include("globals.jl")
# include("graphs.jl")
# include("raster.jl")

# @testset "Tutorial1" begin
#     include("tutorial1.jl")
#     params = Params(numBuyer = 500, numSeller = 2, knownSellers = 2)
    
#     sim = run_simulation(5, params)

#     @test 0.8 < last(getglobal(sim, :p)) < 1.2
# end

