using Vahana
using Test

struct Person <: Agent
    foo::Int64
end

struct HH <: Agent
    bar::Int64
end

 
struct FooEdgeState <: EdgeState
    foo::Int64
end

struct StatelessEdgeType <: EdgeState end

include("aggregate.jl")
include("globals.jl")
include("initialization.jl")
include("graphs.jl")
include("raster.jl")

@testset "Tutorial1" begin
    include("tutorial1.jl")
    params = Params(numBuyer = 500, numSeller = 2, knownSellers = 2)
    
    sim = run_simulation(5, params)

    @test 0.8 < last(getglobal(sim, :p)) < 1.2
end

