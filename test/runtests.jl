using Vahana
using Test
using BenchmarkTools

enable_asserts(true)







#include("edges.jl")

#(a1id, a2id, a3id) = add_agents!(sim, AVec(1), AVec(2), AVec(3))

# filtertypes(typeidents::String, typelist::Vector) =
#     filter(x -> occursin(typeidents, SubString(String(Symbol(x)),5)), typelist)


include("core.jl")

include("edges.jl")

include("globals.jl")

include("raster.jl")

include("graphs.jl")

# @testset "Tutorial1" begin
#     include("tutorial1.jl")
#     params = Params(numBuyer = 500, numSeller = 2, knownSellers = 2)

#     sim = run_simulation(5, params)

#     @test 0.8 < last(getglobal(sim, :p)) < 1.2
# end

