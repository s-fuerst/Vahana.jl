using Vahana
using Test

enable_asserts(true)

# A for Agent, one type per difference container type
struct ADict     foo::Int64 end
struct AVec      foo::Int64 end
struct AVecFixed foo::Int64 end
struct ASLDict end

# ES = EdgeState and ESL = EdgeStateless
struct ESDict  foo::Int64 end
struct ESLDict1 end
struct ESLDict2 end

model = ModelTypes() |>
    add_agenttype!(ADict) |>
    add_agenttype!(AVec, Vector) |>
    add_agenttype!(AVecFixed, Vector; size = 10) |>
    add_agenttype!(ASLDict) |>
    add_edgetype!(ESDict) |>
    add_edgetype!(ESLDict1) |> 
    add_edgetype!(ESLDict2)  

function add_example_network!(sim)

    # construct 3 ADict agents, 10 AVec agents and 10 AVecFixed
    a1id = add_agent!(sim, ADict(1))
    a2id, a3id = add_agents!(sim, ADict(2), ADict(3))
    avids = add_agents!(sim, [ AVec(i) for i in 1:10])
    avfids = add_agents!(sim, [ AVecFixed(i) for i in 1:10])
    
    

    # we construct the following network for ESDict:
    # a2 & a3 & avids[1] & avfids[10] -> a1
    add_edge!(sim, a2id, a1id, ESDict(1))
    add_edge!(sim, a3id, a1id, ESDict(2))
    add_edge!(sim, avids[1], a1id, ESDict(3))
    add_edge!(sim, avfids[10], a1id, ESDict(4))
    # we construct the following network for ESLVec:
    # avids -> a1
    add_edges!(sim, a1id, [ Edge(avids[i], ESLDict1()) for i in 1:10 ])
    # we construct the following network for ESLVecFixed:
    # avfids[i] -> avids[i]
    for i in 1:10
        add_edge!(sim, avfids[i], avids[i], ESLDict2())
    end

    (a1id, a2id, a3id, avids, avfids)
end

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

