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

# We need a lot of edgetypes to test all the edge property combinations
# The properties are
# (S) Stateless
# (E) SingleEdge
# (T) SingleAgentType, which can also have a size information (Ts)
# (I) IgnoreFrom
# so EdgeET means an Edge(State) with the SingleEdge and SingleAgentType property

struct DefaultEdge foo::Int64 end
struct EdgeS end
struct EdgeE foo::Int64 end
struct EdgeT foo::Int64 end
struct EdgeI foo::Int64 end
struct EdgeSE end
struct EdgeST end
struct EdgeSI end
struct EdgeET foo::Int64 end
struct EdgeEI foo::Int64 end
struct EdgeTI foo::Int64 end
struct EdgeSET end
struct EdgeSEI end
struct EdgeSTI end
struct EdgeETI foo::Int64 end
struct EdgeSETI end

struct EdgeTs foo::Int64 end
struct EdgeSETsI end

statelessEdgeTypes = [ EdgeS, EdgeSE, EdgeST, EdgeSI, EdgeSET, EdgeSEI, EdgeSTI, EdgeSETI,
                       EdgeSETsI ]

statefulEdgeTypes = [ DefaultEdge, EdgeE, EdgeT, EdgeI, EdgeET, EdgeEI, EdgeTI, EdgeETI,
                      EdgeTs ]


# the following structs are needed for the raster tests
struct GridA 
    pos::Tuple{Int64, Int64}
    active::Bool
end

struct GridE end

struct Position 
    sum::Int64
end

struct MovingAgent 
    value::Int64
end

struct OnPosition  end

model = ModelTypes() |>
    add_agenttype!(ADict) |>
    add_agenttype!(AVec, :Vector) |>
    add_agenttype!(AVecFixed, :Vector; size = 10) |>
    add_agenttype!(ASLDict) |>
    add_edgetype!(ESDict) |>
    add_edgetype!(ESLDict1) |> 
    add_edgetype!(ESLDict2, :SingleAgentType; to_agenttype = AVec) |> # to = AVec 
    add_agenttype!(GridA) |>
    add_edgetype!(GridE) |>
    add_agenttype!(Position) |>
    add_agenttype!(MovingAgent) |>
    add_edgetype!(OnPosition) |>
    add_edgetype!(DefaultEdge) |>
    add_edgetype!(EdgeS) |>
    add_edgetype!(EdgeE, :SingleEdge) |>
    add_edgetype!(EdgeT, :SingleAgentType; to_agenttype = ADict) |>
    add_edgetype!(EdgeI, :IgnoreFrom) |>
    add_edgetype!(EdgeSE, :SingleEdge) |>
    add_edgetype!(EdgeST, :SingleAgentType; to_agenttype = ADict) |>
    add_edgetype!(EdgeSI, :IgnoreFrom) |>
    add_edgetype!(EdgeET, :SingleEdge, :SingleAgentType; to_agenttype = ADict) |>
    add_edgetype!(EdgeEI, :SingleEdge, :IgnoreFrom) |>
    add_edgetype!(EdgeTI, :SingleAgentType, :IgnoreFrom; to_agenttype = ADict) |>
    add_edgetype!(EdgeSET, :SingleEdge, :SingleAgentType; to_agenttype = ADict) |>
    add_edgetype!(EdgeSEI, :SingleEdge, :IgnoreFrom) |>
    add_edgetype!(EdgeSTI, :SingleAgentType, :IgnoreFrom; to_agenttype = ADict) |>
    add_edgetype!(EdgeETI, :SingleEdge, :SingleAgentType, :IgnoreFrom; to_agenttype = ADict) |>
    add_edgetype!(EdgeSETI, :SingleEdge, :SingleAgentType, :IgnoreFrom; to_agenttype = ADict) |>
    add_edgetype!(EdgeTs, :SingleAgentType; to_agenttype = ADict, size = 10) |>
    add_edgetype!(EdgeSETsI, :SingleEdge, :SingleAgentType, :IgnoreFrom;
                  to_agenttype = ADict, size = 10) 

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

function create_sum_state_neighbors(edgetypeval) 
    function sum_state_neighbors(agent, id, sim)
        s = 0
        for n in neighborstates_flexible(sim, id, edgetypeval)
            s = s + n.foo
        end
        typeof(agent)(s)
    end
end

function create_sum_state_neighbors_stateless(edgetypeval) 
    function sum_state_neighbors_stateless(agent, id, sim)
        s = 0
        for n in neighborstates_flexible(sim, id, edgetypeval)
            s = s + n.foo
        end
        typeof(agent)(s)
    end
end


function nothing_transition(agent, id, sim)
    nothing
end

# we must construct it once on the here to avoid the "world" problem
const sim = construct(model, "Test", nothing, nothing)

#include("edges.jl")

(a1id, a2id, a3id) = add_agents!(sim, AVec(1), AVec(2), AVec(3))



for t in statelessEdgeTypes
    println(t)
    add_edge!(sim, a1id, a2id, t())
    add_edge!(sim, a3id, a2id, t())

    @eval println(sim.$(Symbol(t,"_write")))
    println()
end

for t in statefulEdgeTypes
    println(t)
    add_edge!(sim, a1id, a2id, t(1))
    add_edge!(sim, a3id, a2id, t(3))
    @eval println(sim.$(Symbol(t,"_write")))
    println()
end



#include("aggregate.jl")

#include("core.jl")

#include("globals.jl")
# include("graphs.jl")
#include("raster.jl")

# @testset "Tutorial1" begin
#     include("tutorial1.jl")
#     params = Params(numBuyer = 500, numSeller = 2, knownSellers = 2)

#     sim = run_simulation(5, params)

#     @test 0.8 < last(getglobal(sim, :p)) < 1.2
# end

