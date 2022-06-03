using Vahana
using Test
using BenchmarkTools

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

struct EdgeD foo::Int64 end # D for default (no property is set)
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
struct EdgeETs foo::Int64 end
struct EdgeTsI foo::Int64 end
struct EdgeETsI foo::Int64 end

struct EdgeSTs end
struct EdgeSETs end
struct EdgeSTsI end
struct EdgeSETsI end

statelessEdgeTypes = [ EdgeS, EdgeSE, EdgeST, EdgeSI, EdgeSET, EdgeSEI, EdgeSTI, EdgeSETI,
                       EdgeSTs, EdgeSETs, EdgeSTsI, EdgeSETsI  ]

statefulEdgeTypes = [ EdgeD, EdgeE, EdgeT, EdgeI, EdgeET, EdgeEI, EdgeTI, EdgeETI,
                      EdgeTs, EdgeETs, EdgeTsI, EdgeETsI ]




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

struct GraphA 
    id::Int64
    sum::Int64
end

struct GraphE end

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
    add_edgetype!(EdgeD) |>
    add_edgetype!(EdgeS, :Stateless) |>
    add_edgetype!(EdgeE, :SingleEdge) |>
    add_edgetype!(EdgeT, :SingleAgentType; to_agenttype = AVec) |>
    add_edgetype!(EdgeI, :IgnoreFrom) |>
    add_edgetype!(EdgeSE, :Stateless, :SingleEdge) |>
    add_edgetype!(EdgeST, :Stateless, :SingleAgentType; to_agenttype = AVec) |>
    add_edgetype!(EdgeSI, :Stateless, :IgnoreFrom) |>
    add_edgetype!(EdgeET, :SingleEdge, :SingleAgentType; to_agenttype = AVec) |>
    add_edgetype!(EdgeEI, :SingleEdge, :IgnoreFrom) |>
    add_edgetype!(EdgeTI, :SingleAgentType, :IgnoreFrom; to_agenttype = AVec) |>
    add_edgetype!(EdgeSET, :Stateless, :SingleEdge, :SingleAgentType; to_agenttype = AVec) |>
    add_edgetype!(EdgeSEI, :Stateless, :SingleEdge, :IgnoreFrom) |>
    add_edgetype!(EdgeSTI, :Stateless, :SingleAgentType, :IgnoreFrom; to_agenttype = AVec) |>
    add_edgetype!(EdgeETI, :SingleEdge, :SingleAgentType, :IgnoreFrom; to_agenttype = AVec) |>
    add_edgetype!(EdgeSETI, :Stateless, :SingleEdge, :SingleAgentType, :IgnoreFrom; to_agenttype = AVec) |>
    add_edgetype!(EdgeTs, :SingleAgentType; to_agenttype = AVec, size = 10) |>
    add_edgetype!(EdgeETs, :SingleEdge, :SingleAgentType; to_agenttype = AVec, size = 10) |>
    add_edgetype!(EdgeTsI, :SingleAgentType, :IgnoreFrom; to_agenttype = AVec, size = 10) |>
    add_edgetype!(EdgeETsI, :SingleEdge, :SingleAgentType, :IgnoreFrom; to_agenttype = AVec, size = 10) |>
    add_edgetype!(EdgeSTs, :Stateless, :SingleAgentType; to_agenttype = AVec, size = 10) |>
    add_edgetype!(EdgeSETs, :Stateless, :SingleEdge, :SingleAgentType; to_agenttype = AVec, size = 10) |>
    add_edgetype!(EdgeSTsI, :Stateless, :SingleAgentType, :IgnoreFrom; to_agenttype = AVec, size = 10) |>
    add_edgetype!(EdgeSETsI, :Stateless, :SingleEdge, :SingleAgentType, :IgnoreFrom; to_agenttype = AVec, size = 10) |>
    add_agenttype!(GraphA) |>
    add_edgetype!(GraphE)

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
construct(model, "Test", nothing, nothing)

#include("edges.jl")

#(a1id, a2id, a3id) = add_agents!(sim, AVec(1), AVec(2), AVec(3))

filtertypes(typeidents::String, typelist::Vector) =
    filter(x -> occursin(typeidents, SubString(String(Symbol(x)),5)), typelist)

hasprop(type, prop::String) = occursin(prop, SubString(String(Symbol(type)), 5))



sim = construct(model, "Test", nothing, nothing)

# (a1id, a2id, a3id) = add_agents!(sim, AVec(1), AVec(2), AVec(3))

# Lets add some edges for each of the different property combinations
# For each combination we will have
# 2 -> 3 (with state 1 for stateful edges)
# 3 -> 1 (with state 3 for stateful edges)
# and for all combination that supports multiple edges we add also
# 1 -> 3 (with state 2 for stateful edges)

# for t in statelessEdgeTypes
#     add_edge!(sim, a2id, a3id, t())
#     # we can not check the "ET" combination, instead a warning
#     # is given when add_edgetype is called
#     if hasprop(t, "E") && !hasprop(t, "T") && !(hasprop(t, "S") && hasprop(t, "I"))
#         # and check in the case that a second edge can not be added to
#         # the same agent (and that this can be checked),
#         # that this throws an assertion
#         @test_throws AssertionError add_edge!(sim, a1id, a3id, t())
#     elseif !hasprop(t, "E")
#         add_edge!(sim, a1id, a3id, t())
#     end
#     edge = Edge(a3id, t())
#     add_edge!(sim, a1id, edge)

#     # println(t)
#     # @eval println($sim.$(Symbol(t,"_write")))
#     # println()
# end

# for t in statefulEdgeTypes
#     add_edge!(sim, a2id, a3id, t(1))
#     # we can not check the "ET" combination, instead a warning
#     # is given when add_edgetype is called
#     if hasprop(t, "E") && !hasprop(t, "T") && !(hasprop(t, "S") && hasprop(t, "I"))
#         @test_throws AssertionError add_edge!(sim, a1id, a3id, t(2))
#     elseif !hasprop(t, "E")
#         add_edge!(sim, a1id, a3id, t(2))
#     end
#     edge = Edge(a3id, t(3))
#     add_edge!(sim, a1id, edge)

#     # println(t)
#     # @eval println($sim.$(Symbol(t,"_write")))
#     # println()
# end

# finish_init!(sim)

# sim

# show_random_agent(sim, Val(AVec))

include("core.jl")

include("edges.jl")

include("aggregate.jl")

include("globals.jl")

include("raster.jl")

include("graphs.jl")

# @testset "Tutorial1" begin
#     include("tutorial1.jl")
#     params = Params(numBuyer = 500, numSeller = 2, knownSellers = 2)

#     sim = run_simulation(5, params)

#     @test 0.8 < last(getglobal(sim, :p)) < 1.2
# end

