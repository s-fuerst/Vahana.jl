using Vahana
using BenchmarkTools

enable_asserts(false)

struct AgentState end

struct EdgeState
    v::Float64
end

statelessEdgeTypes = [ "EdgeS", "EdgeSE", "EdgeST", "EdgeSI", "EdgeSET", "EdgeSEI", "EdgeSTI", "EdgeSETI",
                       "EdgeSTF", "EdgeSETF", "EdgeSTFI", "EdgeSETFI" ]

statefulEdgeTypes = [ "edge", "EdgeE", "EdgeT", "EdgeI", "EdgeET", "EdgeEI", "EdgeTI", "EdgeETI",
                      "EdgeTF", "EdgeETF", "EdgeTFI", "EdgeETFI" ]

allEdgeTypes = vcat(statefulEdgeTypes, statelessEdgeTypes)

#allEdgeTypes = [ "EdgeT", "EdgeTF" ]


hasprop(type, prop::String) = occursin(prop, SubString(String(Symbol(type)), 5))

function prepare(name)
    props = []
    
    if hasprop(name, "S")
        props = vcat(props, :Stateless)
    end
    if hasprop(name, "E")
        props = vcat(props, :SingleEdge)
    end
    if hasprop(name, "T")
        props = vcat(props, :SingleAgentType)
    end
    if hasprop(name, "I")
        props = vcat(props, :IgnoreFrom)
    end

    if hasprop(name, "F")
        mt = ModelTypes() |>
            add_agenttype!(AgentState) |>
            add_edgetype!(EdgeState, props...; to_agenttype=AgentState, size=3)
    else
        mt = ModelTypes() |>
            add_agenttype!(AgentState) |>
            add_edgetype!(EdgeState, props...; to_agenttype=AgentState)
    end    
    construct(mt, name, nothing, nothing)

    mt
end

function pt(b)
    if b === nothing
        "-"
    else
        round(mean(b.times), digits=1) |> string
    end
end

function run_benchmark(mt, name)
    sim = construct(mt, name, nothing, nothing)

    a1 = add_agent!(sim, AgentState())
    a2 = add_agent!(sim, AgentState())
    a3 = add_agent!(sim, AgentState())

    stateless = hasprop(name, "S")
    singleedge = hasprop(name, "E")
    singletype = hasprop(name, "T")
    ignorefrom = hasprop(name, "I")
    fixsize = hasprop(name, "F")


    add_edge!(sim, a1, a2, EdgeState(0.1))
    add_edge!(sim, a1, a2, EdgeState(0.2))
    add_edge!(sim, a1, a2, EdgeState(0.3))

    edge = Edge(a2, EdgeState(2.0))

    for i=1:10
        add_edge!(sim, a1, a3, EdgeState(i))
    end

    addedge = @benchmark add_edge!($sim, $a1, $edge)

    finish_init!(sim)

    if !stateless && !ignorefrom 
        edgeto = @benchmark edges_to($sim, $a3, Val(EdgeState))
    else
        edgeto = nothing
    end

    if ignorefrom 
        nids = nothing
    else
        nids = @benchmark neighborids($sim, $a3, Val(EdgeState))
    end

    if stateless 
        estates = nothing
    else
        estates = @benchmark edgestates($sim, $a3, Val(EdgeState))
    end


    if ignorefrom && stateless || !singleedge || !singletype
        hasn = @benchmark has_neighbor($sim, $a3, Val(EdgeState))
    else
        hasn = nothing
    end
    
    if singleedge
        numn = nothing
    else
        numn = @benchmark num_neighbors($sim, $a3, Val(EdgeState))
    end
    println("| $(name) | $(pt(addedge)) | $(pt(edgeto)) |  $(pt(hasn)) | $(pt(numn)) | $(pt(nids)) | $(pt(estates)) |")
end

# aggregate | edgestates | 

println("| EdgeType | add_edge! | edges_to | has_neighbor | num_neighbors | neighborids | edgestates |")
for t in allEdgeTypes
    mt = prepare(t)
    run_benchmark(mt, t)
    GC.gc()
end

mutable struct Foo
    b::Bool
end

const foo = Foo(false)

function agent_nr(id::UInt64)
    id & (2 ^ 32 - 1)
end


cb = @benchmark agent_nr($rand(UInt64))

println(round(mean(cb.times), digits=1))

# prepare("Test2")

# b = run_benchmark("Test2")

# println(mean(b))

