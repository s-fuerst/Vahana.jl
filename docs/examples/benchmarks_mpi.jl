using Vahana
using BenchmarkTools

enable_asserts(false)

suppress_warnings(true)

detect_stateless_trait(true)

struct AgentState end

struct EdgeState
    v::Float64
end

statelessEdgeTypes = [ "EdgeS", "EdgeSE", "EdgeST", "EdgeSI", "EdgeSEI", "EdgeSTI", "EdgeSETI",
                       "EdgeSTF", "EdgeSTFI", "EdgeSETFI" ]

statefulEdgeTypes = [ "edge", "EdgeE", "EdgeT", "EdgeI", "EdgeEI", "EdgeTI",
                      "EdgeTF", "EdgeTFI" ]

allEdgeTypes = vcat(statefulEdgeTypes, statelessEdgeTypes)

#allEdgeTypes = [ "EdgeT", "EdgeTF" ]


hastrait(type, trait::String) = occursin(trait, SubString(String(Symbol(type)), 5))

function pt(b)
    if b === nothing
        "-"
    else
        (round(minimum(b.times), digits=1) |> string) * "/" * (Int32(round(mean(b.times))) |> string)
    end
end


function prepare(name)
    traits = []
    
    if hastrait(name, "S")
        traits = vcat(traits, :Stateless)
    end
    if hastrait(name, "E")
        traits = vcat(traits, :SingleEdge)
    end
    if hastrait(name, "T")
        traits = vcat(traits, :SingleType)
    end
    if hastrait(name, "I")
        traits = vcat(traits, :IgnoreFrom)
    end

    if hastrait(name, "F")
        mt = ModelTypes() |>
            register_agenttype!(AgentState) |>
            register_edgestatetype!(EdgeState, traits...; target=AgentState, size=3)
    else
        mt = ModelTypes() |>
            register_agenttype!(AgentState) |>
            register_edgestatetype!(EdgeState, traits...; target=AgentState)
    end    
    create_model(mt, name)
end

function createsim(foragg, mt, name)
    sim = create_simulation(mt, nothing, nothing)

    a1 = add_agent!(sim, AgentState())
    a2 = add_agent!(sim, AgentState())
    a3 = add_agent!(sim, AgentState())

    add_edge!(sim, a1, a2, EdgeState(0.1))
    add_edge!(sim, a2, a3, EdgeState(0.2))
    add_edge!(sim, a3, a1, EdgeState(0.3))

    if ! foragg
        for i=1:9
            add_edge!(sim, a1, a3, EdgeState(i))
        end
    end

    finish_init!(sim)
    
    (sim, a1, a2, a3)
end

function run_benchmark(mt, name)
    stateless = hastrait(name, "S")
    singleedge = hastrait(name, "E")
    singletype = hastrait(name, "T")
    ignorefrom = hastrait(name, "I")
    fixedsize = hastrait(name, "F")

    (sim, a1, a2, a3) = createsim(false, mt, name)

    edge = Edge(a2, EdgeState(2.0))

    addedge = @benchmark add_edge!($sim, $a1, $edge)

    finish_simulation!(sim)

    (sim, a1, a2, a3) = createsim(false, mt, name)

    if !stateless && !ignorefrom
        edgeto = @benchmark edges($sim, $a3, EdgeState)
    else
        edgeto = nothing
    end

    if ignorefrom 
        nids = nothing
    else
        nids = @benchmark edgeids($sim, $a3, EdgeState)
    end

    if stateless 
        estates = nothing
    else
        estates = @benchmark edgestates($sim, $a3, EdgeState)
    end

    if ignorefrom && stateless || !singleedge || !singletype
        hasn = @benchmark has_neighbor($sim, $a3, EdgeState)
    else
        hasn = nothing
    end
    
    if singleedge
        numn = nothing
    else
        numn = @benchmark num_edges($sim, $a3, EdgeState)
    end

    finish_simulation!(sim)

    (sim, a1, a2, a3) = createsim(true, mt, name)

    if stateless 
        agg = nothing
    else
        agg = nothing
#        agg = @benchmark mapreduce($sim, s -> s.v, +, EdgeState)
    end

    finish_simulation!(sim)
    
    x(f) = f ? "x" : " "

    println("| $(x(stateless)) | $(x(singleedge)) | $(x(singletype))  | $(x(ignorefrom)) | $(x(fixedsize)) | $(pt(addedge)) | $(pt(edgeto)) |  $(pt(hasn)) | $(pt(numn)) | $(pt(nids)) | $(pt(estates)) | $(pt(agg)) |")
end

######################################## create edge table

println("| S | E | T | I | F | add_edge! | edges | has_neighbor | num_edges | edgeids | edgestates | mapreduce |")

for t in allEdgeTypes
    mt = prepare(t)
    run_benchmark(mt, t)
    GC.gc()
end



######################################## Agent functions

struct AgentWithState state::Int64 end

println("| traits  | add_agent | agentstate | agentstate_flexible | mapreduce |")

function run_benchmark_agents(mt)
    sim = create_simulation(mt, nothing, nothing)

    ids = add_agents!(sim, [ AgentWithState(i) for i in 1:10 ])

    finish_init!(sim)
    
    raggregate = @benchmark mapreduce($sim, a -> a.state, +, AgentWithState)
    raddagent = @benchmark add_agent!($sim, AgentWithState(1))
    ragentstate = @benchmark agentstate($sim, $(ids[1]), AgentWithState)
    ragentstateflex = @benchmark agentstate_flexible($sim, $(ids[1]))

    println("| $(mt.name) | $(pt(raddagent))| $(pt(ragentstate)) | $(pt(ragentstateflex)) | $(pt(raggregate)) | ")
end

mt = ModelTypes() |>
    register_agenttype!(AgentWithState) |>
    create_model("Mortal");
run_benchmark_agents(mt);

mt = ModelTypes() |>
    register_agenttype!(AgentWithState, :Immortal) |>
    create_model("Immortal");
run_benchmark_agents(mt);

# ######################################## raster move_to!

# struct GridNode end

# struct GridEdge end

# struct MovingAgent end

# struct OnPosition  end

# struct AgentsOnPoint end


# const sim = ModelTypes() |>
#     register_agenttype!(GridNode) |>
#     register_agenttype!(MovingAgent) |>
#     register_edgestatetype!(GridEdge) |>
#     register_edgestatetype!(OnPosition, :SingleEdge, :SingleType; target = GridNode) |>
#     register_edgestatetype!(AgentsOnPoint, :SingleType; target = MovingAgent) |>
#     create_model("Raster_Test") |>
#     create_simulation(nothing, nothing)

# add_raster!(sim,
#             :grid,
#             (20, 12),
#             _ -> GridNode())

# id = add_agent!(sim, MovingAgent())


# @benchmark move_to!(sim, :grid, id, (4, 3), nothing, nothing)

# # @benchmark add_agent!(sim, MovingAgent())

# sim.rasters[:grid][1,3]
