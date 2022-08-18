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

function prepare(name)
    traits = []
    
    if hastrait(name, "S")
        traits = vcat(traits, :Stateless)
    end
    if hastrait(name, "E")
        traits = vcat(traits, :SingleEdge)
    end
    if hastrait(name, "T")
        traits = vcat(traits, :SingleAgentType)
    end
    if hastrait(name, "I")
        traits = vcat(traits, :IgnoreFrom)
    end

    if hastrait(name, "F")
        mt = ModelTypes() |>
            register_agenttype!(AgentState) |>
            register_edgetype!(EdgeState, traits...; to_agenttype=AgentState, size=3)
    else
        mt = ModelTypes() |>
            register_agenttype!(AgentState) |>
            register_edgetype!(EdgeState, traits...; to_agenttype=AgentState)
    end    
    construct_model(mt, name)
end


function run_benchmark(mt, name)
    sim = new_simulation(mt, nothing, nothing)

    a1 = add_agent!(sim, AgentState())
    a2 = add_agent!(sim, AgentState())
    a3 = add_agent!(sim, AgentState())

    stateless = hastrait(name, "S")
    singleedge = hastrait(name, "E")
    singletype = hastrait(name, "T")
    ignorefrom = hastrait(name, "I")
    fixedsize = hastrait(name, "F")

    add_edge!(sim, a1, a2, EdgeState(0.1))
    add_edge!(sim, a2, a3, EdgeState(0.2))
    add_edge!(sim, a3, a1, EdgeState(0.3))

    sim_agg = deepcopy(sim)

    
    for i=1:9
        add_edge!(sim, a1, a3, EdgeState(i))
    end

    finish_init!(sim)
    finish_init!(sim_agg)

    edge = Edge(a2, EdgeState(2.0))

    sim_add = deepcopy(sim)
    addedge = @benchmark add_edge!($sim_add, $a1, $edge)


    if !stateless && !ignorefrom 
        edgeto = @benchmark edges_to($sim, $a3, EdgeState)
    else
        edgeto = nothing
    end

    if ignorefrom 
        nids = nothing
    else
        nids = @benchmark neighborids($sim, $a3, EdgeState)
    end

    if stateless 
        estates = nothing
    else
        estates = @benchmark edgestates($sim, $a3, EdgeState)
    end


    if stateless 
        agg = nothing
    else
        agg = @benchmark aggregate($sim_agg, s -> s.v, +, EdgeState)
    end
    

    if ignorefrom && stateless || !singleedge || !singletype
        hasn = @benchmark has_neighbor($sim, $a3, EdgeState)
    else
        hasn = nothing
    end
    
    if singleedge
        numn = nothing
    else
        numn = @benchmark num_neighbors($sim, $a3, EdgeState)
    end

    x(f) = f ? "x" : " "

    function pt(b)
        if b === nothing
            "-"
        else
            (round(minimum(b.times), digits=1) |> string) * "/" * (Int32(round(mean(b.times))) |> string)
        end
    end

    
    println("| $(x(stateless)) | $(x(singleedge)) | $(x(singletype))  | $(x(ignorefrom)) | $(x(fixedsize)) | $(pt(addedge)) | $(pt(edgeto)) |  $(pt(hasn)) | $(pt(numn)) | $(pt(nids)) | $(pt(estates)) | $(pt(agg)) |")
end

######################################## create edge table

println("| S | E | T | I | F | add_edge! | edges_to | has_neighbor | num_neighbors | neighborids | edgestates | aggregate |")

for t in allEdgeTypes
    mt = prepare(t)
    run_benchmark(mt, t)
    GC.gc()
end
    
######################################## raster move_to!

struct GridNode end

struct GridEdge end

struct MovingAgent end

struct OnPosition  end

struct AgentsOnPoint end


const sim = ModelTypes() |>
    register_agenttype!(GridNode) |>
    register_agenttype!(MovingAgent) |>
    register_edgetype!(GridEdge) |>
    register_edgetype!(OnPosition, :SingleEdge, :SingleAgentType; to_agenttype = GridNode) |>
    register_edgetype!(AgentsOnPoint, :SingleAgentType; to_agenttype = MovingAgent) |>
    construct_model("Raster_Test") |>
    new_simulation(nothing, nothing)

add_raster!(sim,
            :grid,
            (20, 12),
            _ -> GridNode())

id = add_agent!(sim, MovingAgent())


@benchmark move_to!(sim, :grid, id, (4, 3), nothing, nothing)

# @benchmark add_agent!(sim, MovingAgent())

sim.rasters[:grid][1,3]
