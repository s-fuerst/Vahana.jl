# Game of Life Implementation
#
# The add_grid! function creates a cellular automata graph structure where
# each Cell is connected with 8 surrounding Cells.
#
# In this version the transition function send a message to all surrounding
# Cell (where send a message means: creates an edge of type
# ActiveNeighbor). 
using Vahana, Random, BenchmarkTools, StaticArrays, StatProfilerHTML

enable_asserts(false)

struct Cell 
    active::Bool
end

struct Neighbor end

struct ActiveNeighbor end

Base.@kwdef struct Params
    dims::Tuple{Int64, Int64}
    rules::SVector{4, Int64}
end

# We need to create the ActiveNeighbor edges after the initialization
# of the Cells
function initial_active(c::Cell, id, sim)
    if c.active
        foreach(e -> add_edge!(sim, id, e, ActiveNeighbor()),
                neighbors(sim, id, Val(Neighbor)))
    end
    c
end

# The active calculation is from the Agents.jl implementation
# but seems to we wrong (rules[2] is never used)
function transition(c::Cell, id, sim)
    n = num_neighbors(sim, id, Val(ActiveNeighbor))
    rules = param(sim, :rules)
    if (c.active == true && n <= rules[4] && n >= rules[1]) ||
        (c.active == false && n >= rules[3] && n <= rules[4])

        foreach(from -> add_edge!(sim, id, from, ActiveNeighbor()),
                neighbors(sim, id, Val(Neighbor)))

        return Cell(true)
    end
    Cell(false)
end

const model = ModelTypes() |>    
    add_agenttype!(Cell, :Vector) |> 
#    add_agenttype!(Cell) |> 
    add_edgetype!(Neighbor, :Stateless) |>
    add_edgetype!(ActiveNeighbor, :Stateless)


function new_sim(params)
    construct(model, "Game of Life", params, nothing)
end

function init!(sim)
    add_raster!(sim, 
                param(sim, :dims),
                _ -> rand() < 0.2 ? Cell(true) : Cell(false),
                Neighbor();
                name = :raster)


    finish_init!(sim)

    apply_transition!(sim, initial_active,
                      [Cell], [Neighbor, ActiveNeighbor], [ActiveNeighbor])
    
end

function step!(sim)
    apply_transition!(sim, transition,
                      [Cell], [Neighbor, ActiveNeighbor], [ActiveNeighbor])
    calc_raster(sim, :raster, c -> c.active, Val(Cell))
end

sim = new_sim(Params(rules = SA[2,3,3,3],
                     dims = (200,200)))

init!(sim)

step!(sim)

#@profilehtml for i in 1:20 step!(sim) end
