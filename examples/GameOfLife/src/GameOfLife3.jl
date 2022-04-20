# Game of Life Implementation
#
# The add_grid! function creates a cellular automata graph structure where
# each Cell is connected with 8 surrounding Cells.
#
# In this version the transition function send a message to all surrounding
# Cell (where send a message means: creates an edge of type
# ActiveNeighbor). 
using Vahana, Random

struct Cell <: Agent
    active::Bool
end

struct Neighbor <: EdgeState end

struct ActiveNeighbor <: EdgeState end

Base.@kwdef struct Params
    dims::Tuple{Int64, Int64}
    rules::Tuple
end

# We need to create the ActiveNeighbor edges after the initialization
# of the Cells
function initial_active(c::Cell, id, sim)
    if c.active
        foreach(e -> add_edge!(sim, id, e.from, ActiveNeighbor),
                edges_to(sim, id, Neighbor))
    end
    c
 end

# The active calculation is from the Agents.jl implementation
# but seems to we wrong (rules[2] is never used)
function transition(c::Cell, id, sim)
    n = edges_to(sim, id, ActiveNeighbor) |> length
    rules = param(sim, :rules)
    if (c.active == true && n <= rules[4] && n >= rules[1]) ||
        (c.active == false && n >= rules[3] && n <= rules[4])

        foreach(e -> add_edge!(sim, id, e.from, ActiveNeighbor),
                edges_to(sim, id, Neighbor))

        return Cell(true)
    end
    Cell(false)
 end

function init(params::Params)
    sim = Simulation("Game of Life", params, nothing)
    add_agenttype!(sim, Cell)
    add_edgetype!(sim, Neighbor)
    add_edgetype!(sim, ActiveNeighbor)

    add_raster!(sim, 
                param(sim, :dims),
                _ -> rand() < 0.2 ? Cell(true) : Cell(false),
                Neighbor();
                name = :raster)

    finish_init!(sim)

    apply_transition!(sim, initial_active,
                      [Cell], [Neighbor, ActiveNeighbor], [ActiveNeighbor])

    sim
end

function step!(sim)
    apply_transition!(sim, transition,
                      [Cell], [Neighbor, ActiveNeighbor], [ActiveNeighbor])
    calc_raster(sim, :raster, c -> c.active)
end

sim = init(Params(rules = (2,3,3,3),
                  dims = (200,200)))

step!(sim)
