# Game of Life Implementation
#
# The add_grid! function creates a cellular automata graph structure where
# each Cell is connected with 8 surrounding Cells.
#
# In this version the transition function send a message to all surrounding
# Cell (where send a message means: creates an edge of type
# ActiveNeighbor). 
using Vahana, Random, SparseArrays

import Graphs

struct Cell <: Agent
    active::Bool
    pos::Tuple{Int64, Int64}
end

struct Neighbor <: EdgeState end

struct ActiveNeighbor <: EdgeState end

Base.@kwdef struct Params
    dims::Tuple{Int64, Int64}
    rules::Tuple
end

mutable struct Globals
    numactive::Vector{Int64}
    grid::Vector{SparseMatrixCSC}
end

# We need to create the ActiveNeighbor edges after the initialization
# of the Cells
function initial_active(c::Cell, id, sim)
    if c.active
        for e in edges_to(sim, id, Neighbor)
            add_edge!(sim, id, e.from, ActiveNeighbor)
        end
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

        for e in edges_to(sim, id, Neighbor)
            add_edge!(sim, id, e.from, ActiveNeighbor)
        end
        return Cell(true, c.pos)
    end
    Cell(false, c.pos)
 end

function countactive!(sim)
    pushglobal!(sim, :numactive, aggregate(sim, Cell, c -> c.active, +))
end

function addgrid!(sim)
    dims = param(sim, :dims)
    spm4c(c::Cell) = sparse([c.pos[1] + 1], [c.pos[2] + 1],
                            [c.active],
                            dims[1], dims[2])
    pushglobal!(sim, :grid, calcgrid(sim))
end 

function calcgrid(sim)
    dims = param(sim, :dims)
    spm4c(c::Cell) = sparse([c.pos[1]], [c.pos[2]],
                            [c.active],
                            dims[1], dims[2])
    aggregate(sim, Cell, spm4c, +)
end 

function init(params::Params)
    sim = Simulation("Game of Life", params, Globals(Vector(), Vector()))
    add_agenttype!(sim, Cell)
    add_edgetype!(sim, Neighbor)
    add_edgetype!(sim, ActiveNeighbor)

    add_grid!(sim, 
              param(sim, :dims),
              pos -> rand() < 0.2 ? Cell(true, pos) : Cell(false, pos),
              Neighbor())

    finish_init!(sim)

    apply_transition!(sim, initial_active,
                      [Cell], [Neighbor, ActiveNeighbor], [ActiveNeighbor])
    
    countactive!(sim)
    
    sim
end

function step!(sim)
    apply_transition!(sim, transition,
                      [Cell], [Neighbor, ActiveNeighbor], [ActiveNeighbor])
    countactive!(sim)
    addgrid!(sim)
    getglobal(sim, :grid) |> last
end

sim = init(Params(rules = (2,3,3,3),
                  dims = (200,200)))

step!(sim)

# this produces a nice view of the current state in the REPL
getglobal(sim, :grid) |> last
