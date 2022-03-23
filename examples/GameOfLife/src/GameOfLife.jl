# Game of Life Implementation
#
# The add_grid! function creates a cellular automata graph structure where
# each Cell is connected with 8 surrounding Cells.
#
# In this version the transition function reads the state of the surrounding
# Cells to determine how many of them are active.

using Vahana, Random, SparseArrays

import Graphs

struct Cell <: Agent
    active::Bool
    pos::Tuple{Int64, Int64}
end

struct Neighbor <: EdgeState end

Base.@kwdef struct Params
    dims::Tuple{Int64, Int64}
    rules::Tuple
end

mutable struct Globals
    numactive::Vector{Int64}
    grid::Vector{SparseMatrixCSC}
end

# The active calculation is from the Agents.jl implementation
# but seems to we wrong (rules[2] is never used)
function transition(c::Cell, id, sim)
    n = mapreduce(a -> a.active, +, neighborstates(sim, id, Neighbor))
    rules = param(sim, :rules)
    if c.active == true && n <= rules[4] && n >= rules[1]
        return Cell(true, c.pos)
    elseif c.active == false && n >= rules[3] && n <= rules[4]
        return Cell(true, c.pos)
    end
    Cell(false, c.pos)
 end

function countactive!(sim)
    pushglobal!(sim, :numactive, aggregate(sim, Cell, c -> c.active, +))
end

function addgrid!(sim)
    dims = param(sim, :dims)
    spm4c(c::Cell) = sparse([c.pos[1]], [c.pos[2]],
                            [c.active],
                            dims[1], dims[2])
    pushglobal!(sim, :grid, aggregate(sim, Cell, spm4c, +))
end 

function init(params::Params)
    sim = Simulation("Game of Life", params, Globals(Vector(), Vector()))
    add_agenttype!(sim, Cell)
    add_edgetype!(sim, Neighbor)

    add_grid!(sim, 
              param(sim, :dims),
              pos -> rand() < 0.2 ? Cell(true, pos) : Cell(false, pos),
              Neighbor())

    finish_init!(sim)

    countactive!(sim)
    
    sim
end

function step!(sim)
    apply_transition!(sim, transition, [Cell], [Neighbor], [])
    countactive!(sim)
    addgrid!(sim)
    getglobal(sim, :grid) |> last
end

sim = init(Params(rules = (2,3,3,3),
                  dims = (200,200)))

step!(sim)

# this produces a nice view of the current state in the REPL
getglobal(sim, :grid) |> last
