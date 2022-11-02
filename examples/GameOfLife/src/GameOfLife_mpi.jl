# Game of Life Implementation
#
# The add_grid! function creates a cellular automata graph structure where
# each Cell is connected with 8 surrounding Cells.
#
# In this version the transition function reads the state of the surrounding
# Cells to determine how many of them are active.

using Vahana, Random, SparseArrays, BenchmarkTools

import Graphs

struct Cell 
    active::Bool
    pos::Tuple{Int64, Int64}
end

struct Neighbor  end

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
    n = mapreduce(a -> a.active, +, neighborstates(sim, id, Neighbor, Cell))
    rules = param(sim, :rules)
    if c.active == true && n <= rules[4] && n >= rules[1]
        return Cell(true, c.pos)
    elseif c.active == false && n >= rules[3] && n <= rules[4]
        return Cell(true, c.pos)
    end
    Cell(false, c.pos)
 end

function countactive!(sim)
    pushglobal!(sim, :numactive, aggregate(sim, c -> c.active, +, Cell))
end

function addgrid!(sim)
#    r = calc_rasterstate(sim, :raster, c -> c.active, Cell)
    r = calc_raster(sim, :raster, Bool, [ Cell ]) do id
        agentstate(sim, id, Cell).active
    end
    
    pushglobal!(sim, :grid, sparse(r))
end 

model = ModelTypes() |>
    register_agenttype!(Cell, :ConstantSize; size = 200*200) |>
    register_edgetype!(Neighbor; to_agenttype = Cell) |>
    construct_model("Game of Life")


function init(params::Params)
    sim = new_simulation(model, params, Globals(Vector(), Vector()))

    add_raster!(sim,
                :raster,
                param(sim, :dims),
                pos -> rand() < 0.2 ? Cell(true, pos) : Cell(false, pos))

    connect_raster_neighbors!(sim, :raster, (_,_) -> Neighbor())

    finish_init!(sim)

    countactive!(sim)
    
    sim
end

function step!(sim)
    apply_transition!(sim, transition, [Cell], [Cell, Neighbor], [])
    countactive!(sim)
#     addgrid!(sim)
#     getglobal(sim, :grid) |> last
end

sim = init(Params(rules = (2,3,3,3),
                  dims = (200,200)))

# step!(sim)

# @time for _ in 1:10
#     step!(sim)
# end

@btime step!(sim)

finish_simulation!(sim)

println(getglobal(sim, :numactive)[10])
# this produces a nice view of the current state in the REPL
#getglobal(sim, :grid) |> last
