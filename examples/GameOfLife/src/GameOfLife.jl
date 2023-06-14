# Game of Life Implementation
#
# The add_grid! function creates a cellular automata graph structure where
# each Cell is connected with 8 surrounding Cells.
#
# In this version the transition function reads the state of the surrounding
# Cells to determine how many of them are active.

using Vahana, Random

detect_stateless_trait(true)

import Graphs

struct Cell 
    active::Bool
end

struct Neighbor  end

Base.@kwdef struct Params
    dims::Tuple{Int64, Int64}
    rules::Tuple
end

mutable struct Globals
    numactive::Vector{Int64}
end

# The active calculation is from the Agents.jl implementation
# but seems to we wrong (rules[2] is never used)
function transition(c::Cell, id, sim)
    n = mapreduce(a -> a.active, +, neighborstates(sim, id, Neighbor, Cell))
    rules = param(sim, :rules)
    if c.active == true && n <= rules[4] && n >= rules[1]
        return Cell(true)
    elseif c.active == false && n >= rules[3] && n <= rules[4]
        return Cell(true)
    end
    Cell(false)
 end

function addgrid!(sim)
#    r = calc_rasterstate(sim, :raster, c -> c.active, Cell)
    r = calc_raster(sim, :raster, Bool, Cell) do id
        agentstate(sim, id, Cell).active
    end
    
    push_global!(sim, :grid, sparse(r))
end 

model = ModelTypes() |>
    register_agenttype!(Cell, :Immortal) |>
    register_edgetype!(Neighbor, :SingleType; target = Cell) |>
    create_model("Game of Life")


function init(model)
    sim = create_simulation(model,
                            Params(rules = (2,3,3,3),
                                   dims = (200,200)),
                            Globals(Vector()))

    add_raster!(sim,
                :raster,
                param(sim, :dims),
                pos -> Cell(rand() < 0.2))

    connect_raster_neighbors!(sim, :raster, (_,_) -> Neighbor())

    finish_init!(sim)

    sim
end

function step!(sim)
    apply!(sim, transition, Cell, [Cell, Neighbor], Cell)
    countactive!(sim)
    with_logger(sim) do
        @info "<Begin> GC"
    end
    GC.gc(false)
    with_logger(sim) do
        @info "<End> GC"
    end
#     addgrid!(sim)
#     get_global(sim, :grid) |> last
end

sim = init()

step!(sim)

if mpi.isroot
    @time for i in 1:200 step!(sim) end
else
    @time for i in 1:200 step!(sim) end
end    


finish_simulation!(sim)

println(get_global(sim, :numactive)[10])
# this produces a nice view of the current state in the REPL
#get_global(sim, :grid) |> last
