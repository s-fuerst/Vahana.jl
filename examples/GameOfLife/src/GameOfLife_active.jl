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
        foreach(nid -> add_edge!(sim, id, nid, ActiveNeighbor()),
                edgeids(sim, id, Neighbor))
    end
    c
end

# The active calculation is from the Agents.jl implementation
# but seems to we wrong (rules[2] is never used)
function transition(c::Cell, id, sim)
    n = num_edges(sim, id, ActiveNeighbor)
    rules = param(sim, :rules)
    if (c.active == true && n <= rules[4] && n >= rules[1]) ||
        (c.active == false && n >= rules[3] && n <= rules[4])

        foreach(nid -> add_edge!(sim, id, nid, ActiveNeighbor()),
                edgeids(sim, id, Neighbor))

        return Cell(true)
    end
    Cell(false)
end

dim = (2000, 2000)
size = dim[1] * dim[2]

const model = ModelTypes() |>    
    register_agenttype!(Cell, :ConstantSize) |> 
    register_edgestatetype!(Neighbor, :Stateless, :SingleType; target = Cell, size=size) |>
    register_edgestatetype!(ActiveNeighbor, :Stateless, :IgnoreFrom, :SingleType; target = Cell , size=size) |>
    create_model("GameOfLife")


function init!(sim)
    add_raster!(sim,
                :raster,
                param(sim, :dims),
                _ -> rand() < 0.2 ? Cell(true) : Cell(false))

    connect_raster_neighbors!(sim, :raster, (_,_) -> Neighbor()) 

    finish_init!(sim)

    apply!(sim, initial_active,
                      [Cell],
                      [Cell, Neighbor, ActiveNeighbor],
                      [Cell, ActiveNeighbor])
    
end

function step!(sim)
    apply!(sim, transition,
                      [Cell],
                      [Cell, Neighbor, ActiveNeighbor],
                      [Cell, ActiveNeighbor])
    calc_rasterstate(sim, :raster, c -> c.active, Bool, Cell)
end

const sim = create_simulation(model,
                           Params(rules = SA[2,3,3,3],
                                  dims = dim),
                           nothing)

init!(sim)


#@benchmark
step!(sim)

# @btime apply!(sim, transition, [Cell], [Neighbor, ActiveNeighbor], [ActiveNeighbor])

if mpi.isroot
    @time for i in 1:200 step!(sim) end
else
    @time for i in 1:200 step!(sim) end
end    
