# # Predator/Prey Model

# This example shows how spatial information can be integrated into a
# model. In its current version, Vahana is not the ideal tool for models
# that depend mainly on the movement of agents, as in this example, but
# as shown here, it is still possible, even though we are actually
# only working with graphs.

# The model is based on the [Predator-Prey for High-Performance
# Computing](https://peerj.com/articles/cs-36/) (PPHPC) model. In
# PPHPC the agents move randomly, in our implementation the prey move
# to locations with grass (if one is in sight) and the predators move to
# locations with prey to demonstrate how features like this can be
# implemented in Vahana.

using Vahana

using Random

Random.seed!(1); #hide

# We want to run the model with optimized performance, see
# [Performance Tuning](performance.html) for details.

detect_stateless_trait(true)

enable_asserts(true);

# ## AgentTypes

# We have three types of agents, besides the predator and the prey we
# also have the grid cells, which represent the spatial environment
# but are also implemented as agents and thus as nodes of the graph in
# Vahana.

struct Predator
    energy::Int64
    pos::CartesianIndex{2}
end

struct Prey
    energy::Int64
    pos::CartesianIndex{2}
end

struct Cell
    pos::CartesianIndex{2}
    countdown::Int64
end

# ## Edges

# All agents have their position as part of their state, but e.g. the
# cell can only "see" the animals if they are connected to the cell via
# an edge. Vahana supports the creation of such edges via the
# [`move_to!`](@ref) function.

# Prey/PredatorPosition are edges from the prey/predator to the
# cell. We use two different types (and not just one type Position)
# because this allows the cell to access only the prey or predator on
# the cell.

struct PreyPosition end

struct PredatorPosition end

# PreyView and PredatorView are edges from the cells to the prey and
# predator respectively. These edges represent the cells that are
# visible to a prey or predator so that, for example, it is possible to
# check which cell in the visible area contains food.

struct PreyView end

struct PredatorView end

# VisiblePrey are edges from prey to predator. So far, all edge types
# connect animals to cells, and without an incoming edge from a prey,
# a predator don't know anything about the prey positions. The
# FollowPrey edges are created by the cells through the `find_prey`
# transition function, as the cells know which Prey is directly on the
# cell via the PreyPosition edges and therefore in the view of an
# predator via the PredatorView edges. 

struct VisiblePrey end

# The last two edge types are messages to inform the agents that they
# must die or found something to eat.

struct Die end

struct Eat end

# ## Params

# We follow the parameter structure from the PPHPC model, but use the
# same parameters for Prey and Predator for the example run.

Base.@kwdef struct SpeciesParams
    gain_from_food::Int64 = 10
    loss_per_turn::Int64 = 1
    repro_thres::Int64 = 5
    repro_prob::Int64 = 50
end

Base.@kwdef struct AllParams
    raster_size::Tuple{Int64, Int64} = (100, 100)
    restart::Int64 = 5
    predator::SpeciesParams = SpeciesParams()
    prey::SpeciesParams = SpeciesParams()
    num_predators::Int64 = 500
    num_prey::Int64 = 2000
end

# ## Globals

# We are creating timeseries for the predator and prey population, the
# number of cells with food and the average energy of the predators
# and prey.

Base.@kwdef mutable struct PPGlobals
    predator_pop = Vector{Int64}()
    prey_pop = Vector{Int64}()
    cells_with_food = Vector{Int64}()
    mean_predator_energy = Vector{Float64}()
    mean_prey_energy = Vector{Float64}()
end

# ## Create Simulation

# We have now defined all the Julia structs needed to create the model
# and a simulation. We add some traits, mainly for performance reasons.


const ppsim = ModelTypes() |>
    register_agenttype!(Predator) |>
    register_agenttype!(Prey) |>
    register_agenttype!(Cell) |>
    register_edgetype!(PredatorPosition, :SingleAgentType; to_agenttype = Cell) |>
    register_edgetype!(PreyPosition, :SingleAgentType; to_agenttype = Cell) |>
    register_edgetype!(PredatorView) |>
    register_edgetype!(PreyView) |>
    register_edgetype!(VisiblePrey, :SingleAgentType; to_agenttype = Predator) |> 
    register_edgetype!(Die, :HasEdgeOnly) |>
    register_edgetype!(Eat, :HasEdgeOnly) |>
    construct_model("Predator Prey") |>
    new_simulation(AllParams(), PPGlobals())

# ## Initialization

# First we add the Cells to the Simulation. Therefore we define a
# constructor functions for the cells. There is a 50% probability that a cell
# contains food (and in this case `countdown` is 0).

init_cell(pos::CartesianIndex) =
    Cell(pos, rand() < 0.5 ? 0 : rand(1:param(ppsim, :restart)))

add_raster!(ppsim, :raster, param(ppsim, :raster_size), init_cell)

connect_raster_neighbors!(ppsim, :raster, (_,_) -> PreyView(); periodic = false)

# Predator and Pray are starting on a random position.

function random_pos(sim)
    size = param(sim, :raster_size)
    CartesianIndex(rand(1:size[1]), rand(1:size[2]))
end

# We define two helper functions to move an agent to a new
# position. This will always add a (pos)edge from the agent to the
# cell at the `newpos` position. If the viewedge argument is
# specified, (view)edges will also be added from the agent to the cell
# and its neighbors.

function move!(sim, id, newpos, posedge, viewedge = nothing)
    move_to!(sim, :raster, id, newpos, nothing, posedge())
    if ! isnothing(viewedge)
        move_to!(sim, :raster, id, newpos, viewedge(), viewedge();
                 distance = 1, metric = :manhatten)
    end
end

# Now we can add the Predator and Pray to the simulation, and use
# the `move!` function to create also the position and view edges.

energyrange = 1:(2*param(ppsim, :prey).gain_from_food)
foreach(1:param(ppsim, :num_prey)) do _
    energy = rand(energyrange)
    pos = random_pos(ppsim)
    id = add_agent!(ppsim, Prey(energy, pos))
    move!(ppsim, id, pos, PreyPosition, PreyView)
end

energyrange = 1:(2*param(ppsim, :predator).gain_from_food)
foreach(1:param(ppsim, :num_predators)) do _
    energy = rand(energyrange)
    pos = random_pos(ppsim)
    id = add_agent!(ppsim, Predator(energy, pos))
    move!(ppsim, id, pos, PredatorPosition, PredatorView)
end

# At the end of the initialization we have to call finish_init!

finish_init!(ppsim)

# ## Transition Functions

# As mentioned in the comment for the PredatorView, the cells are
# responsible to connect the Prey with the Predators. So each cell
# iterate over each prey on the cell and add edges to all predators
# the can view the cell.

function find_prey(state::Cell, id, sim)
    checked(foreach, neighborids(sim, id, PreyPosition)) do preyid
        checked(foreach, neighborids(sim, id, PredatorView)) do predid
            add_edge!(sim, preyid, predid, VisiblePrey())
        end
    end
    state
end


function move(state::Predator, id, sim)
    e = state.energy - param(sim, :predator).loss_per_turn
    if e > 0
        prey = neighborstates(sim, id, VisiblePrey, Prey)
        newpos = if isnothing(prey)
            nextcellid = rand(neighborids(sim, id, PredatorView))
            agentstate(sim, nextcellid, Cell).pos
        else
            rand(prey).pos
        end
        move!(sim, id, newpos, PredatorPosition)
        Predator(e, newpos)
    else
        nothing
    end
end

function move(state::Prey, id, sim) 
    e = state.energy - param(sim, :prey).loss_per_turn
    if e > 0
        withgrass = filter(neighborids(sim, id, PreyView)) do id
            agentstate(sim, id, Cell).countdown == 0
        end
        nextcellid = if length(withgrass) == 0
            rand(neighborids(sim, id, PreyView))
        else
            rand(withgrass)
        end
        newpos = agentstate(sim, nextcellid, Cell).pos
        move!(sim, id, newpos, PreyPosition)
        Prey(e, newpos)
    else
        nothing
    end
end

function grow_food(state::Cell, _, _)
    Cell(state.pos, max(state.countdown - 1, 0))
end

function try_eat(state::Cell, id, sim)
    predators = neighborids(sim, id, PredatorPosition)
    prey = neighborids(sim, id, PreyPosition)

    ## first the predators eat the prey, in case that both are on the cell
    if ! isnothing(predators) && ! isnothing(prey)
        prey = Set(prey)
        for pred in predators
            if length(prey) > 0
                p = rand(prey)
                add_edge!(sim, id, p, Die())
                add_edge!(sim, id, pred, Eat())
                delete!(prey, p)
            end
        end
    end

    ## then check if there is prey left that can eat the grass
    if  ! isnothing(prey) && length(prey) > 0 && state.countdown == 0
        add_edge!(sim, id, rand(prey), Eat())
        Cell(state.pos, param(sim, :restart))
    else
        state
    end
end

function try_reproduce_imp(state, id, sim, C, posedge, viewedge, species_params)
    move!(sim, id, state.pos, posedge, viewedge) # ist das wirklich notwendig?
    if has_neighbor(sim, id, Eat)
        state = C(state.energy + species_params.gain_from_food, state.pos)
    end
    if state.energy > species_params.repro_thres &&
        rand() * 100 < species_params.repro_prob

        energy_offspring = Int64(round(state.energy / 2))
        newid = add_agent!(sim, C(energy_offspring, state.pos))
        move!(sim, newid, state.pos, posedge, viewedge)
        C(state.energy - energy_offspring, state.pos)
    else
        state
    end
end

try_reproduce(state::Predator, id, sim) =
    try_reproduce_imp(state, id, sim, Predator,
                    PredatorPosition, PredatorView, param(sim, :predator))

function try_reproduce(state::Prey, id, sim)
    if has_neighbor(sim, id, Die)
        return nothing
    end
    try_reproduce_imp(state, id, sim, Prey,
                    PreyPosition, PreyView, param(sim, :prey))
end

function update_globals(sim)
    pushglobal!(sim, :predator_pop, aggregate(sim, _ -> 1, +, Predator; init = 0))
    pushglobal!(sim, :prey_pop, aggregate(sim, _ -> 1, +, Prey; init = 0))
    pushglobal!(sim, :cells_with_food,
                aggregate(sim, c -> c.countdown == 0, +, Cell))
    pushglobal!(sim, :mean_predator_energy,
                aggregate(sim, p -> p.energy, +, Predator; init = 0) /
                    last(getglobal(sim, :predator_pop)))
    pushglobal!(sim, :mean_prey_energy,
                aggregate(sim, p -> p.energy, +, Prey; init = 0) /
                    last(getglobal(sim, :prey_pop)))
end

update_globals(ppsim)

function step!(sim)
    apply_transition!(sim, move,
                      [ Prey ],
                      [ PreyView ],
                      [ PreyPosition ])

    apply_transition!(sim, find_prey,
                      [ Cell ],
                      [ PreyPosition, PredatorView ],
                      [ VisiblePrey ])
    
    apply_transition!(sim, move,
                      [ Predator ],
                      [ PredatorView, VisiblePrey ],
                      [ PredatorPosition ])

    apply_transition!(sim, grow_food, [ Cell ], [], [])
    
    apply_transition!(sim, try_eat,
                      [ Cell ],
                      [ PredatorPosition, PreyPosition ],
                      [ Die, Eat ])

    apply_transition!(sim, try_reproduce,
                      [ Predator, Prey ],
                      [ Die, Eat ],
                      [ PredatorPosition, PreyPosition,
                        PredatorView, PreyView ])

    update_globals(sim)
end

for _ in 1:200 step!(ppsim) end

# ## Plots

using CairoMakie
using Makie

## t is PreyPosition or PredatorPosition
function plot_agents_on_cell(sim, t)
    calc_raster(sim, :raster) do id
        num_neighbors(sim, id, t)
    end |> heatmap
end

# First the time series

plotglobals(ppsim, [ :predator_pop, :prey_pop, :cells_with_food ])

# And as an example a heatmap that show how many predators are on a
# cell

plot_agents_on_cell(ppsim, PredatorPosition)

# # Tests

using Test

Base.@kwdef struct TestParams
    raster_size::Tuple{Int64, Int64}
    restart::Int64
    predator::SpeciesParams
    prey::SpeciesParams
end

function runtests()
    predator_params = SpeciesParams(
        gain_from_food = 10,
        loss_per_turn = 1,
        repro_thres = 6,
        repro_prob = 100
    )

    prey_params = SpeciesParams(
        gain_from_food = 10,
        loss_per_turn = 1,
        repro_thres = 6,
        repro_prob = 100
    )

    testparams = TestParams(
        raster_size = (100, 100),
        restart = 5,
        predator = predator_params,
        prey = prey_params
    )
    
    test = new_simulation(model, testparams, nothing; name = "Test")
    
    @testset "PredatorPrey" begin
        cellids = add_raster!(test, :raster, testparams.raster_size,
                              pos -> Cell(pos, (pos[1]*3 + pos[2]) % 20))

        ## prey1 should be removed in move
        prey1 = add_agent!(test, Prey(1, CartesianIndex(2,2)))
        move_prey!(test, prey1, CartesianIndex(2,2), true)
        ## prey2 should move to Pos(5,5) and Eat (and reproduce)
        prey2 = add_agent!(test, Prey(5, CartesianIndex(5,6)))
        move_prey!(test, prey2, CartesianIndex(5,6), true)
        ## prey3 should not find anything to Eat, but still reproduce
        prey3 = add_agent!(test, Prey(9, CartesianIndex(7,7)))
        move_prey!(test, prey3, CartesianIndex(7,7), true)
        ## prey4 shoud be eaten by pred2
        prey4 = add_agent!(test, Prey(5, CartesianIndex(12,12)))
        move_prey!(test, prey4, CartesianIndex(12,12), true)
        ## pred1 should be removed in move
        pred1 = add_agent!(test, Predator(1, CartesianIndex(17, 17)))
        move_predator!(test, pred1, CartesianIndex(17, 17), true)
        ## pred2 should be move to pos(12,12) and eat prey4                   
        pred2 = add_agent!(test, Predator(5, CartesianIndex(12,13)))
        move_predator!(test, pred2, CartesianIndex(12, 13), true)

        finish_init!(test)
        
        apply_transition!(test, find_prey,
                          [ Cell ],
                          [ PreyPosition, PredatorView ],
                          [ VisiblePrey ])

        @test num_edges(test, VisiblePrey) == 1

        apply_transition!(test, move,
                          [ Prey ],
                          [ PreyView ],
                          [ PreyPosition ])

        apply_transition!(test, move,
                          [ Predator ],
                          [ PredatorView, VisiblePrey ],
                          [ PredatorPosition ])
        
        ## prey and predator removed?
        @test num_agents(test, Predator) == 1    
        @test num_agents(test, Prey) == 3

        ## energy reduced by one
        @test agentstate(test, prey2, Prey).energy == 4

        ## prey2 moved to (5,5) as there is something to eat    
        @test agentstate(test, prey2, Prey).pos == CartesianIndex(5,5)
        
        apply_transition!(test, grow_food, [ Cell ], [], [])
        @test agentstate(test, cellids[1][1], Cell).countdown == 3    
        
        apply_transition!(test, try_eat,
                          [ Cell ],
                          [ PredatorPosition, PreyPosition ],
                          [ Die, Eat ])
        @test num_edges(test, Die) == 1
        @test num_edges(test, Eat) == 2
        @test agentstate(test, cellids[5,5], Cell).countdown == params.restart 
        
        apply_transition!(test, try_reproduce,
                          [ Predator, Prey ],
                          [ Die, Eat ],
                          [ PredatorPosition, PreyPosition,
                            PredatorView, PreyView ])

        ## two offsprings, one was eaten: 3 + 2 - 1
        @test num_agents(test, Prey) == 4
        @test agentstate(test, prey2, Prey).energy == 7
        @test agentstate(test, prey3, Prey).energy == 4
    end
end



