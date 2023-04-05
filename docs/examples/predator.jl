# # Predator/Prey Model

# This example shows how spatial information can be integrated into a
# model. In its current version, Vahana is not the ideal tool for
# models that depend mainly on the movement of agents, but as shown
# here, it is still possible, even though we are actually only working
# with graphs.

# The model is based on the [Predator-Prey for High-Performance
# Computing](https://peerj.com/articles/cs-36/) (PPHPC) model. In
# PPHPC the agents move randomly, in our implementation the prey move
# to locations with grass (if one is in sight) and the predators move to
# locations with prey to demonstrate how features like this can be
# implemented in Vahana.

using CairoMakie

using Vahana

using Random

Random.seed!(1); #hide

# We want to run the model with optimized performance, see
# [Performance Tuning](performance.html) for details.

detect_stateless_trait(true)

enable_asserts(false);

# ## AgentTypes

# Beside the predator and the prey we also have the grid cells, which
# represent the spatial environment but are also implemented as agents
# and thus as nodes of the graph.

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

# All agents have their position as part of their state, but a
# cell can only "see" the animals if they are connected to the cell via
# an edge. Vahana supports the creation of such edges via the
# [`move_to!`](@ref) function.

# Prey/PredatorPosition are edges from the prey/predator to the
# cell. We use two different types (and not just one type Position)
# because this allows the cell to differentiate between prey or
# predators by the type of the edges.

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
# VisiblePrey edges are created by the cells through the `find_prey`
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
    gain_from_food::Int64 = 5
    loss_per_turn::Int64 = 1
    repro_thres::Int64 = 5
    repro_prob::Int64 = 20
end

Base.@kwdef struct AllParams
    raster_size::Tuple{Int64, Int64} = (100, 100)
    restart::Int64 = 5
    predator::SpeciesParams = SpeciesParams()
    prey::SpeciesParams = SpeciesParams()
    num_predators::Int64 = 500
    num_prey::Int64 = 2000
end;

# ## Globals

# We are creating timeseries (in form of `Vector`s) for the predator and
# prey population, the number of cells with food and the average
# energy of the predators and prey.

Base.@kwdef mutable struct PPGlobals
    predator_pop = Vector{Int64}()
    prey_pop = Vector{Int64}()
    cells_with_food = Vector{Int64}()
    mean_predator_energy = Vector{Float64}()
    mean_prey_energy = Vector{Float64}()
end;

# ## Create the Simulation

# We have now defined all the Julia structs needed to create the model
# and a simulation. We also add some traits to some edgetypes, mainly
# for performance reasons.


const ppsim = ModelTypes() |>
    register_agenttype!(Predator) |>
    register_agenttype!(Prey) |>
    register_agenttype!(Cell) |>
    register_edgestatetype!(PredatorPosition, :SingleType; target = Cell) |>
    register_edgestatetype!(PreyPosition, :SingleType; target = Cell) |>
    register_edgestatetype!(PredatorView) |>
    register_edgestatetype!(PreyView) |>
    register_edgestatetype!(VisiblePrey, :SingleType; target = Predator) |> 
    register_edgestatetype!(Die, :HasEdgeOnly) |>
    register_edgestatetype!(Eat, :HasEdgeOnly) |>
    create_model("Predator Prey") |>
    create_simulation(AllParams(), PPGlobals())

# ## Initialization

# First we add the Cells to the Simulation. Therefore we define a
# constructor functions for the cells. There is a 50% probability that a cell
# contains food (and in this case `countdown` is 0).

init_cell(pos::CartesianIndex) =
    Cell(pos, rand() < 0.5 ? 0 : rand(1:param(ppsim, :restart)))

add_raster!(ppsim, :raster, param(ppsim, :raster_size), init_cell)

# Predator and Pray are starting on a random position.

function random_pos(sim)
    size = param(sim, :raster_size)
    CartesianIndex(rand(1:size[1]), rand(1:size[2]))
end;

# We define two helper functions to move an animal to a new
# position. This will add a single (pos)edge from the animal to the
# cell at the `newpos` position, and for all cells in the manhatten
# distance of 1 also (view)edges from those cell to the animal and
# from the animal to the cell.

function move!(sim, id, newpos, posedge, viewedge)
    move_to!(sim, :raster, id, newpos, nothing, posedge)
    move_to!(sim, :raster, id, newpos, viewedge, viewedge;
             distance = 1, metric = :manhatten)
end;

# Now we can add the Predator and Pray to the simulation, and use
# the `move!` function to create also the position and view edges.

energyrange = 1:(2*param(ppsim, :prey).gain_from_food)
foreach(1:param(ppsim, :num_prey)) do _
    energy = rand(energyrange)
    pos = random_pos(ppsim)
    id = add_agent!(ppsim, Prey(energy, pos))
    move!(ppsim, id, pos, PreyPosition(), PreyView())
end

energyrange = 1:(2*param(ppsim, :predator).gain_from_food)
foreach(1:param(ppsim, :num_predators)) do _
    energy = rand(energyrange)
    pos = random_pos(ppsim)
    id = add_agent!(ppsim, Predator(energy, pos))
    move!(ppsim, id, pos, PredatorPosition(), PredatorView())
end

# At the end of the initialization phase we have to call finish_init!

finish_init!(ppsim)

# ## Transition Functions

# As mentioned in the comment for the PredatorView, the cells are
# responsible to connect the Prey with the Predators. So each cell
# iterate over each prey on the cell and add edges to all predators
# the can view the cell.

# It may seem strange that the transition function does not return the state of
# the cell. The `apply!` method has the keyword argument
# `invariant_compute`, if this is set to true as we do this later when
# we call `find_prey`, Vahana knows that the state of the agents being
# called will not be change and the return values of the transition
# functions are ignored.

function find_prey(_, id, sim)
    checked(foreach, edgeids(sim, id, PreyPosition)) do preyid
        checked(foreach, edgeids(sim, id, PredatorView)) do predid
            add_edge!(sim, preyid, predid, VisiblePrey())
        end
    end
end;

# If a predator has enough energy left to move and there is a prey in the
# predator's field of view, a random prey is selected and its position
# is used as the new position. If no prey is visible, a random cell in
# the predator's field of view is selected as the new position.

# If there is not enough energy left, the transition function returns `nothing`,
# which means that the predator dies and is no longer part of the graph.

function move(state::Predator, id, sim)
    e = state.energy - param(sim, :predator).loss_per_turn
    if e > 0
        ## we need to access the pos of the prey which is part of it's state
        prey = edgestates(sim, id, VisiblePrey, Prey)
        newpos = if isnothing(prey)
            nextcellid = rand(edgeids(sim, id, PredatorView))
            agentstate(sim, nextcellid, Cell).pos
        else
            rand(prey).pos
        end
        move!(sim, id, newpos, PredatorPosition(), PredatorView())
        Predator(e, newpos)
    else
        nothing
    end
end;

# The difference in the movement of the prey compared to the movement of
# the predator is that the prey is looking for cells with grass instead of
# prey. In the PPHPC model implemented here, grass is available when
# the `countdown` field of a cell is equal to 0.

function move(state::Prey, id, sim) 
    e = state.energy - param(sim, :prey).loss_per_turn
    if e > 0
        withgrass = filter(edgeids(sim, id, PreyView)) do id
            agentstate(sim, id, Cell).countdown == 0
        end
        nextcellid = if length(withgrass) == 0
            rand(edgeids(sim, id, PreyView))
        else
            rand(withgrass)
        end
        newpos = agentstate(sim, nextcellid, Cell).pos
        move!(sim, id, newpos, PreyPosition(), PreyView())
        Prey(e, newpos)
    else
        nothing
    end
end;

# If a cell has no grass and the countdown field is therefore > 0, the
# countdown is decreased by 1.

function grow_food(state::Cell, _, _)
    Cell(state.pos, max(state.countdown - 1, 0))
end;

# In the try_eat transition function, each cell checks whether predator and
# prey are on the cell at the same time. In this case, the cell
# generates random matches between predator and prey, wherby each prey
# is selected only once at most (only one predator can eat a single
# prey, and each predator eat not more then one prey).

# For these matches, `Die` edges are generated from the cell to the
# prey and `Eat` edges are generated from the cell to the
# predator. Since it is not important for the model at which location
# a prey animal was eaten, the `Die` and `Eat` edgetypes have the
# property `:HasEdgeOnly`.

# If prey on the cell has survived and the cell contains grass
# (countdown == 0), an `Eat` edge is created with one of the surviving
# prey as the target.
    
function try_eat(state::Cell, id, sim)
    predators = edgeids(sim, id, PredatorPosition)
    prey = edgeids(sim, id, PreyPosition)

    ## first the predators eat the prey, in case that both are on the cell
    if ! isnothing(predators) && ! isnothing(prey)
        prey = Set(prey)
        for pred in shuffle(predators)
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
end;

# The reproduction rule for predator and prey is almost the same, so we
# first define a function that can be used for both species. In this function we first
# check if the animal found something to eat. In this case on of the
# previous transition functions did create a edge of typ `Eat` with
# the animal as target, so we can use the `has_edge` function to check
# if this is the case.

# If the energy of the animal is above the threshold given as
# parameter, an offspring is added to the simulation via the
# `add_agent` call and the offspring is also moved it to the same
# position as its parent.

function try_reproduce_imp(state, id, sim, C, posedge, viewedge, species_params)
    if has_edge(sim, id, Eat)
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
end;

# For the Predator we can just call the reproduce function with the necessary
# arguments.

try_reproduce(state::Predator, id, sim) =
    try_reproduce_imp(state, id, sim, Predator,
                    PredatorPosition(), PredatorView(), param(sim, :predator))

# The prey animal needs an extra step because it might have been eaten
# by a predator. So we check if there is a `Die` edge leading to the
# prey animal, and if so, the prey animal is removed from the
# simulation (by returning nothing). Otherwise, we again just call the
# reproduce function defined above.

function try_reproduce(state::Prey, id, sim)
    if has_edge(sim, id, Die)
        return nothing
    end
    try_reproduce_imp(state, id, sim, Prey,
                    PreyPosition(), PreyView(), param(sim, :prey))
end;

# We update the global values and use the `mapreduce` method to count
# the population and the number of cells with food. Based on the
# values, we can then also calculate the mean energy values.

function update_globals(sim)
    push_global!(sim, :predator_pop, mapreduce(sim, _ -> 1, +, Predator; init = 0))
    push_global!(sim, :prey_pop, mapreduce(sim, _ -> 1, +, Prey; init = 0))
    push_global!(sim, :cells_with_food,
                mapreduce(sim, c -> c.countdown == 0, +, Cell))
    push_global!(sim, :mean_predator_energy,
                mapreduce(sim, p -> p.energy, +, Predator; init = 0) /
                    last(get_global(sim, :predator_pop)))
    push_global!(sim, :mean_prey_energy,
                mapreduce(sim, p -> p.energy, +, Prey; init = 0) /
                    last(get_global(sim, :prey_pop)))
end;

# We add to our time series the initialzation values.

update_globals(ppsim);

# And finally we define in which order our transitions functions are called.
# Worth mentioning here are the keyword arguments in `find_prey` and
# `try_reproduce`. In the `find_prey` transition the state of the cell
# itself does not change, it only creates `VisiblePrey` edges. By
# `invariant_compute = true` Vahana knows that the state of the cells
# are constant with respect to this transition function.

# The add_existing keyword in the try_reproduce transition function signify that
# the currently existing position and view edges should not be
# removed, and only additional edges should be added, in our case the
# position of the potential offspring.

function step!(sim)
    apply!(sim, move,
           [ Prey ],
           [ Prey, PreyView, Cell ],
           [ Prey, PreyView, PreyPosition ])

    apply!(sim, find_prey,
           [ Cell ],
           [ PreyPosition, PredatorView ],
           [ VisiblePrey ])
    
    apply!(sim, move,
           [ Predator ],
           [ Predator, PredatorView, Cell, Prey, VisiblePrey ],
           [ Predator, PredatorView, PredatorPosition ])

    apply!(sim, grow_food, Cell, Cell, Cell)
    
    apply!(sim, try_eat,
           [ Cell ],
           [ Cell, PredatorPosition, PreyPosition ],
           [ Cell, Die, Eat ])

    apply!(sim, try_reproduce,
           [ Predator, Prey ],
           [ Predator, Prey, Die, Eat ],
           [ Predator, Prey, PredatorPosition, PreyPosition, PredatorView, PreyView ];
           add_existing = [ PredatorPosition, PreyPosition,
                                       PredatorView, PreyView ])

    update_globals(sim)
end;

# Now we can run the simulation 

for _ in 1:400 step!(ppsim) end

# ## Plots

# To visualize the results we use the Makie package.

# ### Time series

# First we create a linechart for the time series stored in the global state.
# For this purpose Vahana offers the function `plot_globals`. Since
# This function returns not only the Makie Figure itself, but also the
# Axis and Plots, so that the result can be processed further.
# If the default result is displayed, the Julia function `first` can be used
# to extract the mapping from the returned tuple.

plot_globals(ppsim, [ :predator_pop, :prey_pop, :cells_with_food ]) |> first

# ### Spatial distribution

# To visualize the spatial information we can use `heatmap` in
# combination with the function `calc_raster` of Vahana. E.g. the number of
# PreyPosition or PredatorPosition edges connected to an edge give us
# the number of Prey/Predator currently on that cell.

# Note that the function name `num_edges` can be confusing when it
# appears in conjunction with grids. In Vahana, neighbors are always
# neighbors in a graph, so in our specific case, the neighbors of a
# cell in the Predator/PreyPosition network are the Predators/Prey
# that are exactly on the cell, and not in the spatial neighborhood of
# the cell. Only in the case when a network is constructed with the
# function `connect_raster_neighbors!` the neighborhood of the nodes
# in the network coincides with a spatial neighborhood.

# It would be nice to have a colorbar for the heatmaps so we define a small
# helper function that can be used in a pipe.
function add_colorbar(hm)
    Makie.Colorbar(hm.figure[:,2], hm.plot)
    hm
end;

# #### Predators Positions

calc_raster(ppsim, :raster, Int64, [ PredatorPosition ]) do id
    num_edges(ppsim, id, PredatorPosition)
end |> heatmap |> add_colorbar


# #### Prey Positions 

calc_raster(ppsim, :raster, Int64, [ PreyPosition ]) do id
    num_edges(ppsim, id, PreyPosition)
end |> heatmap |> add_colorbar

# #### Cells that contains food

calc_raster(ppsim, :raster, Int64, [ Cell ]) do id
    agentstate(ppsim, id, Cell).countdown == 0
end |> heatmap |> add_colorbar 
