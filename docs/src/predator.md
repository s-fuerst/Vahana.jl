```@meta
EditURL = "<unknown>/examples/predator.jl"
```

# Predator/Prey Model

This example shows how spatial information can be integrated into a
model. In its current version, Vahana is not the ideal tool for
models that depend mainly on the movement of agents, but as shown
here, it is still possible, even though we are actually only working
with graphs.

The model is based on the [Predator-Prey for High-Performance
Computing](https://peerj.com/articles/cs-36/) (PPHPC) model. In
PPHPC the agents move randomly, in our implementation the prey move
to locations with grass (if one is in sight) and the predators move to
locations with prey to demonstrate how features like this can be
implemented in Vahana.

````@example predator
using Vahana

using Random

Random.seed!(1); #hide
nothing #hide
````

We want to run the model with optimized performance, see
[Performance Tuning](performance.html) for details.

````@example predator
detect_stateless_trait(true)

enable_asserts(false);
nothing #hide
````

## AgentTypes

Beside the predator and the prey we also have the grid cells, which
represent the spatial environment but are also implemented as agents
and thus as nodes of the graph.

````@example predator
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
````

## Edges

All agents have their position as part of their state, but a
cell can only "see" the animals if they are connected to the cell via
an edge. Vahana supports the creation of such edges via the
[`move_to!`](@ref) function.

Prey/PredatorPosition are edges from the prey/predator to the
cell. We use two different types (and not just one type Position)
because this allows the cell to differentiate between prey or
predators by the type of the edges.

````@example predator
struct PreyPosition end

struct PredatorPosition end
````

PreyView and PredatorView are edges from the cells to the prey and
predator respectively. These edges represent the cells that are
visible to a prey or predator so that, for example, it is possible to
check which cell in the visible area contains food.

````@example predator
struct PreyView end

struct PredatorView end
````

VisiblePrey are edges from prey to predator. So far, all edge types
connect animals to cells, and without an incoming edge from a prey,
a predator don't know anything about the prey positions. The
VisiblePrey edges are created by the cells through the `find_prey`
transition function, as the cells know which Prey is directly on the
cell via the PreyPosition edges and therefore in the view of an
predator via the PredatorView edges.

````@example predator
struct VisiblePrey end
````

The last two edge types are messages to inform the agents that they
must die or found something to eat.

````@example predator
struct Die end

struct Eat end
````

## Params

We follow the parameter structure from the PPHPC model, but use the
same parameters for Prey and Predator for the example run.

````@example predator
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
end;
nothing #hide
````

## Globals

We are creating timeseries (in form of `Vector`s) for the predator and
prey population, the number of cells with food and the average
energy of the predators and prey.

````@example predator
Base.@kwdef mutable struct PPGlobals
    predator_pop = Vector{Int64}()
    prey_pop = Vector{Int64}()
    cells_with_food = Vector{Int64}()
    mean_predator_energy = Vector{Float64}()
    mean_prey_energy = Vector{Float64}()
end
````

## Create the Simulation

We have now defined all the Julia structs needed to create the model
and a simulation. We also add some traits to some edgetypes, mainly
for performance reasons.

````@example predator
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
````

## Initialization

First we add the Cells to the Simulation. Therefore we define a
constructor functions for the cells. There is a 50% probability that a cell
contains food (and in this case `countdown` is 0).

````@example predator
init_cell(pos::CartesianIndex) =
    Cell(pos, rand() < 0.5 ? 0 : rand(1:param(ppsim, :restart)))

add_raster!(ppsim, :raster, param(ppsim, :raster_size), init_cell)
````

Predator and Pray are starting on a random position.

````@example predator
function random_pos(sim)
    size = param(sim, :raster_size)
    CartesianIndex(rand(1:size[1]), rand(1:size[2]))
end
````

We define two helper functions to move an animal to a new
position. This will add a single (pos)edge from the animal to the
cell at the `newpos` position, and for all cells in the manhatten
distance of 1 also (view)edges from those cell to the animal and
from the animal to the cell.

````@example predator
function move!(sim, id, newpos, posedge, viewedge)
    move_to!(sim, :raster, id, newpos, nothing, posedge)
    move_to!(sim, :raster, id, newpos, viewedge, viewedge;
             distance = 1, metric = :manhatten)
end
````

Now we can add the Predator and Pray to the simulation, and use
the `move!` function to create also the position and view edges.

````@example predator
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
````

At the end of the initialization phase we have to call finish_init!

````@example predator
finish_init!(ppsim)
````

## Transition Functions

As mentioned in the comment for the PredatorView, the cells are
responsible to connect the Prey with the Predators. So each cell
iterate over each prey on the cell and add edges to all predators
the can view the cell.

````@example predator
function find_prey(state::Cell, id, sim)
    checked(foreach, neighborids(sim, id, PreyPosition)) do preyid
        checked(foreach, neighborids(sim, id, PredatorView)) do predid
            add_edge!(sim, preyid, predid, VisiblePrey())
        end
    end
    state
end
````

If a predator has enough energy left to move and there is a prey in the
predator's field of view, a random prey is selected and its position
is used as the new position. If no prey is visible, a random cell in
the predator's field of view is selected as the new position.

If there is not enough energy left, the transition function returns `nothing`,
which means that the predator dies and is no longer part of the graph.

````@example predator
function move(state::Predator, id, sim)
    e = state.energy - param(sim, :predator).loss_per_turn
    if e > 0
        # we need to access the pos of the prey which is part of it's state
        prey = neighborstates(sim, id, VisiblePrey, Prey)
        newpos = if isnothing(prey)
            nextcellid = rand(neighborids(sim, id, PredatorView))
            agentstate(sim, nextcellid, Cell).pos
        else
            rand(prey).pos
        end
        move!(sim, id, newpos, PredatorPosition(), PredatorView())
        Predator(e, newpos)
    else
        nothing
    end
end
````

The difference in the movement of the prey compared to the movement of
the predator is that the prey is looking for cells with grass instead of
prey. In the PPHPC model implemented here, grass is available when
the `countdown` field of a cell is equal to 0.

````@example predator
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
        move!(sim, id, newpos, PreyPosition(), PreyView())
        Prey(e, newpos)
    else
        nothing
    end
end
````

If a cell has no grass and the countdown field is therefore > 0, the
countdown is decreased by 1.

````@example predator
function grow_food(state::Cell, _, _)
    Cell(state.pos, max(state.countdown - 1, 0))
end
````

In the try_eat transition function, each cell checks whether predator and
prey are on the cell at the same time. In this case, the cell
generates random matches between predator and prey, wherby each prey
is selected only once at most (only one predator can eat a single
prey, and each predator eat not more then one prey).

For these matches, `Die` edges are generated from the cell to the
prey and `Eat` edges are generated from the cell to the
predator. Since it is not important for the model at which location
a prey animal was eaten, the `Die` and `Eat` edgetypes have the
property `:HasEdgeOnly`.

If prey on the cell has survived and the cell contains grass
(countdown == 0), an `Eat` edge is created with one of the surviving
prey as the target.

````@example predator
function try_eat(state::Cell, id, sim)
    predators = neighborids(sim, id, PredatorPosition)
    prey = neighborids(sim, id, PreyPosition)

    # first the predators eat the prey, in case that both are on the cell
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

    # then check if there is prey left that can eat the grass
    if  ! isnothing(prey) && length(prey) > 0 && state.countdown == 0
        add_edge!(sim, id, rand(prey), Eat())
        Cell(state.pos, param(sim, :restart))
    else
        state
    end
end
````

The reproduction rule for predator and prey is almost the same, so we
first define a function that can be used for both species.

````@example predator
function try_reproduce_imp(state, id, sim, C, posedge, viewedge, species_params)
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
                    PredatorPosition(), PredatorView(), param(sim, :predator))

function try_reproduce(state::Prey, id, sim)
    if has_neighbor(sim, id, Die)
        return nothing
    end
    try_reproduce_imp(state, id, sim, Prey,
                    PreyPosition(), PreyView(), param(sim, :prey))
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
                      [ PreyView, PreyPosition ])

    apply_transition!(sim, find_prey,
                      [ Cell ],
                      [ PreyPosition, PredatorView ],
                      [ VisiblePrey ])

    apply_transition!(sim, move,
                      [ Predator ],
                      [ PredatorView, VisiblePrey ],
                      [ PredatorView, PredatorPosition ])

    apply_transition!(sim, grow_food, [ Cell ], [], [])

    apply_transition!(sim, try_eat,
                      [ Cell ],
                      [ PredatorPosition, PreyPosition ],
                      [ Die, Eat ])

    apply_transition!(sim, try_reproduce,
                      [ Predator, Prey ],
                      [ Die, Eat ],
                      [ PredatorPosition, PreyPosition,
                        PredatorView, PreyView ];
                      add_existing = [ PredatorPosition, PreyPosition,
                                       PredatorView, PreyView ])

    update_globals(sim)
end

@time for _ in 1:400 step!(ppsim) end
````

## Plots

To visualize the results we use the Makie package.

````@example predator
using CairoMakie
using Makie
````

### Time series

First we create a linechart for the time series stored in the global state.
For this purpose Vahana offers the function `plotglobals`. Since
This function returns not only the Makie Figure itself, but also the
Axis and Plots, so that the result can be processed further.
If the default result is displayed, the Julia function `first` can be used
to extract the mapping from the returned tuple.

````@example predator
plotglobals(ppsim, [ :predator_pop, :prey_pop, :cells_with_food ]) |> first
````

### Spatial distribution

To visualize the spatial information we can use `heatmap` in
combination with the function `calc_raster` of Vahana. E.g. the number of
PreyPosition or PredatorPosition edges connected to an edge give us
the number of Prey/Predator currently on that cell.

Note that the function name `num_neighbors` can be confusing when it
appears in conjunction with grids. In Vahana, neighbors are always
neighbors in a graph, so in our specific case, the neighbors of a
cell in the Predator/PreyPosition network are the Predators/Prey
that are exactly on the cell, and not in the spatial neighborhood of
the cell. Only in the case when a network is constructed with the
function `connect_raster_neighbors!` the neighborhood of the nodes
in the network coincides with a spatial neighborhood.

It would be nice to have a colorbar for the heatmaps so we define a small
helper function that can be used in a pipe

````@example predator
function add_colorbar(hm)
    Colorbar(hm.figure[:,2], hm.plot)
    hm
end
````

First we visualize the position of the Predators

````@example predator
calc_raster(ppsim, :raster) do id
    num_neighbors(ppsim, id, PredatorPosition)
end |> heatmap |> add_colorbar
````

We do this now to visualise the position of the Prey

````@example predator
calc_raster(ppsim, :raster) do id
    num_neighbors(ppsim, id, PreyPosition)
end |> heatmap |> add_colorbar
````

And can also show easily which cells contains food

````@example predator
calc_raster(ppsim, :raster) do id
    agentstate(ppsim, id, Cell).countdown != 0
end |> heatmap |> add_colorbar
````

# Tests

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

---

*This page was generated using [Literate.jl](https://github.com/fredrikekre/Literate.jl).*

