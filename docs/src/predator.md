```@meta
EditURL = "<unknown>/examples/predator.jl"
```

# Predator/Prey Model

This example shows how spatial information can be integrated into a
model. In its current version, Vahana is not the ideal tool for models
that depend mainly on the movement of agents, as in this example, but
as shown here, it is still possible, even though we are actually
only working with graphs.

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

enable_asserts(false)
````

## AgentTypes

We have three types of agents, besides the predator and the prey we
also have the grid cells, which represent the spatial environment
but are also implemented as agents and thus as nodes of the graph in
Vahana.

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

All agents have their position as part of their state, but e.g. the
cell can only "see" the animals if they are connected to the cell via
an edge. Vahana supports the creation of such edges via the
[`move_to!`](@ref) function.

Prey/PredatorPosition are edges from the prey/predator to the
cell. We use two different types (and not just one type Position)
because this allows the cell to access only the prey or predator on
the cell.

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

struct VisiblePrey end

struct Die end

struct Eat end
````

## Params

````@example predator
Base.@kwdef struct SpeciesParams
    gain_from_food::Int64
    loss_per_turn::Int64
    repro_thres::Int64
    repro_prob::Int64
end

Base.@kwdef struct AllParams
    raster_size::Tuple{Int64, Int64}
    restart::Int64
    predator::SpeciesParams
    prey::SpeciesParams
    num_predators::Int64
    num_prey::Int64
end

predator_params = SpeciesParams(
    gain_from_food = 10,
    loss_per_turn = 1,
    repro_thres = 5,
    repro_prob = 50
)

prey_params = SpeciesParams(
    gain_from_food = 10,
    loss_per_turn = 1,
    repro_thres = 5,
    repro_prob = 50
)

ggparams = AllParams(
    raster_size = (100, 100),
    restart = 5,
    predator = predator_params,
    prey = prey_params,
    num_predators = 500,
    num_prey = 2000,
)
````

## Globals

````@example predator
Base.@kwdef mutable struct PPGlobals
    predator_pop = Vector{Int64}()
    prey_pop = Vector{Int64}()
    cells_with_food = Vector{Int64}()
    mean_predator_energy = Vector{Float64}()
    mean_prey_energy = Vector{Float64}()
end
````

## Create Simulation

````@example predator
const ppmodel = ModelTypes() |>
    register_agenttype!(Predator) |>
    register_agenttype!(Prey) |>
    register_agenttype!(Cell, :Vector; size = ggparams.raster_size[1] * ggparams.raster_size[2]) |>
    register_edgetype!(PredatorPosition, :SingleAgentType; to_agenttype = Cell) |>
    register_edgetype!(PreyPosition, :SingleAgentType; to_agenttype = Cell) |>
    register_edgetype!(PredatorView) |>
    register_edgetype!(PreyView, :SingleAgentType; to_agenttype = Prey) |>
    register_edgetype!(VisiblePrey) |>
    register_edgetype!(Die, :HasEdgeOnly) |>
    register_edgetype!(Eat, :HasEdgeOnly) |>
    construct_model("Predator Prey")
````

## Initialization

````@example predator
function init_cell(pos::CartesianIndex)
    if rand() < 0.5
        Cell(pos, 0)
    else
        Cell(pos, ceil(rand() * param(ppsim, :restart)) |> Int64)
    end
end

function random_pos(sim)
    size = param(sim, :raster_size)
    CartesianIndex(ceil(rand() * size[1]) |> Int64,
                   ceil(rand() * size[2]) |> Int64)
end

function move_predator!(sim, id, newpos, withview)
    move_to!(sim, :raster, id, newpos, nothing, PredatorPosition())
    if withview
        move_to!(sim, :raster, id, newpos, PredatorView(), PredatorView();
                 distance = 1, metric = :manhatten)
    end
end

function move_prey!(sim, id, newpos, withview)
    move_to!(sim, :raster, id, newpos, nothing, PreyPosition())
    if withview
        move_to!(sim, :raster, id, newpos, PreyView(), nothing;
                 distance = 1, metric = :manhatten)
    end
end


const ppsim = new_simulation(ppmodel, ggparams, PPGlobals())

add_raster!(ppsim, :raster, param(ppsim, :raster_size), init_cell)

foreach(1:param(ppsim, :num_prey)) do _
    energy = ceil(rand() * 2 * param(ppsim, :prey).gain_from_food) |> Int64
    pos = random_pos(ppsim)
    id = add_agent!(ppsim, Prey(energy, pos))
    move_prey!(ppsim, id, pos, true)
end

foreach(1:param(ppsim, :num_predators)) do _
    energy = ceil(rand() * 2 * param(ppsim, :predator).gain_from_food) |> Int64
    pos = random_pos(ppsim)
    id = add_agent!(ppsim, Predator(energy, pos))
    move_predator!(ppsim, id, pos, true)
end

finish_init!(ppsim)
````

## Transition Functions

````@example predator
function find_prey(state::Cell, id, sim)
    prey = neighborids(sim, id, PreyPosition)
    if isnothing(prey)
        return state
    end

    checked(foreach, neighborids(sim, id, PredatorView)) do predid
        add_edge!(sim, rand(prey), predid, VisiblePrey())
    end
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
        move_predator!(sim, id, newpos, false)
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
        move_prey!(sim, id, newpos, false)
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

    # first the predators eat the prey, in case that both are on the cell
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

    # then check if there is prey left that can eat the grass
    if  ! isnothing(prey) && length(prey) > 0 && state.countdown == 0
        add_edge!(sim, id, rand(prey), Eat())
        Cell(state.pos, param(sim, :restart))
    else
        state
    end
end

function try_reproduce_imp(state, id, sim, C, move_fn, species_params)
    move_fn(sim, id, state.pos, true)
    if has_neighbor(sim, id, Eat)
        state = C(state.energy + species_params.gain_from_food, state.pos)
    end
    if state.energy > species_params.repro_thres &&
        rand() * 100 < species_params.repro_prob

        energy_offspring = Int64(round(state.energy / 2))
        newid = add_agent!(sim, C(energy_offspring, state.pos))
        move_fn(sim, newid, state.pos, true)
        C(state.energy - energy_offspring, state.pos)
    else
        state
    end
end

try_reproduce(state::Predator, id, sim) =
    try_reproduce_imp(state, id, sim, Predator,
                    move_predator!, param(sim, :predator))

function try_reproduce(state::Prey, id, sim)
    if has_neighbor(sim, id, Die)
        return nothing
    end
    try_reproduce_imp(state, id, sim, Prey, move_prey!, param(sim, :prey))
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

using Plots

# t is PreyPosition or PredatorPosition
function plot_agents_on_cell(sim, t)
    calc_raster(sim, :raster) do id
        num_neighbors(sim, id, t)
    end |> heatmap
end

for _ in 1:200 step!(ppsim) end

plotglobals(ppsim, [ :predator_pop, :prey_pop, :cells_with_food ])

plot!()
````

# Tests

````@example predator
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

        # prey1 should be removed in move
        prey1 = add_agent!(test, Prey(1, CartesianIndex(2,2)))
        move_prey!(test, prey1, CartesianIndex(2,2), true)
        # prey2 should move to Pos(5,5) and Eat (and reproduce)
        prey2 = add_agent!(test, Prey(5, CartesianIndex(5,6)))
        move_prey!(test, prey2, CartesianIndex(5,6), true)
        # prey3 should not find anything to Eat, but still reproduce
        prey3 = add_agent!(test, Prey(9, CartesianIndex(7,7)))
        move_prey!(test, prey3, CartesianIndex(7,7), true)
        # prey4 shoud be eaten by pred2
        prey4 = add_agent!(test, Prey(5, CartesianIndex(12,12)))
        move_prey!(test, prey4, CartesianIndex(12,12), true)
        # pred1 should be removed in move
        pred1 = add_agent!(test, Predator(1, CartesianIndex(17, 17)))
        move_predator!(test, pred1, CartesianIndex(17, 17), true)
        # pred2 should be move to pos(12,12) and eat prey4
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

        # prey and predator removed?
        @test num_agents(test, Predator) == 1
        @test num_agents(test, Prey) == 3

        # energy reduced by one
        @test agentstate(test, prey2, Prey).energy == 4

        # prey2 moved to (5,5) as there is something to eat
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

        # two offsprings, one was eaten: 3 + 2 - 1
        @test num_agents(test, Prey) == 4
        @test agentstate(test, prey2, Prey).energy == 7
        @test agentstate(test, prey3, Prey).energy == 4
    end
end
````

---

*This page was generated using [Literate.jl](https://github.com/fredrikekre/Literate.jl).*

