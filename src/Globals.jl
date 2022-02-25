export AbstractGlobal
export current_state, all_states, push_global!
export aggregate!
abstract type AbstractGlobal end

abstract type Globals{ T <: AbstractGlobal } end

current_state(sim, T::DataType) = current_state(sim.globals[T])

all_states(sim, T::DataType) = all_states(sim.globals[T])

function push_global!(sim, value::T) where { T <: AbstractGlobal }
    push_global!(sim, sim.globals[T], value)
end

######################################## GlobalState

struct GlobalState{T} <: Globals{T}
    state::T
end

current_state(g::GlobalState) = g.state

all_states(g::GlobalState) = g.state

function push_global!(sim, ::GlobalState, value::T) where { T <: AbstractGlobal }
    sim.globals[T] = GlobalState(value)
end

######################################## GlobalSeries

struct GlobalSeries{T} <: Globals{T}
    values::Vector{T}
end

current_state(g::GlobalSeries) = g.values |> last

all_states(g::GlobalSeries) = g.values

function push_global!(_, g::GlobalSeries, value::T) where { T <: AbstractGlobal }
    push!(g.values, value)
end

