export AbstractGlobal
export current_state, all_states, add_globalstate!

abstract type AbstractGlobal end

abstract type Globals{T} end

"""
    current_state(sim, ::Type{T})

TODO DOC
"""
current_state(sim, ::Type{T}) where {T<:AbstractGlobal} =
    current_state(sim.globals[T])

"""
    all_states(sim, ::Type{T})

TODO DOC
"""
all_states(sim, ::Type{T}) where {T<:AbstractGlobal} =
    all_states(sim.globals[T])

"""
    add_globalstate!(sim, value::T) where {T <: AbstractGlobal}

TODO DOC
"""
function add_globalstate!(sim, value::T) where {T <: AbstractGlobal}
    add_globalstate!(sim, sim.globals[T], value)
end

######################################## EmptyGlobal

struct EmptyGlobal{U, T} <: Globals{T} end

current_state(g::EmptyGlobal) = g

all_states(g::EmptyGlobal) = g

function add_globalstate!(sim,
               ::EmptyGlobal{U, T},
               value::T) where {U, T <: AbstractGlobal}
    first_value(sim, U, value)
end

######################################## GlobalState

struct GlobalState{T} <: Globals{T}
    state::T
end

function first_value(sim, ::Type{GlobalState}, value::T) where {T <: AbstractGlobal}
    push!(sim.globals, T => GlobalState{T}(value))
end

current_state(g::GlobalState) = g.state

all_states(g::GlobalState) = g.state

function add_globalstate!(sim, ::GlobalState, value::T) where {T <: AbstractGlobal}
    sim.globals[T] = GlobalState(value)
end

######################################## GlobalSeries

struct GlobalSeries{T} <: Globals{T}
    values::Vector{T}
end

function first_value(sim, ::Type{GlobalSeries}, value::T) where {T <: AbstractGlobal}
    push!(sim.globals, T => GlobalSeries{T}(fill(value, 1)))
end

current_state(g::GlobalSeries) = g.values |> last

all_states(g::GlobalSeries) = g.values

function add_globalstate!(_, g::GlobalSeries, value::T) where {T <: AbstractGlobal}
    push!(g.values, value)
end

