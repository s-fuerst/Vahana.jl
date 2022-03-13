export GlobalState
export current_state, all_states, add_globalstate!

abstract type GlobalState end

abstract type Globals{T} end

"""
    current_state(sim, ::Type{T})

TODO DOC
"""
current_state(sim, ::Type{T}) where {T<:GlobalState} =
    current_state(sim.globals[T])

"""
    all_states(sim, ::Type{T})

TODO DOC
"""
all_states(sim, ::Type{T}) where {T<:GlobalState} =
    all_states(sim.globals[T])

"""
    add_globalstate!(sim, value::T) where {T <: GlobalState}

TODO DOC
"""
function add_globalstate!(sim, value::T) where {T <: GlobalState}
    add_globalstate!(sim, sim.globals[T], value)
end

######################################## EmptyGlobal

struct EmptyGlobal{U, T} <: Globals{T} end

current_state(g::EmptyGlobal) = g

all_states(g::EmptyGlobal) = g

function add_globalstate!(sim,
               ::EmptyGlobal{U, T},
               value::T) where {U, T <: GlobalState}
    first_value(sim, U, value)
end

######################################## GlobalSingle

struct GlobalSingle{T} <: Globals{T}
    state::T
end

function first_value(sim, ::Type{GlobalSingle}, value::T) where {T <: GlobalState}
    push!(sim.globals, T => GlobalSingle{T}(value))
end

current_state(g::GlobalSingle) = g.state

all_states(g::GlobalSingle) = g.state

function add_globalstate!(sim, ::GlobalSingle, value::T) where {T <: GlobalState}
    sim.globals[T] = GlobalSingle(value)
end

######################################## GlobalSeries

struct GlobalSeries{T} <: Globals{T}
    values::Vector{T}
end

function first_value(sim, ::Type{GlobalSeries}, value::T) where {T <: GlobalState}
    push!(sim.globals, T => GlobalSeries{T}(fill(value, 1)))
end

current_state(g::GlobalSeries) = g.values |> last

all_states(g::GlobalSeries) = g.values

function add_globalstate!(_, g::GlobalSeries, value::T) where {T <: GlobalState}
    push!(g.values, value)
end

