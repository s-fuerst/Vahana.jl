export set_global!, get_global, push_global!, modify_global!

"""
    get_global(sim::Simulation, name)

Returns the value of the field `name` of the `globals` struct for simulation `sim`.

See also [`create_simulation`](@ref), [`set_global!`](@ref) and [`push_global!`](@ref)
"""
get_global(sim, name) = getfield(sim.globals, name)

"""
    set_global!(sim::Simulation, name, value)

Set the value of the field `name` of the `globals` struct for simulation `sim`.

In parallel simulations, `set_global!` must be called on all
processes, and with identical `value` across all processes.

`set_global!` must not be called within a transition function. 

See also [`create_simulation`](@ref), [`mapreduce`](@ref),
[`modify_global!`](@ref) [`push_global!`](@ref) and
[`get_global`](@ref)
"""
function set_global!(sim, name, value)
    sim.globals_last_change = sim.num_transitions - 1
    setfield!(sim.globals, name, value)
end


"""
    modify_global!(sim::Simulation, name, f)

Modify the value of the `name` field of the `globals` structure for the
simulation `sim` using the `f` function, which receives the current value as an
argument.

This is a combination of [`set_global!`](@ref) and [`get_global`](@ref):
`set_global!(sim, name, f(get_global(sim, name)))`.

`modify_global!` must not be called within a transition function.

In parallel simulations, `push_global!` must be called on all
processes, and with identical `value` across all processes.

See also [`create_simulation`](@ref), [`mapreduce`](@ref),
[`set_global!`](@ref) [`push_global!`](@ref) and
[`get_global`](@ref)
"""
function modify_global!(sim, name, f)
    set_global!(sim, name, f(get_global(sim, name)))
end

"""
    push_global!(sim::Simulation, name, value)

In the case that a field of the `globals` struct from the Simulation
constructor is a vector (e.g. for time series data), `push_global!` can
be used to add a value to this vector, instead of writing
`set_global!(sim, name, push!(get_global(sim, name), value)`.

In parallel simulations, `push_global!` must be called on all
processes, and with identical `value` across all processes.

`push_global!` must not be called within a transition function. 

See also [`create_model`](@ref), [`mapreduce`](@ref), [`set_global!`](@ref) and
[`get_global`](@ref)
"""
function push_global!(sim, name, value)
    sim.globals_last_change = sim.num_transitions - 1
    push!(getfield(sim.globals, name), value)
end
