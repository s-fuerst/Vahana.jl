export set_global!, get_global, push_global!

"""
    get_global(sim::Simulation, name)

Returns the value of the field `name` of the `globals` struct for simulation `sim`.

See also [`create_simulation`](@ref), [`set_global!`](@ref) and [`push_global!`](@ref)
"""
get_global(sim, name) = getfield(sim.globals, name)

"""
    set_global!(sim::Simulation, name, value)

Set the value of the field `name` of the `globals` struct for simulation `sim`.

`set_global!` must not be called within a transition function. 

See also [`create_simulation`](@ref), [`mapreduce`](@ref), [`push_global!`](@ref) and
[`get_global`](@ref)
"""
function set_global!(sim, name, value)
    sim.globals_last_change = sim.num_transitions - 1
    setfield!(sim.globals, name, value)
end

"""
    push_global!(sim::Simulation, name, value)

In the case that a field of the `globals` struct from the Simulation
constructor is a vector (e.g. for time series data), `push_global!` can
be used to add a value to this vector, instead of writing
`set_global!(sim, name, push!(get_global(sim, name), value)`.

`push_global!` must not be called within a transition function. 

See also [`create_model`](@ref), [`mapreduce`](@ref), [`set_global!`](@ref) and
[`get_global`](@ref)
"""
function push_global!(sim, name, value)
    sim.globals_last_change = sim.num_transitions - 1
    push!(getfield(sim.globals, name), value)
end
