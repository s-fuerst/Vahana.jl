export setglobal!, getglobal, pushglobal!

"""
    getglobal(sim::Simulation, name)

Returns the value of the field `name` of the `globals` struct for simulation `sim`.

See also [`new_simulation`](@ref), [`setglobal!`](@ref) and [`pushglobal!`](@ref)
"""
getglobal(sim, name) = getfield(sim.globals, name)

"""
    setglobal!(sim::Simulation, name, value)

Set the value of the field `name` of the `globals` struct for simulation `sim`.

`setglobal!` must not be called within a transition function. 

See also [`new_simulation`](@ref), [`aggregate`](@ref), [`pushglobal!`](@ref) and
[`getglobal`](@ref)
"""
setglobal!(sim, name, value) = setfield!(sim.globals, name, value)

"""
    pushglobal!(sim::Simulation, name, value)

In the case that a field of the `globals` struct from the Simulation
constructor is a vector (e.g. for time series data), `pushglobal!` can
be used to add a value to this vector, instead of writing
`setglobal!(sim, name, push!(getglobal(sim, name), value)`.

`pushglobal!` must not be called within a transition function. 

See also [`construct_model`](@ref), [`aggregate`](@ref), [`setglobal!`](@ref) and
[`getglobal`](@ref)
"""
pushglobal!(sim, name, value) =
    setfield!(sim.globals, name, push!(getfield(sim.globals, name), value))
