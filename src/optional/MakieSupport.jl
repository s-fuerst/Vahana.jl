using Makie

export plot_globals

"""
    plot_globals(sim, names::Vector{Symbol})

Creates a Makie lineplot with one line for each global in `names`, wherby
those global values must be Vectors (or Iterable).

Returns figure, axis, plots. `plots` is a vector of Plots, which one plot for
element of `names`.

!!! info 

    `plot_globals` is only available when a Makie backend is imported
    by the client.

See also [`push_global!`](@ref), [`get_global`](@ref)
"""
function plot_globals(sim, names::Vector{Symbol})
    f = Figure()
    ax = Axis(f[1,1])
    plots = Dict{Symbol, Lines}()
    for name in names
        vals = get_global(sim, name)
        plots[name] = lines!(ax, 1:length(vals), vals, label = String(name))
    end
    axislegend()
    display(f)
    f, ax, plots
end


