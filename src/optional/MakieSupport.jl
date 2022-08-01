println("Including MakieSupport.jl")

using Makie

export plotglobals

"""
TODO DOC 
"""
function plotglobals(sim, names::Vector{Symbol})
    f = Figure()
    ax = Axis(f[1,1])
    plots = Dict{Symbol, Lines}()
    for name in names
        vals = getglobal(sim, name)
        plots[name] = lines!(ax, 1:length(vals), vals, label = String(name))
    end
    axislegend()
    f, ax, plots
end


