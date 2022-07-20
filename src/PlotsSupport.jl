using Plots

export plotglobals

"""
TODO DOC 
"""
function plotglobals(sim, names::Vector{Symbol})
    plot()
    for name in names
        f = getglobal(sim, name)
        plot!(1:length(f), f, label = String(name))
    end
end

