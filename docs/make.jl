using Documenter, Vahana

using Pkg
Pkg.status("Vahana")

using GraphMakie, Makie, DataFrames

# this is a really dirty workaround, but for whatever reason the
# sandboxing mechanism of Documenter clashes with the way Vahana
# constructs the model.

# But we do not need the sandboxing, as we can have different models
# in a single julia session.
import Documenter.get_sandbox_module!
get_sandbox_module!(_, _, _) = Main

import Literate

cd(@__DIR__)

Literate.markdown(joinpath(@__DIR__, "examples", "tutorial1.jl"), "src"; execute = false)

Literate.markdown(joinpath(@__DIR__, "examples", "predator.jl"), "src"; execute = false)

Literate.markdown(joinpath(@__DIR__, "examples", "hegselmann.jl"), "src"; execute = false)

###
makedocs(sitename="Vahana Documentation",
         modules = [Vahana],
#         format = Documenter.LaTeX(),
         format = Documenter.HTML(prettyurls = false,
                                  edit_link = :commit),
         clean = false, 
         pages = [
             "Introduction" => "index.md",
             "Tutorials" => [
                 "First Steps" => "tutorial1.md",
                 "Utilizing Graphs.jl" => "hegselmann.md",
                 "Adding Spatial Information" => "predator.md"
             ],
             "Performance Tuning" => "performance.md",
             "Parallel Simulations" => "parallel.md", 
             "API" => [
                 "Model Definition" => "definition.md",
                 "Initialization" => "initialization.md",
                 "Transition Function" => "transition.md",
                 "Global Layer" => "global.md",
                 "Raster" => "raster.md",
#                 "REPL helpers" => "repl.md",
                 "Plots" => "plots.md",
                 "File storage" => "hdf5.md",
                 "Logging" => "logging.md",
                 "Configuration" => "config.md",
                 "Misc" => "misc.md",
                 "Index" => "apiindex.md"
             ],
#             "Glossary" => "glossary.md",
             "Change Log" => "changelog.md"
         ])

deploydocs(
    repo = "github.com/s-fuerst/Vahana.jl.git",
    devbranch = "main"
)
