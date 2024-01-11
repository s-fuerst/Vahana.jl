using Documenter, Vahana

using GraphMakie, Makie, DataFrames

# this is a really dirty workaround, but for whatever reason the
# sandboxing mechanism of Documenter clashes with the way Vahana
# constructs the model.

# But we do not need the sandboxing, as we can have different models
# in a single julia session.
import Documenter.Utilities.get_sandbox_module!
get_sandbox_module!(_, _, _) = Main

import Literate

cd(@__DIR__)

Literate.markdown(joinpath(@__DIR__, "examples", "tutorial1.jl"), "src"; execute = false)

Literate.markdown(joinpath(@__DIR__, "examples", "predator.jl"), "src"; execute = false)

Literate.markdown(joinpath(@__DIR__, "examples", "hegselmann.jl"), "src"; execute = false)

makedocs(sitename="Vahana Documentation",
         modules = [Vahana],
#         format = Documenter.LaTeX(),
         format = Documenter.HTML(prettyurls = false,
                                  edit_link = :commit),
         clean = false, 
         pages = [
             "Introduction" => "index.md",
             "Tutorials" => [
                 "Tutorial" => "tutorial1.md",
                 "Opinion Model" => "hegselmann.md",
                 "Predator / Prey" => "predator.md"
             ],
             "Performance Tuning" => "performance.md",
             "API" => [
                 "Model Definition" => "definition.md",
                 "Initialization" => "initialization.md",
                 "Transition Function" => "transition.md",
                 "Global Layer" => "global.md",
#                 "REPL helpers" => "repl.md",
                 "Plots" => "plots.md",
                 "HDF5" => "hdf5.md",
                 "Logging" => "logging.md",
                 "Configuration" => "config.md",
                 "Misc" => "misc.md"
                 # "All" => "index.md"
             ]
#             "Glossary" => "glossary.md",
#             "Change Log" => "changelog.md"
         ])

deploydocs(
    repo = "github.com/s-fuerst/Vahana.jl.git",
    devbranch = "main"
)
