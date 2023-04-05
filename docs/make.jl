using Documenter, Vahana

using GraphMakie, Makie

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
#         format = Documenter.LaTeX(),
         format = Documenter.HTML(prettyurls = false),
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
                 "REPL helpers" => "repl.md",
                 "Plots" => "plots.md",
                 "Configuration" => "config.md"
                 # "All" => "index.md"
             ]
#             "Glossary" => "glossary.md",
#             "Change Log" => "changelog.md"
         ])
