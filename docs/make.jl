using Documenter, Vahana

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

#Literate.markdown(joinpath(@__DIR__, "examples", "tutorial2.jl"), "src"; execute = false)

makedocs(sitename="Vahana Documentation",
#         format = Documenter.LaTeX(),
         format = Documenter.HTML(prettyurls = false),
         pages = [
             "Introduction" => "index.md",
             "Tutorial" => "tutorial1.md",
             "Performance Tuning" => "performance.md",
             "API" => [
                 "Model Definition" => "definition.md",
                 "Initialization" => "initialization.md",
                 "Transition Function" => "transition.md",
                 "Global Layer" => "between.md",
                 "REPL helpers" => "repl.md",
                 "Configuration" => "config.md"
                 # "All" => "index.md"
             ],
             "Change Log" => "changelog.md"
         ])
