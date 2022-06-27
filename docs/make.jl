using Documenter, Vahana

# this is a really dirty workaround, but for whatever reason the
# sandboxing mechanism of Documenter clashes with the way Vahana
# constructs the model.

# But we do not need the sandboxing, as we can have different models
# in a single julia session.
import Documenter.Utilities.get_sandbox_module!
get_sandbox_module!(_, _, _) = Main

import Literate

Literate.markdown(joinpath(@__DIR__, "examples", "tutorial1.jl"), "src"; execute = false)

#Literate.markdown(joinpath(@__DIR__, "examples", "tutorial2.jl"), "src"; execute = false)

makedocs(sitename="Vahana Documentation",
         format = Documenter.HTML(prettyurls = false),
         pages = [
             "Tutorial1" => "tutorial1.md",
             "Performance Improvments" => "performance.md",
             "API" => [
                 "Types" => "types.md",
                 "Initialization" => "initialization.md",
                 "Transition Function" => "transition.md",
                 "Between transitions" => "between.md",
                 "REPL helpers" => "repl.md",
                 "Miscellaneous" => "misc.md"
                 # "All" => "index.md"
             ],
             "Change Log" => "changelog.md"
         ])
