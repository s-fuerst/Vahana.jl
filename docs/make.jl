using Documenter, Vahana

import Literate

Literate.markdown(joinpath(@__DIR__, "examples", "tutorial1.jl"), "src"; execute = false)

makedocs(sitename="Vahana Documentation",
         format = Documenter.HTML(prettyurls = false),
         pages = [
             "Tutorial" => "tutorial1.md",
             "API" => [
                 "Types" => "types.md",
                 "Initialization" => "initialization.md",
                 "Transition Function" => "transition.md",
                 "Between transitions" => "between.md",
                 "REPL helpers" => "repl.md"
                 # "All" => "index.md"
             ],
             "Change Log" => "changelog.md"
         ])
