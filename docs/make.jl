using Documenter, Vahana

makedocs(sitename="Vahana Documentation",
         format = Documenter.HTML(prettyurls = false),
         pages = [
             "Tutorial" => "tutorial.md",
             "API" => [
                 "Initialization" => "initialization.md",
                 "Transition Function" => "transition.md",
                 "Between transitions" => "between.md",
                 "REPL helpers" => "repl.md"
                 # "All" => "index.md"
             ],
             "Change Log" => "changelog.md"
         ])
