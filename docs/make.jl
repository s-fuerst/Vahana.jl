using Documenter, Vahana

# module __atexample__named__tutorial1
# struct Buyer end
# struct Seller end
# struct KnownSellers end
# struct Bought end
# end


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
