using Pkg

Pkg.add(path = "/home/fuerst/.julia/dev/Vahana")

using Vahana

struct Person
    foo::Int64
end

struct EdgeWithState
    foo::Int64
end

function createAgents(sim::Simulation)
    params(sim).foo # access to parameters

    p = params(sim)
    p.foo

    return (Households = Vector())
    # oder
    return Dict(:Households => Vector())
end

const params = (:foo => 2, :bar => 3.2) # params Union{Tuple, NamedTuple} #Tuple for empty ()

const sim =
    Simulation("foo", params; seed = 123, meta = Dict(:composer => "Steffen")) |>
    add_agenttype!(:Households, Household) |> # can we have stateless agents?
    add_agenttype!(:Persons, Person, Vahana.FixedArray) |>
    add_edgetype!(:BelongsTo, relation = NTo1{ :Persons, :Households } ) |> # default is Stateless
    add_edgetype!(:SomethingWithState, EdgeWithState, AnyToAny) |> #AnyToAny is default
    add_agents!(createAgents) |>
    add_edges!(TODO) |>
    finish_initialization()


struct StatelessEdge end
# noch schöner wäre es, wenn man direkt mit types arbeiten könnte
const sim =
    Simulation("foo", params; seed = 123, meta = Dict(:composer => "Steffen")) |>
    add_agenttype!(Household) |> # can we have stateless agents?
    add_agenttype!(Person, Vahana.FixedArray) |>
    add_edgetype!(StatelessEdge; relation = NTo1{ :Persons, :Households } ) |> # default is Stateless
    add_edgetype!(SomethingWithState) |> #AnyToAny is default
    add_agents!(createAgents) |>
    add_edges!(TODO) |>
    finish_initialization()


struct 
