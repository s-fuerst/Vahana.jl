using Pkg

Pkg.add(path = "/home/fuerst/.julia/dev/Vahana")

using Vahana

struct Person
    id::Int64
    foo::Int64
end

# später mit Metaprogramming
@agent Person
    foo::Int64
end

struct EdgeWithState
    foo::Int64
end

p1 = Person(1,2)

function createAgents(sim)
    params(sim).foo # access to parameters

    params = params(sim)
    params.foo

    ## die ganze id geschichte sollte auf Dauer optional per Macro vereinfacht
    ## werden können
    p1 = Person(create_id(Person), 2)

    ## später mit Metaprogramming
    p1 = Person(2)

    # anstatt return auch 
    # symetrisch zur transition function, soll das alles gehen
    ([ [Person(1), Person(2) ],  HH(1) ], edges)
    # oder nur
    self
end

function transition(sim, self, others)
   # das soll alles gehen
    ([ [Person(1), Person(2) ],  HH(1) ], edges)
    # oder nur
    self

end


const params = (:foo => 2, :bar => 3.2) # params Union{Tuple, NamedTuple} #Tuple for empty ()

# Working
const sim = Simulation




struct StatelessEdge end
# noch schöner wäre es, wenn man direkt mit types arbeiten könnte
const sim =
    Simulation("foo", params; seed = 123, meta = Dict(:composer => "Steffen")) |>
    add_agenttype!(Household) |> # can we have stateless agents?
    add_agenttype!(Person, Vahana.FixedArray) |>
    add_edgetype!(StatelessEdge; relation = NTo1{ :Persons, :Households } ) |> # default is Stateless
    add_edgetype!(SomethingWithState) |> #AnyToAny is default
    add_agents!(createAgents) |>
    add_agents!([Person(create_id(Person), 1), Person(2)]) # auch erlauben
    add_edges!(TODO) |>
    finish_initialization()

