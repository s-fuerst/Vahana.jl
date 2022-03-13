# Tutorial

## Model Background

We have a simple but volatile market with $n$ buyers, $m$ sellers, and
two goods called $x$ and $y$. Each seller sells both commodities,
buyers buy both commodities from a single seller, but at each step the
buyer randomly selects the seller from a fixed subset of all
sellers. We are only interested in relative prices, so the price for
good $x$ is set to 1, the price for good $y$ is $p$.

At each step, each buyer has a constant fixed budget $B$ for buying the
commodities. The quantities are choosen using a Cobb-Douglas
utility function:

```math
\max_{x,y} && u(x, y) = x^\alpha \cdot y^{1 - \alpha} \\
\textrm{s.t.:} && x + y \cdot p \leq B \nonumber
```

The solution of this optimization is:

```math
x &=& B \cdot \alpha \label{eqn:demand} \\
y &=& \frac{B \cdot (1 - \alpha)}{p} \nonumber
```

For illustrative reasons, we neglect production except for the
assumption that $x$ and $y$ are joint products, so that the seller
tries to find a price where $x =y$. In the case of a single buyer,
the solution is $p = \frac{1 - \alpha}{\alpha}$.  But in our case,
the sellers are facing different buyers with different preferences
$\alpha$ and a different budget $B$. So they start with a random
price and adjust that price at each time step:

```math
&&p_t = \frac{d_y}{d_x} \cdot p_{t-1} \label{eqn:price} \\
\textrm{where }&& d_x = \sum_{b \in \textrm{buyers}}x_b ,\quad d_y = \sum_{b \in \textrm{buyers}} y_b  \nonumber 
```

It's easy to see that in the single buyer case, this will lead to $p =
\frac{1 - \alpha}{\alpha}$ after a single step.


## Goal

In this tutorial, we will build a model with multiple buyers and
sellers, whereby the buyers will always randomly choice a seller from
a fixed subset of the sellers. This subset is of course different for
each buyer.


## Initialization

In addition to the Vahana package, we also need the Random package to
initialize the buyer with different random values for B and $\alpha$
and to select a seller from the list of known sellers.

    using Vahana
    using Random

We have two types of agents, `Buyer` and `Seller`. In Vahana, agents
are defined as structs, which are subtypes of `Agent`. The
structs define the state of the various agent types. All the types
defined for the Model must be "plain data" types. Such a type is
call isbitstype in Julia, and must fulfill the following contrains: A
bitstype is immutable and contains no references to other values, only
primitive types and other isbitstype types.

Since we want to calculate the average price of $y$ later, the seller
needs to store the quantity of the sold good $y$. So we have:

    struct Buyer <: Agent
        α::Float64
        B::Float64
    end
    
    struct Seller <: Agent
        p::Float64
        d_y::Float64 # the sum of bought goods y (for a single time step)
    end

We also write constructors for these agents. For the Buyer we want for
$\alpha$ a random value between zero and one, and for B a random
integer betwenn 1 and 100. The initial price $p$ of a seller should be
between 0.5 and 1.5.

    Buyer() = Buyer(rand(), rand((1:100)))
    
    Seller() = Seller(rand() + 0.5, 0)

And we have also
two types of networks. The first is fixed and describes the sellers known to
a single buyer. Since the direction of an edge determines the flow of
information and the buyer needs the price information to calculate his
demand, it is a network from sellers to buyers. We call this network
`KnownSellers`.

As for the agents we must define a struct for each type of network.
The edges of th `KnownSellers` network do not carry additional
information, so we define them as a concrete subtype of `EdgeState`
without any field.

    struct KnownSellers <: EdgeState end

The second network \texttt{BOUGHT} will be constructed in the
transition function called `calc_demand` which will be the
implementation of equation (\ref{eqn:demand}). This network describes
from which seller a buyer bought something, so it edge will go from
the buyers to the sellers. And those edges come with additional state,
which provides the quantity of the bought goods:

    struct Bought <: EdgeState
        x::Float64
        y::Float64
    end

We need sometimes to sum those quantities, so we define a + operator
for the this struct:

    import Base.+
    +(a::Bought, b::Bought) = Bought(a.x + b.x, a.y + b.y)

As already mentioned we want to track the development of the average
price. Additional we are also interessted for the excess demand. So
again we create two structs, this time those structs are a subtype of
`GlobalState`.
\comment{Diese Subtype ist zumindest bei den Edges und Globals eigentlich von der Implementierung garnicht notwendig, haben aber bessere Fehlermeldungen zur Folge, falls bei der Modellimplementierung etwas falsch gemacht wird (da schon direkt beim Funktionsaufruf der Fehler durch den Typ festgestellt wird). Ich denke daher, dass die minimal größere Komplexität hier es den Anwender*innen insgesamt trotzdem leichter macht.}

We need something that give us time series for the variables we want
to observe. In our case, we want to know the trajectories of the price
and demands. 

    struct ExcessDemand <: GlobalState
        x_minus_y::Float64
    end
    
    struct AveragePrice <: GlobalState
        p::Float64
    end

We now have all the elements needed to initialize a simulation. So the
first thing we need to do is create a `Simulation` object. The the
`Simulation` constructor needs two parameters, first the name of the
simulation and second the parameters for the simulation in the form of
a named tuple. (Hint: If a NamedTupe has only one element, it must
have a ',' after the value, e.g. Simulation("Name", (steps = 100,))

    sim = Simulation("Excess Demand",
    		 (numBuyer = 50, numSeller = 5, knownSellers = 2))

    Simulation Name: Excess Demand
    Parameters: (numBuyer = 50, numSeller = 5, knownSellers = 2)

Then we must register all of our types. For the Global types we have
two options. When `add_globalstatetype!` is used, only a single
instance of that type is stored in the simulation. This is often
useful for a global state that is used by the simulation itself,
e.g. the GDP of an economy.
In our case we want to see after the simulation has finished the
complete time series for the Global types, so we use the
`add_globalseriestype!` function. So for all types together we have:

    add_agenttype!(sim, Buyer)
    add_agenttype!(sim, Seller)
    add_edgetype!(sim, KnownSellers)
    add_edgetype!(sim, Bought)
    add_globalseriestype!(sim, ExcessDemand)
    add_globalseriestype!(sim, AveragePrice)

Now we can also populate our simulation with the agents and the
`KnownSeller` network (the `Bought` network is a result of the
`calc_demand` transition, and therefore no edges are added in the
initialization phase).

Vahana therefore has the functions `add_agent!` or `add_agents!`,
where `add_agents` can be used to add multiple agents at once. These
functions return id(s) the agent(s), which can be used to create edges
between agents. Do not use the IDs for other purposes, they are not
guaranteed to be stable.

In our case, we use them to iterate over all buyer IDs, randomly select
`numSellers` seller IDs for each buyer ID, and then create edges
between them in the `KnownSellers` network.

We can see in the following code fragment also how parameters of the
Simulation can be accessed via the `param` function.

    buyerids = add_agents!(sim, [ Buyer() for _ in 1:param(sim, :numBuyer)])
    
    sellerids = add_agents!(sim, [ Seller() for _ in 1:param(sim, :numSeller)])
    
    for b in buyerids
        for s in rand(sellerids, param(sim, :knownSellers))
    	add_edge!(sim, s, b, KnownSellers)
        end
    end
    
    sim

    Simulation Name: Excess Demand
    Parameters: (numBuyer = 50, numSeller = 5, knownSellers = 2)
    Agent(s):
     Type Seller (ID: 2) with 0/5 (R/W) Agent(s)
     Type Buyer (ID: 1) with 0/50 (R/W) Agent(s)
    Network(s):
     Bought with 0/0 (R/W) Edges for 0/0 (R/W) Agent(s)
     KnownSellers with 0/100 (R/W) Edges for 0/50 (R/W) Agent(s)
    Global(s):
     ExcessDemand (empty)
     AveragePrice (empty)

As you can see from the result of the code block above, Vahana has a
"pretty print" function for some of its data structures. But to
understand the current output, it is important to know that for each
of the agent and edge types, Vahana has a container from which it
reads the state of the agents/edges and another to which it writes the
new state. During the initialization phase, the read containers are
empty, and the write containers are filled. In the above output you
can see the state of the two containers, where R stands for read and W
for write.

It's also important to aware that the given numbers are only for the
partition of the graph that is assigned to the process.\comment{Das gilt natürlich nur für die spätere MPI Version, und wird deswegen jetzt auch erstmal nicht weiter ausgeführt.}

To complete the initialization it is necessary to call
`finish_init!`. In the later MPI version this will be used e.g. to
distribute the graph to the different processes. The function also
sets all containers to a read-only state, which is why the (R/W) part
is now omitted in the output.

    finish_init!(sim)

    Simulation Name: Excess Demand
    Parameters: (numBuyer = 50, numSeller = 5, knownSellers = 2)
    Agent(s):
     Type Seller (ID: 2) with 5 Agent(s)
     Type Buyer (ID: 1) with 50 Agent(s)
    Network(s):
     Bought with 0 Edges for 0 Agent(s)
     KnownSellers with 100 Edges for 50 Agent(s)
    Global(s):
     ExcessDemand (empty)
     AveragePrice (empty)

Now (after `finish_init!`) we can also have a little look at our
agents and edges. But again, we can only see the partition that is
assigned to the process.

    show_agents(sim, Buyer)

    Vahana.BufferedAgentDict{Buyer}
    Read:
    0x010000010000002c => Buyer(0.7221645256627303, 32.0)
    0x0100000100000002 => Buyer(0.33510068710869545, 55.0)
    0x010000010000001f => Buyer(0.30004721198947626, 100.0)
    0x0100000100000013 => Buyer(0.7797234220044298, 85.0)
    0x0100000100000031 => Buyer(0.04432961745979447, 85.0)
    ...


## Transition functions

We have now a simulation with an initial state. To modify the state we
need to define transition functions. Those functions have the
signature `transition_function(agent::T, id::AgentID, sim::Simulation) -> Union{T, Nothing}`.

This function is called for every agent in the explicit graph layer.
The returned agents and edges are gathered by the platform and then
concatenated to yield the new state of the explicit graph layer.

The parameter `agent` contains the state of a single agent of the type
`T`. The parameter `id` contains the id of this agent, as the id is
not part of the agent state itself, and maybe needed for constructing
new edges. 


### `calc_demand`

The first transition function we are implementing calculates the
demand for the goods $x$ and $y$ as shown in equation (\ref{eqn:demand}).

This transition function is called for all Buyers. First the `edge_to`
function is used to get a `Vector` that cotains all the sellers known
by `b`. It's important to call `edges_to` only for `id`, and not
e.g. to `from` IDs of edges derived by `edges_to`.

One of the available edges to a sellers is selected using the `rand`
function.

For this seller the state is accessed via the `agentstate_from`
function (or alternatively via `agentstate(sim, seller_edge.from)`).\comment{Hier ist für die Parallel Version geplant, dass ~agentstate~ Zugriffe "lazy" sind, sprich hier eine MPI-Kommunikation erst stattfindet, falls auf den State tatsächlich zugegriffen wird (und dieser nicht schon gecacht wurde). Der State von Edges wird hingegen nach einer transition function immer zusammen mit der Information über die Edge selbst übertragen.}
Then the agent calculates it's demand for the goods $x$ and $y$, and
adds an edge with the information about the demand to the `Bought`
network, which is then used in the next transition function by the
sellers to sum up the demand and calculate the new price.

The state of the buyer itself isn't changed, but a transition function
must return either an instance of the type of the first parameter (in
our actual case a Buyer) or in the case that the agent should be
removed from the simulation `nothing`. So we just return the `b` we
got as function parameter.

    function calc_demand(b::Buyer, id, sim)
        seller_edge = rand(edges_to(sim, id, KnownSellers))
        s = agentstate_from(sim, seller_edge)
        x = b.B * b.α
        y = b.B * (1 - b.α) / s.p
        add_edge!(sim, id, seller_edge.from, Bought(x, y))
        b
    end


### `calc_price`

In the `calc_price` transition function the sellers summarise all the
goods $x$ and $y$ they sold. Therefore they first get all the Bought
edges that are pointing to them via the `network(sim, Bought)`.

In the case that they sold nothing, and therefore no edge (or to be
more exact, a Vector with the lenght 0) is returned, they do not
change there state. Otherwise the `states` function is used get a
Vector that only contain the states of all the edges. Remember that we
defined addition for `Bought`. This allows us to use the + Operator in
reduce to calculate the sum of all sold goods.

Then we construct a new seller. `q.y / q.x * s.p` is thereby the new
price as shown in equation (\ref{eqn:price}).

    function calc_price(s::Seller, id, sim)
        edges = edges_to(sim, id, Bought)
        if length(edges) == 0
    	return s
        end
        q = reduce(+, states(edges))
        Seller(q.y / q.x * s.p, q.y)
    end


### applying the transition functions

To apply our defined transistion functions to the simulation, Vahana
provides the `apply_transition!` function with the signature
`apply_transition!(sim::Simulation, f::Function, compute::Vector,
networks::Vector, rebuild::Vector)::Simulation`, where `f` is the
transition function. `compute` is a vector of agent types. In most
cases (like here) this vector will only contain a single type, but in
general it's possible to define a transition function for multiple
agent types (e.g. just define also `calc_demand(s::Seller, ...))` in
the case that sellers could also bought goods from other sellers, and
add `Seller` to the `compute` vector).

The `networks` vector must contain all edge types which are accessed in
the transition function via a call of the `network`
function.\comment{Eigentlich sind immer alle Netzwerke lesbar, aber a) finde ich es nicht schlecht, dass dies explizit angegeben werden muss, da dadurch alleien druch das Anschauen der verschiedene apply\_transition calls einen guten Überblick über den Informationsfluß gibt und vor allem in Zukunft auch Optimierungsmöglichkeiten bietet, da hierdurch angegeben wird, welche Netzwerke synchroniziert sein müssen, bevor mit der transition function angefangen werden kann.}

And the `rebuild` vector must contain all agent and edge types, which
are changed in this transition function. Only the containers of those
types and the `compute` types are writeable inside the transition
function, calling add\_agent! or add\_edge! on a type that is not in
`[ compute; networks ]` will throw an error. 

And be aware the writeable containers are starting with an empty set,
which also explains the `rebuild` term.\comment{Für eine spätere Version kann ich mir eine Version vorstellen, bei welcher Differenzen zum alten State erzeugt werden auch ein removeEdge erlaubt ist, dies kann sicherlich in bestimmten Fällen die benötigte Bandwidth verringern.}

So for our transition functions we have

    sim2 = apply_transition(sim, calc_demand, [ Buyer ], [ KnownSellers ], [ Bought ])

    Simulation Name: Excess Demand
    Parameters: (numBuyer = 50, numSeller = 5, knownSellers = 2)
    Agent(s):
     Type Seller (ID: 2) with 5 Agent(s)
     Type Buyer (ID: 1) with 50 Agent(s)
    Network(s):
     Bought with 50 Edges for 5 Agent(s)
     KnownSellers with 100 Edges for 50 Agent(s)
    Global(s):
     ExcessDemand (empty)
     AveragePrice (empty)

and 

    sim3 = apply_transition(sim2, calc_price, [ Seller ], [ Bought ], [ Bought ])

    Simulation Name: Excess Demand
    Parameters: (numBuyer = 50, numSeller = 5, knownSellers = 2)
    Agent(s):
     Type Seller (ID: 2) with 5 Agent(s)
     Type Buyer (ID: 1) with 50 Agent(s)
    Network(s):
     Bought with 0 Edges for 0 Agent(s)
     KnownSellers with 100 Edges for 50 Agent(s)
    Global(s):
     ExcessDemand (empty)
     AveragePrice (empty)

We used here a version of the `apply_transition` which returns a copy of
the simulation instead of changing the simulation inplace. This can be
usefull while implementing the model in the REPL, as the transition
does not destroy the original state, but should not be used in
production, here always the `apply_transition!` should be used.

But here we can now simply compare the simulation states before and
after calling the transition functions:

    show_agents(sim, Seller)

    Vahana.BufferedAgentDict{Seller}
    Read:
    0x0200000100000003 => Seller(0.8851435708320818, 0.0)
    0x0200000100000004 => Seller(1.416409135270989, 0.0)
    0x0200000100000001 => Seller(1.1030231790953082, 0.0)
    0x0200000100000002 => Seller(0.9192111212679848, 0.0)
    0x0200000100000005 => Seller(1.1471607867899096, 0.0)

    show_agents(sim3, Seller)

    Vahana.BufferedAgentDict{Seller}
    Read:
    0x0200000100000003 => Seller(1.7224062448942294, 426.72026790405715)
    0x0200000100000004 => Seller(0.9153111502078305, 185.90552446299466)
    0x0200000100000001 => Seller(0.946469894590766, 208.95502336759432)
    0x0200000100000002 => Seller(1.162187856903494, 349.0936671751986)
    0x0200000100000005 => Seller(1.2761448969654006, 199.40495816842736)


## Globals

However, our goal was to obtain a time series for the average price
and the excess demand. Vahana does not allow direct access to agent
state, but follows the MapReduce pattern from functional
programming. The Map part allows mapping an agent or edge state to a
new value, and the Reduce part describes how the new values should be
combined into a single value.

E.g. to calculate the excess demand we write

    aggregate(sim2, Bought, b -> b.x - b.y, +)

    -164.2289900008493

where the second parameter specifies for which agent or edge type the
aggregation should be performed, and the anonymous function in the
third place describes the assignment for each instance of that
type. In our case, the function gets an edge of type `Bought` and
calculates how much more of good x was bought. And this is then added
for all edges.

To calculate the average price we define a helper function

    function calc_average_price(sim)
        m = aggregate(sim, Seller, s -> s.p * s.d_y, +)
        q = aggregate(sim, Seller, s -> s.d_y, +)
        m / q
    end
    
    calc_average_price(sim3)

    1.2868587619631882

And now we have all elements to run the simulation, e.g. for 10 steps:

    for _ in 1:10
        apply_transition!(sim, calc_demand, [ Buyer ], [ KnownSellers], [ Bought ])
        add_globalstate!(sim, ExcessDemand(aggregate(sim, Bought, b -> b.x - b.y, +)))
        apply_transition!(sim, calc_price, [ Seller ], [ Bought ], [ Bought ])
        add_globalstate!(sim, AveragePrice(calc_average_price(sim)))
    end

To get the resulting timeseries, we use the `all_states` function:

    all_states(sim, ExcessDemand)

    10-element Vector{ExcessDemand}:
     ExcessDemand(-185.50317584222284)
     ExcessDemand(-200.6571534663646)
     ExcessDemand(82.05255297930877)
     ExcessDemand(-86.05116030368393)
     ExcessDemand(-297.68024322405324)
     ExcessDemand(-21.763045300946644)
     ExcessDemand(-35.255107251511276)
     ExcessDemand(20.491282451786503)
     ExcessDemand(-44.83375225804576)
     ExcessDemand(-60.40724947518859)

    all_states(sim, AveragePrice)

    10-element Vector{AveragePrice}:
     AveragePrice(1.49385260400276)
     AveragePrice(1.1692469696619405)
     AveragePrice(1.3165849867002954)
     AveragePrice(1.5404206764528308)
     AveragePrice(1.2140879476808186)
     AveragePrice(1.3465477339839174)
     AveragePrice(1.2748262084987954)
     AveragePrice(1.280919694200749)
     AveragePrice(1.207381728712588)
     AveragePrice(1.2171349154617417)

\bibliographystyle{gcfwp}
\bibliography{TheBib}


