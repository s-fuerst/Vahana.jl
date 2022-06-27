#=
# Tutorial

TODO: Goal of Tutorial

# Model Background

We have a simple but volatile market with $n$ buyers, $m$ sellers, and
two goods called $x$ and $y$. Each seller sells both commodities,
buyers buy both commodities from a single seller, but at each step the
buyer randomly selects a seller from a fixed subset of all
sellers. We are only interested in relative prices, so the price for
good $x$ is set to 1, the price for good $y$ is $p$.

At each step, each buyer has a constant fixed budget $B$ for buying the
commodities. The quantities are choosen using a Cobb-Douglas
utility function:


```math
\begin{aligned}
\max_{x,y} u(x, y) &= x^\alpha \cdot y^{1 - \alpha} \\
\textrm{s.t.:} & x + y \cdot p \leq B
\end{aligned}
```

The solution of this optimization is:

```math
\begin{aligned}
x &= B \cdot \alpha \\
y &= \frac{B \cdot (1 - \alpha)}{p} 
\end{aligned}
```

For illustrative reasons, we neglect production except for the
assumption that $x$ and $y$ are joint products, so that the seller
tries to find a price where $x =y$. In the case of a single buyer,
the solution is $p = \frac{1 - \alpha}{\alpha}$.  But in our case,
the sellers are facing different buyers with different preferences
$\alpha$ and a different budget $B$. So they start with a random
price and adjust that price at each time step:


```math
\begin{aligned}
p_t &= \frac{d_y}{d_x} \cdot p_{t-1} \\
\textrm{where } d_x &= \sum_{b \in \textrm{buyers}}x_b ,\quad d_y = \sum_{b \in \textrm{buyers}} y_b 
\end{aligned}
```

It's easy to see that in the single buyer case, this will lead to $p =
\frac{1 - \alpha}{\alpha}$ after a single step.

# Goal

In this tutorial, we will build a model with multiple buyers and
sellers, whereby the buyers will always randomly choice a seller from
a fixed subset of the sellers. This subset is of course different for
each buyer.

# Initialization

In addition to the Vahana package, we also need the Random package to
initialize the buyer with different random values for B and $\alpha$
and to select a seller from the list of known sellers.

=#

using Vahana
using Random

# We have two types of agents, `Buyer` and `Seller`. In Vahana,
# agents are defined as (immutable) structs. The structs define the
# state of the various agent types. All the types defined for the Model
# must be "plain data" types. Such a type is call isbitstype in Julia,
# and must fulfill the following contrains: A bitstype is immutable and
# contains no references to other values, only primitive types and other
# isbitstype types.
# 
# Since we want to calculate the average price of $y$ later, the seller
# needs to store the quantity of the sold good $y$. So we have:

struct Buyer
    α::Float64
    B::Float64
end

struct Seller 
    p::Float64
    d_y::Float64 # the sum of bought goods y (for a single time step)
end

# We also write constructors for these agents. For the Buyer we want for
# $\alpha$ a random value between zero and one, and for B a random
# integer betwenn 1 and 100. The initial price $p$ of a seller should be
# between 0.5 and 1.5.

Buyer() = Buyer(rand(), rand((1:100)))

Seller() = Seller(rand() + 0.5, 0);

# And we have also two types of networks. The first is fixed and
# describes the sellers known to a single buyer. Since the direction of
# an edge determines the flow of information and the buyer needs the
# price information to calculate his demand, it is a network from
# sellers to buyers. We call this network `KnownSeller`.

# As for the agents we must define a struct for each type of network.  The edges
# of the `KnownSeller` network do not carry additional information, so
# we define them just as a struct without any field.

struct KnownSellers end

# The second network `Bought` will be constructed in the
# transition function called `calc_demand` which will be the
# implementation of equation (\ref{eqn:demand}). This network describes
# from which seller a buyer bought something, so it edge will go from
# the buyers to the sellers. And those edges come with additional state,
# which provides the quantity of the bought goods:

struct Bought 
    x::Float64
    y::Float64
end

# We need sometimes to sum those quantities, so we define a + operator
# for the this struct:

import Base.+
    +(a::Bought, b::Bought) = Bought(a.x + b.x, a.y + b.y);

# Before we can construct our simulation, we need to define two
# additional structures. The first structure contains the parameters of
# the simulation.

Base.@kwdef struct Params
    numBuyer::Int64
    numSeller::Int64
    knownSellers::Int64
end;

# We use the `@kwdef` macro here only for code readability
# reasons, as this allows us to use the name of the parameters in the
# `Params` constructor.

# The second struct allows us to add a global state to the
# simulation, which can be an exogenous input or an (aggregated) state
# from the simulation itself. This global state can then be used for the
# agents' decisions or, as in our case, as the output of the simulation,
# namely the development of the average price and excess demand.

mutable struct Globals
    x_minus_y::Vector{Float64}
    p::Vector{Float64}
end

# We now have all the elements needed to construct an uninitialized
# simulation. Therefore we combine first all the Agent- and Edgetypes
# into a `ModelTypes` collection, via the `register_agenttype!` and
# `register_edgetype!` functions, and then call `contruct_model` on this
# collection. 

const model = ModelTypes() |>
    register_agenttype!(Buyer) |>
    register_agenttype!(Seller) |>
    register_edgetype!(KnownSellers) |>
    register_edgetype!(Bought) |>
    construct_model("Excess Demand")

# `construct_model` returns a blueprint for our simulation. Simulations
# itself can be seen as instances of models, where each simulation has
# it's individual state and set of parameters.  The Simulation is
# instanciated by the `new_simulation` function, which needs beside
# the model also the parameters struct and the globals struct, whereby
# the parameters and globals can be also `nothing`. 

const sim = new_simulation(model,
                           Params(numBuyer = 50,
                                  numSeller = 5,
                                  knownSellers = 2),
                           Globals(Vector(),
                                   Vector()))

# Now we can also populate our simulation with the agents and the
# `KnownSeller` network (the `Bought` network is a result of the
# `calc_demand` transition, and therefore no edges are added in the
# initialization phase).

# Vahana therefore has the functions `add_agent!` or `add_agents!`,
# where `add_agents` can be used to add multiple agents at once. These
# functions return id(s) the agent(s), which can be used to create edges
# between agents. Do not use the IDs for other purposes, they are not
# guaranteed to be stable.

# In our case, we use them to iterate over all buyer IDs, randomly select
# `numSellers` seller IDs for each buyer ID, and then create edges
# between them in the `KnownSellers` network.

# We can see in the following code fragment also how parameters of the
# Simulation can be accessed via the `param` function.

buyerids = add_agents!(sim, [ Buyer() for _ in 1:param(sim, :numBuyer)])

sellerids = add_agents!(sim, [ Seller() for _ in 1:param(sim, :numSeller)])

for b in buyerids
    for s in rand(sellerids, param(sim, :knownSellers))
        add_edge!(sim, s, b, KnownSellers())
    end
end

sim

# As you can see from the result of the code block above, Vahana has a
# "pretty print" function for some of its data structures. But to
# understand the current output, it is important to know that for each
# of the agent and edge types, Vahana has a container from which it
# reads the state of the agents/edges and another to which it writes the
# new state. During the initialization phase, the read containers are
# empty, and the write containers are filled. In the above output you
# can see the state of the two containers, where R stands for read and W
# for write.

# It's also important to aware that the given numbers are only for the
# partition of the graph that is assigned to the process.\comment{Das gilt natürlich nur für die spätere MPI Version, und wird deswegen jetzt auch erstmal nicht weiter ausgeführt.}

# To complete the initialization it is necessary to call
# `finish_init!`. In the later MPI version this will be used e.g. to
# distribute the graph to the different processes. The function also
# sets all containers to a read-only state, which is why the (R/W) part
# is now omitted in the output.

finish_init!(sim)

# Now (after `finish_init!`) we can also have a little look at our
# agents and edges. But again, we can only see the partition that is
# assigned to the process.

show_agents(sim, Buyer)

# # Transition functions

# We have now a simulation with an initial state. To modify the state we
# need to define transition functions. Those functions have the
# signature `transition_function(agent::T, id::AgentID, sim::Simulation)`.

# The parameter `agent` contains the state of a single agent of the type
# `T`. The parameter `id` contains the id of this agent, as the id is
# not part of the agent state itself, and maybe needed for constructing
# new edges. 

# ## `calc_demand`

# The first transition function we are implementing calculates the
# demand for the goods $x$ and $y$ as shown in equation (\ref{eqn:demand}).

# This transition function is called for all Buyers. First the `edge_to`
# function is used to get a `Vector` that cotains all the sellers known
# by `b`. It's important to call `edges_to` only for `id`, and not
# e.g. to `from` IDs of edges derived by `edges_to`.

# One of the available edges to a sellers is selected using the `rand`
# function. For this seller the state is accessed via the `agentstate`
# function. In the case that the type of the agent from which we want
# to access the state is unknown, it's possible to use the
# `agentstate_flexible` instead. TODO see for details.

# Then the agent calculates it's demand for the goods $x$ and $y$, and
# adds an edge with the information about the demand to the `Bought`
# network, which is then used in the next transition function by the
# sellers to sum up the demand and calculate the new price.

# The state of the buyer itself isn't changed, but a transition function
# must return either an instance of the type of the first parameter (in
# our actual case a Buyer) or in the case that the agent should be
# removed from the simulation `nothing`. So we just return the `b` we
# got as function parameter.

function calc_demand(b::Buyer, id, sim)
    seller_edge = rand(edges_to(sim, id, KnownSellers))
    s = agentstate(sim, seller_edge.from, Seller)
    x = b.B * b.α
    y = b.B * (1 - b.α) / s.p
    add_edge!(sim, id, seller_edge.from, Bought(x, y))
    b
end

# ## `calc_price`

# In the `calc_price` transition function the sellers summarise all the
# goods $x$ and $y$ they sold. Therefore they first get all the Bought
# edges that are pointing to them via the `network(sim, Bought)`.

# In the case that they sold nothing, and therefore no edge (or to be
# more exact, a Vector with the lenght 0) is returned, they do not
# change there state. Otherwise the `edgestates` function is used get a
# Vector that only contain the states of all the edges. Remember that we
# defined addition for `Bought`. This allows us to use the + Operator in
# reduce to calculate the sum of all sold goods.

# Then we construct a new seller. `q.y / q.x * s.p` is thereby the new
# price as shown in equation (\ref{eqn:price}).

function calc_price(s::Seller, id, sim)
    edges = edgestates(sim, id, Bought)
    if isnothing(edges) 
        return s
    end
    q = reduce(+, edges)
    Seller(q.y / q.x * s.p, q.y)
end

# ## applying the transition functions

# To apply our defined transistion functions to the simulation, Vahana
# provides the `apply_transition!` function with the signature
# `apply_transition!(sim::Simulation, f::Function, compute::Vector,
# networks::Vector, rebuild::Vector)`, where `f` is the
# transition function. `compute` is a vector of agent types. In most
# cases (like here) this vector will only contain a single type, but in
# general it's possible to define a transition function for multiple
# agent types (e.g. just define also `calc_demand(s::Seller, ...))` in
# the case that sellers could also bought goods from other sellers, and
# add `Seller` to the `compute` vector).

# The `networks` vector must contain all edge types which are accessed in
# the transition function via a call of the `network`
# function.

# And the `rebuild` vector must contain all agent and edge types, which
# are changed in this transition function. Only the containers of those
# types and the `compute` types are writeable inside the transition
# function, calling add\_agent! or add\_edge! on a type that is not in
# `[ compute; networks ]` will throw an error. 

# And be aware the writeable containers are starting with an empty set,
# which also explains the `rebuild` term.

# So for our transition functions we have

sim2 = apply_transition(sim, calc_demand, [ Buyer ], [ KnownSellers ], [ Bought ])

# and

sim3 = apply_transition(sim2, calc_price, [ Seller ], [ Bought ], [ Bought ])

# We used here a version of the `apply_transition` which returns a copy of
# the simulation instead of changing the simulation inplace. This can be
# usefull while implementing the model in the REPL, as the transition
# does not destroy the original state, but should not be used in
# production, here always the `apply_transition!` should be used.

# But here we can now simply compare the simulation states before and
# after calling the transition functions:

show_agents(sim, Seller)

# and

show_agents(sim3, Seller)

# # Globals

# However, our goal was to obtain a time series for the average price
# and the excess demand. Vahana does not allow direct access to agent
# state, but follows the MapReduce pattern from functional
# programming. The Map part allows mapping an agent or edge state to a
# new value, and the Reduce part describes how the new values should be
# combined into a single value.

# E.g. to calculate the excess demand we write

aggregate(sim2, b -> b.x - b.y, +, Bought)

# where the second parameter specifies for which agent or edge type the
# aggregation should be performed, and the anonymous function in the
# third place describes the assignment for each instance of that
# type. In our case, the function gets an edge of type `Bought` and
# calculates how much more of good x was bought. And this is then added
# for all edges.

# To calculate the average price we define a helper function

function calc_average_price(sim)
    m = aggregate(sim, s -> s.p * s.d_y, +, Seller)
    q = aggregate(sim, s -> s.d_y, +, Seller)
    m / q
end

calc_average_price(sim3)

# And now we have all elements to run the simulation, e.g. for 10 steps:

for _ in 1:10
    apply_transition!(sim, calc_demand, [ Buyer ], [ KnownSellers], [ Bought ])
    pushglobal!(sim, :x_minus_y, aggregate(sim, b -> b.x - b.y, +, Bought))
    apply_transition!(sim, calc_price, [ Seller ], [ Bought ], [ Bought ])
    pushglobal!(sim, :p, calc_average_price(sim))
end

# To get the resulting timeseries, we use the `getglobal` function:

getglobal(sim, :p)

# and

getglobal(sim, :x_minus_y)

