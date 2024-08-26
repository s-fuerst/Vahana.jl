#=
# First Steps

# Goal

In this tutorial, we will build a simple market model with multiple
buyers and sellers. Each buyer has a fixed budget and preferences for
two goods, x and y. Sellers offer these goods, and buyers purchase
from a randomly selected subset of sellers at each time step. 

The model illustrates several core concepts in Vahana, including:

1. Defining agent and edge types as structs 
2. Initializing a simulation with agents and edges
3. Implementing transition functions to update agent states
4. Tracking simulation results

This simple model serves as an introduction to Vahana's capabilities
for agent-based modeling. While relatively basic, it demonstrates how
to represent a multi-agent system with different types of agents,
connections between agents, and state transitions driven by agent
interactions.

# Model Overview

Our model simulates a dynamic market with $n$ buyers and $m$ sellers,
trading two types of goods: $x$ and $y$. We use good $x$ as the
numeraire, fixing its price at 1, while the price of good $y$, denoted
as $p$, is variable and adjusted by sellers over time.  Each buyer in
our market has a unique preference and a constant budget $B$ for
purchasing both commodities. These preference is represented by an
individual parameter α, which determines the buyer's relative desire
for good $x$ versus good $y$. Buyers make their purchasing decisions based
on a Cobb-Douglas utility function:

```math
\max_{x,y} u(x, y) = x^\alpha \cdot y^{1 - \alpha} 
```

subject to their budget constraint: $x + y · p ≤ B$. This utility
maximization leads to an optimal demand for each good:

```math
\begin{aligned}
x &= B \cdot \alpha \\
y &= \frac{B \cdot (1 - \alpha)}{p} 
\end{aligned}
```

On the supply side, sellers offer both goods $x$ and $y$, which we assume
to be joint products. Their primary goal is to balance the production
and sales of these two goods. To achieve this, sellers adjust their
prices based on the aggregate demand from their customers. The price
adjustment mechanism is given by:

```math
\begin{aligned}
p_t &= \frac{d_y}{d_x} \cdot p_{t-1} \\
\textrm{where } d_x &= \sum_{b \in \textrm{buyers}}x_b ,\quad d_y = \sum_{b \in \textrm{buyers}} y_b 
\end{aligned}
```

# Agent and Edge Types

Now that we understand our market model, let's implement it using
Vahana. We'll start by defining our agent types and edge types, then
create and initialize our simulation.

First, we need to import the necessary modules:
=#

using Vahana
import Random: rand
import DataFrames


#=
In Vahana, agents and the edges between them are represented by
Julia structs. However, these structs must meet specific requirements:

1. They must be immutable.
2. They must be "bitstypes".

A bitstype in Julia is a type that is composed entirely of primitive
types (like Int, Float64, Bool) or other bitstypes, and has a known,
fixed size in memory.  This also implies that the type of a struct variables
must be declared.

This restrictions allows Vahana to efficiently manage and distribute
agents across processes in parallel simulations.

Let's define our agent types with these requirements in mind:

=#

struct Buyer
    α::Float64  # Preference parameter
    B::Float64  # Budget
end

struct Seller
    p::Float64   # Current price of good y
    d_y::Float64 # Total demand for good y (used for price adjustment)
end

# To facilitate agent creation with some randomization, we'll define
# custom constructors:

Buyer() = Buyer(rand(), rand((1:100)))

Seller() = Seller(rand() + 0.5, 0);

#=
These constructors create:

- Buyers with random $\alpha$ (between 0 and 1) and $B$ (between 1 and 100)
- Sellers with a random initial price $p$ (between 0.5 and 1.5) and zero initial demand.


Now, let's look at our edge types in more detail:

This `KnownSeller` network is fixed and describes the sellers known to each
buyer. In Vahana, the direction of an edge determines the flow of
information. Since buyers need price information from sellers to
calculate their demand, we define this as an edge from sellers to
buyers. We call this network KnownSeller.

The edges of the `KnownSeller` network don't carry additional
information, so we define them as an empty struct. We'll use this as a kind
of tag later to select only the edges of this type.
=#

struct KnownSeller end

#=
The Bought network represents actual transactions between buyers and
sellers. This network is dynamic, with edges created during the
simulation runtime. Each edge in the Bought network carries information
about the quantities of goods purchased in a transaction. Specifically:
=#

struct Bought
    x::Float64  # Quantity of good x bought
    y::Float64  # Quantity of good y bought
end

# To make working with Bought edges easier, we'll define an addition
# operation:

import Base.+
+(a::Bought, b::Bought) = Bought(a.x + b.x, a.y + b.y);

# This allows us to easily sum up multiple Bought edges, which will be
# useful when sellers calculate total demand.

# # Defining the Model Structure

#=

In Vahana, the [`ModelTypes`](@ref) constructor is the starting point
for defining the structure of your agent-based model. To populate this
instance, we use the `register_agenttype!` and `register_edgetype!`
functions. These functions tell Vahana about the types of agents and
edges in your model, allowing it to set up the necessary internal data
structures and optimizations. The |> operator can be used for function
chaining, providing a concise way to register multiple types.

=#

const modeltypes = ModelTypes() |>
    register_agenttype!(Buyer) |>
    register_agenttype!(Seller) |>
    register_edgetype!(KnownSeller) |>
    register_edgetype!(Bought);

# # Defining Model Parameters and Globals

#=
Vahana offers two ways to define parameters for your model: using
`register_param!` or creating a custom parameter struct. The `register_param!`
function allows you to register individual parameters with your
model. For example, you might use:
=#

modeltypes |>
    register_param!(:numBuyer, 50) |>
    register_param!(:numSeller, 5) |>
    register_param!(:knownSellers, 2);

#=

With this approach, the parameters can then be set to values other
than the default value via [`set_param!`](@ref) calls until the
simulation is initialized with [`finish_init!`](@ref).

Alternatively you can define a custom parameter struct and then pass an
instance of this struct to [`create_simulation`](@ref). 

In addition to parameters, Vahana allows you to manage global state
variables that can change during the simulation. This is particularly
useful for tracking aggregate statistics or maintaining shared
information across all agents. Similar to parameters, you can register
individual global variables:

=#


modeltypes |>
    register_global!(:x_minus_y, Vector{Float64}()) |>
    register_global!(:p, Vector{Float64}()); 

# Again, you could alternatively, you can define a custom struct for globals
# and pass an instance of this struct to [`create_simulation`](@ref) as
# shown in the other tutorials.

# Please note that Vahana's global state is distinct from the
# functionality of the `global` keyword in Julia programming
# language. The two concepts are unrelated and should not be conflated.

# # Create the Model and Simulation

#=

In Vahana, the term "model" is used in a somewhat unconventional way
compared to other agent-based modeling frameworks.  In Vahana, a model
created via [`create_model`](@ref) does not contain any rules about
how the state of a simulation changes over time. Instead, a Vahana
model is more akin to a specification of the possible state space - it
defines the set of all possible graphs that can be created with the
specified agent and edge types.

Think of a Vahana model as a blueprint or a schema. It outlines the
structure of your simulation - what types of agents can exist, what
types of relationships (edges) can exist between them, and what global
parameters and variables are available. However, it doesn't dictate
how these elements interact or evolve over time.

A simulation, on the other hand, is a concrete realization within this
state space. It's an actual graph with specific agents and edges,
representing the current state of your simulated world at a given
point in time.

Transition functions, which we'll define later, are the mechanisms
that actually change the state of the simulation over time. These
functions operate on the current state of the simulation (the current
graph of agents and edges) and produce a new state by modifying agent
attributes, creating or removing agents and edges, and updating global
variables. 

To create a model in Vahana, we use the [`create_model`](@ref) function. This
function takes the ModelTypes object we've been building through our
type registrations and parameter/globals definitions, and the name of
the model:

=#

const model = create_model(modeltypes, "Excess Demand")

# Once we have a model, we can create a simulation based on that model using
# the [`create_simulation`](@ref) function:
    
const sim = create_simulation(model)

#=

This function creates a new simulation instance based on our model. At this
point, the simulation is empty - it doesn't contain any agents or
edges yet. It's essentially a blank canvas ready for us to populate with
agents and edges according to our model's specifications.

# Populating the Simulation

After creating our simulation, the next step is to populate it with
agents and edges. Vahana provides [`add_agent!`](@ref),
[`add_agents!`](@ref), [`add_edge!`](@ref), and [`add_edges!`](@ref)
functions for this purpose.

The [`add_agent!`](@ref) function returns an `AgentID`, while [`add_agents!`](@ref)
returns a vector of `AgentID`s. These identifiers are unique to each
agent at the current state of the simulation.

In parallel simulations, the `AgentID` incorporates information about
the process number to which the agent is assigned. Consequently, the
ID may be modified if an agent is reassigned to a different process.
In the current implementation of Vahana, agents only change processes
during the [`finish_init!`](@ref) function. However, this design
allows for potential future implementation of dynamic load balancing.

It is also possible that the same `AgentID` is utilized
multiple times for distinct agents. Consequently, an `AgentID`
returned by a Vahana function call or passed as an argument to a
callback function (refer to the [Transition
functions](./performance.md#Transition functions) section below for
more details) is only valid within a specific scope. This scope is
limited to either the period before the finish_init! function is
invoked or until the callback/transition function has completed its
execution. After these points, the ID should not be considered
reliable for further use or reference.

If your model requires persistent identification of agents, you should
implement this yourself by adding an ID field to your agent struct. For
example:

```
struct BuyerWithID
    id::UUID  # created via the UUIDs standard library
    α::Float64
    B::Float64
end
```

You would then manage these IDs yourself, ensuring they remain
constant throughout the simulation.

In our case, we use the IDs returned by add_agents! to iterate over
all buyer IDs, randomly select `numSellers` seller IDs for each
buyer ID, and then create edges between them in the `KnownSeller`
network.

We can see in the following code snippet also how parameters of the
Simulation can be accessed via the [`param`](@ref) function.

=#

buyerids = add_agents!(sim, [ Buyer() for _ in 1:param(sim, :numBuyer)])

sellerids = add_agents!(sim, [ Seller() for _ in 1:param(sim, :numSeller)])

for b in buyerids
    for s in rand(sellerids, param(sim, :knownSellers))
        add_edge!(sim, s, b, KnownSeller())
    end
end

sim

# As you can see from the result of the code block above, Vahana has 
# "pretty print" functions for some of its data structures. 

# Finally, we call [`finish_init!`](@ref). This crucial step completes
# the initialization process, setting up data structures and, in
# parallel simulations, distributing agents across processes.

finish_init!(sim)

# # Defining Transition Functions

#=

In Vahana, transition functions define how your simulation evolves
from one state to the next. They encapsulate the rules and behaviors
of your agents, determining how agents interact, make decisions, and
change their states. The transition function is called for each agent separately.

A typical transition function in Vahana has the following signature:

```julia
function transition_function(state, id, sim)
    # Function body
    return new_agent_state
end
```

A transition function must have three parameters. The first represents the
current state of the agent, allowing it to make decisions based on its
own attributes and conditions. The second parameter is the temporary
ID of the agent. This ID can be used within the transition function to
access other elements of the graph that are visible to the agent via
functions like [`edges`](@ref) or [`neighborstates`](@ref). It's
important to note that this ID should not be stored, as it may change
between time steps.  Finally, there's `sim`, which is the simulation
object. This provides access to the global state and parameters of the
simulation. These three parameters together give the agent all the
context it needs to determine its next state.

The first transition function we are implementing calculates the
demand for the goods $x$ and $y$.

This transition function is called for all Buyers. First, the
[`neighborids`](@ref) function is used to get a vector that contains
the (temporary) IDs all the sellers known by the actual buyer.

One of the IDs of the sellers is selected using the `rand` function. For
this seller, the state is accessed via the [`agentstate`](@ref)
function. In cases where the type of the agent whose state we want to
access is unknown, it's possible to use [`agentstate_flexible`](@ref)
instead.

Then the agent calculates it's demand for the goods $x$ and $y$, and
adds an edge with the information about the demand to the `Bought`
network, which is then used in the next transition function by the
sellers to sum up the demand and calculate the new price.

=#

function calc_demand(b::Buyer, id, sim)
    seller = rand(neighborids(sim, id, KnownSeller))
    s = agentstate(sim, seller, Seller)
    x = b.B * b.α
    y = b.B * (1 - b.α) / s.p
    add_edge!(sim, id, seller, Bought(x, y))
end;

#=

In the `calc_price` transition function, sellers summarize all the goods $x$
and $y$ they sold. They do this by summing the state of all incoming
Bought edges. 

If a seller hasn't made any sales (i.e., no incoming Bought edges
exist), [`edgestates`](@ref) returns `nothing`, and the seller's state
remains unchanged. Otherwise, `edgestates` returns a Vector containing
the states of all relevant edges. We aggregate this vector using the
`reduce` function, leveraging the previously defined `+` operator for Bought.

Finally, we construct a new seller state. The new price is calculated
as `q.y / q.x * s.p`.

=#

function calc_price(s::Seller, id, sim)
    sold = edgestates(sim, id, Bought)
    if isnothing(sold) 
        return s
    end
    q = reduce(+, sold) 
    Seller(q.y / q.x * s.p, q.y)
end;

#=

# Applying Transition Functions

To apply these transition functions to the current state of a
simulation, Vahana provides the [`apply!`](@ref) method. This method
is the key mechanism for evolving the simulation state over time,
executing our defined transition functions across the population of
agents.  Let's examine its signature and behavior in more detail:

```julia
apply!(sim, func, call, read, write; add_existing = [], with_edge = nothing)
```

`sim` is the simulation instance and `func` is the transition function to be
applied to the simulation state.

The `call` argument in `apply!` specifies which agent types the
transition function should be applied to. This can be either a single
agent type or a collection of agent types. In most cases, as in our
market model, call will contain only a single type. For example, when
we apply calc_demand, we only want to call it for Buyer
agents. However, Vahana allows for more complex scenarios where a
single transition function can be applied to multiple agent
types. This flexibility allows for more complex agent interactions and
behaviors within a single transition function, which can be
particularly useful in models where different types of agents share
similar behaviors or decision-making processes.

The `read` argument must include all agent and edge state types that
are accessed in the transition function. This is particularly
important in parallel simulations, as it ensures that all necessary
data is transmitted to the processes that need to access it. When
Vahana's assertion system is active, it checks that only these
specified types are accessed. However, if assertions are disabled via
[`enable_asserts`](@ref), forgetting to include a type in read can
lead to incorrect results without raising an error. It's worth noting
that while including unnecessary types in read doesn't cause errors,
it can negatively impact performance.

A noteworthy scenario occurs when the `call` type is excluded from the
`read` set. In these instances, the transition function's first argument is
altered. Rather than representing the agent state, it becomes a Value
Type corresponding to the 'call' type. Consequently, the state of the
agent for which the transition function is invoked becomes inaccessible within
the function itself. 

The `write` collection must contain all agent and edge state types
that are modified in the transition function. Conceptually, one might think of
the resulting graph after a transition as the union of all agents and
edges returned by individual transition function calls. However, this
approach would be inefficient, requiring the reconstruction of even
stable parts of the graph. Instead, Vahana optimizes this process by
removing only the parts of the graph specified in the write
collection. This means you can only change the state of agents or
edges of a type if you're also re-adding the state of constant elements
of that type.

But in cases where you want to add new elements (like additional edges)
without modifying existing ones, you can use the optional
`add_existing` keyword. This keyword takes a single or a collection of
agent or edge state types. For all types in this collection, existing
agents or edges will be preserved, even if the type is also in the
write collection. This provides a flexible way to extend the graph
without completely rebuilding it. But there is the restriction that
agent types in `add_existing` can not be also in `call`.

The `with_edge` keyword restricts the application of the transition function
to agents that are targets of a specified edge type. It's equivalent
to manually checking for the presence of the edge for each agent
before applying the function, but allows Vahana to optimize this
operation internally. `with_edge` should only be used when a small
proportion of agents have edges of the specified type. If most agents
have this edge type, using with_edge may decrease performance compared to
a manual check inside the transition function.

The implementation process, while seemingly complex, is quite
straightforward in practice. Begin by crafting your transition function as
previously demonstrated. Then, follow these steps:

1. Identify the agent types for which the transition function should be invoked and include these types in the `call` parameter.
2. Examine the function calls, such as [`edgestates`](@ref), to determine which types are utilized. Add these types to the `read` parameter.
3. If you access the first argument of the transition function (representing the agent's state), include that type in the `read` parameter as well.
4. Should you intend to return a modified agent state, add that type to the `write` parameter.
5. If your function includes additional `add_edge!` or `add_agent!` calls, incorporate their respective types into the `write` parameter.
6. Be aware that all existing agents and edges will be removed from the simulation unless their types are also specified in the `add_existing` parameter.


Following this schema we get for our transition functions

=#

apply!(sim, calc_demand, Buyer, [ Buyer, Seller, KnownSeller ], Bought);

# and

apply!(sim, calc_price, Seller, [ Seller, Bought ], Seller);


# # Working with Globals

#=

To summarize the state of a simulation, Vahana uses the map-reduce
combination from functional programming. First, a function is applied to each
agent or edge state, and then the result is reduced using a binary function.

E.g. to calculate the excess demand we write

=#


mapreduce(sim, b -> b.x - b.y, +, Bought)

#=

The last argument of Vahana's mapreduce specifies the agent or edge
type for which the aggregation should be performed. The anonymous
function in the second position describes the assignment for each
instance of that type. In this case, the function receives an edge of
the 'Bought' type and calculates the additional quantity of good x
that was purchased. This value is then summed across all edges of the
specified type.

To calculate the average price we define a helper function

=#


function calc_average_price(sim)
    m = mapreduce(sim, s -> s.p * s.d_y, +, Seller)
    q = mapreduce(sim, s -> s.d_y, +, Seller)
    m / q
end

calc_average_price(sim)

# And now we have all elements to run the simulation, e.g. for 5 steps:

for _ in 1:5
    apply!(sim, calc_demand, Buyer, [ Buyer, Seller, KnownSeller ], Bought)
    push_global!(sim, :x_minus_y, mapreduce(sim, b -> b.x - b.y, +, Bought))
    apply!(sim, calc_price, Seller, [ Seller, Bought ], Seller)
    push_global!(sim, :p, calc_average_price(sim))
end

# To get a resulting timeseries, we use the [`get_global`](@ref) function:

get_global(sim, :p)

# # Investigating the Simulation State

#=

Vahana offers several methods to examine the current state of your
simulation. For example, the user-defined show methods for the model
and simulation instances, which provide a quick summary of your
model/simulation:

=#

model
#

sim

# Vahana offers the `show_agent` function to inspect the state of individual
# agents. This function displays comprehensive information about a
# randomly selected agent of the specified type. Additionally, users
# have the option to specify a particular agent identifier if they wish
# to examine a specific agent. The `show_agent` function returns the
# identifier of the selected agent.

show( #hide
show_agent(sim, Seller, 1) 
) #hide

#=

The `show_agent` function has an optional keyword argument, `neighborstate`,
which enables the user to examine the state of neighboring agents. The
`neighborstate` argument should be a vector of symbols representing
the field names that should be displayed.

=#

show( #hide
show_agent(sim, Seller, 1; neighborstate = [ :B ]) 
) #hide

#=

As mentioned in the [Agent and Edge Types](#Agent-and-Edge-Types)
section, all agent and edge types in Vahana must be of type
`bitstype`. This has the usefull side effect that the state of the agents
and edges can be nicely converted into a DataFrame.

It is important to note that the DataFrame in a parallel simulation
contains only the agents or edges of the process in which the function is
called, and not those of the complete simulation. If the latter is
required, [`all_agents`](@ref) or [`all_edges`](@ref) can be used.

=#

first(DataFrame(sim, Bought; types = true), 10)

#

# To get the DataFrame for the Globals, we call the `GlobalsDataFrame`
# function for our sim. Be aware, that only global variable that are vectors
# are added to the DataFrame.

GlobalsDataFrame(sim)


# # Finish the Simulation

#=

Vahana employs an internal C library for memory allocation during
simulations. Upon completing a simulation, it is necessary to invoke
the `finish_simulation` function to properly deallocate the memory resources
utilized. This function returns the global variables associated with the
concluded simulation.  

=#

finish_simulation!(sim)

# # Understanding Vahana's Edge Structure

#=
When we define edge types like `KnownSeller` or `Bought`, you might
notice that these structs don't contain any information about the
source or target agents of the edge. This is by design in Vahana,
which uses a specific internal structure to represent edges
efficiently.

Internally, Vahana declares a parametric type for edges:

```
struct Edge{T} 
    from::AgentID
    state::T
end
```

In this structure, `T` is the type of our edge (like `KnownSeller` or
`Bought`), and from is the ID of the agent at the source of the
edge. You might wonder why there's no to field for the target agent. The
reason lies in how Vahana stores these edges.

Vahana uses container structures (like dictionaries) to store
edges. The exact type of container depends on certain optimization
hints (see Edge Hints for more details), but a typical structure might
look like this:

`Dict{AgentID, Vector{Edge{T}}}`

This design means that the target agent's ID is implicitly stored
as the dictionary key. Adding it to the Edge struct would be redundant
and would unnecessarily consume memory and CPU cycles. As a user of
Vahana, you don't need to interact with this internal structure
directly, but understanding it can help you design more efficient
models and better understand how Vahana works under the hood.
=#

