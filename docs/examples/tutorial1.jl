#=
# Model Background

We have a simple but volatile market with $n$ buyers, $m$ sellers, and
two goods called $x$ and $y$. Each seller sells both commodities,
buyers buy both commodities from a single seller, but at each step the
buyer randomly selects the seller from a fixed subset of all
sellers. We are only interested in relative prices, so the price for
good $x$ is set to 1, the price for good $y$ is $p$.

At each step, each buyer has a constant fixed budget $B$ for buying the
commodities. The quantities are choosen using a Cobb-Douglas
utility function:

\begin{eqnarray}
\max_{x,y} && u(x, y) = x^\alpha \cdot y^{1 - \alpha} \\
\textrm{s.t.:} && x + y \cdot p \leq B \nonumber
\end{eqnarray}

The solution of this optimization is:

\begin{eqnarray} 
x &=& B \cdot \alpha \label{eqn:demand} \\
y &=& \frac{B \cdot (1 - \alpha)}{p} \nonumber
\end{eqnarray}

For illustrative reasons, we neglect production except for the
assumption that $x$ and $y$ are joint products, so that the seller
tries to find a price where $x =y$. In the case of a single buyer,
the solution is $p = \frac{1 - \alpha}{\alpha}$.  But in our case,
the sellers are facing different buyers with different preferences
$\alpha$ and a different budget $B$. So they start with a random
price and adjust that price at each time step:

\begin{eqnarray}
&&p_t = \frac{d_y}{d_x} \cdot p_{t-1} \label{eqn:price} \\
\textrm{where }&& d_x = \sum_{b \in \textrm{buyers}}x_b ,\quad d_y = \sum_{b \in \textrm{buyers}} y_b  \nonumber 
\end{eqnarray}

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
# into a `ModelTypes` collection, via the `add_agenttype!` and
# `add_edgetype!` function

const modeltypes = ModelTypes() |>
    add_agenttype!(Buyer) |>
    add_agenttype!(Seller) |>
    add_edgetype!(KnownSellers) |>
    add_edgetype!(Bought); 

# The Simulation construction itself is done by the `construct`
# function, which needs beside the collection of model types also the
# name of the simulation, the parameters struct and the globals struct, whereby
# the parameters and globals can be also `nothing`.

const sim = construct(modeltypes,
                      "Excess Demand",
                      Params(numBuyer = 50,
                             numSeller = 5,
                             knownSellers = 2),
                      Globals(Vector(),
                              Vector()))

