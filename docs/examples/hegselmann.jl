# # Opinion Model

# In this example we demonstrate how we can use the Graphs.jl package
# to add graphs from other sources (or created by the Graphs.jl
# package itself) to a Vahana simulation.

# For this, we implement a simple version of the [Hegselmann and
# Krause (2002)](http://jasss.soc.surrey.ac.uk/5/3/2.html) opinion
# dynamics model. An alternative implementation of the same model
# using the Agents.jl package can be found
# [here](https://juliadynamics.github.io/Agents.jl/v4.0/examples/hk/).

using Vahana, Statistics

# We have a finite number $n$ of agents, where the state of
# the agents are a real number $x_i(t)$ in the [0,1] interval which
# represents the opinion of that agent. 

struct HKAgent
    opinion::Float64
end

# This time we also have only one network that determine the agents
# that will be considered when an agent updates its opinion.

struct Knows end

# There is a *confidence bound* $\epsilon > 0$, opinions with a
# difference greater then $\epsilon$ are ignored by the agents in the
# transition function. All agents have the same confidence bound, so we
# introduce this bound as a parameter.

struct HKParams
    ε::Float64
end

# We have now all elements to create an uninitialized simulation.

const hkmodel = ModelTypes() |>
    register_agenttype!(HKAgent) |>
    register_edgetype!(Knows) |>
    construct_model("Hegselmann-Krause");

# # Add the graph 

# Vahana allows to add `SimpleGraphs` and `SimpleDiGraphs` from the
# [Graphs.jl](https://juliagraphs.org/Graphs.jl/dev/) package via the
# [`add_graph!`](@ref) function. So it's possible to use e.g.
# [SNAPDatasets](https://github.com/JuliaGraphs/SNAPDatasets.jl) to
# run the opinion model on real datasets. Or the SimpleGraphs module
# from Graphs.jl to create synthetic graphs.

# We show here for both use cases one example and are creating for each
# one an own simulation

const cgsim = new_simulation(hkmodel, HKParams(0.2), nothing);
const snapsim = new_simulation(hkmodel, HKParams(0.2), nothing)

# ## SimpleGraphs 

# First we will show how we can add a synthetic graph. For this we
# need to import the SimpleGraphs module. Since there are many
# functions in the Graphs.jl package with the same name as in Vahana
# (e.g. add_edge!) it is advisable to import only the needed parts of
# Graphs.jl instead of loading the whole package via `using Graphs`.

import Graphs.SimpleGraphs

# We want to add a complete graph, where each agent is connected with
# all the other agents, like in the Agents.jl implementation. We can
# create such a graph via `SimpleGraphs.complete_graph`.

g = SimpleGraphs.complete_graph(50)

# Vahana needs the information how to convert the nodes and edges of
# the SimpleGraphs object to the Vahana structure, this is done by the
# constructor functions in the third and forth arguments of
# [`add_graph!`](@ref). We do not need the Graph.vertix and Graph.edge
# arguments of this constructor functions, but for other use cases
# e.g. for bipartite graphs, it would be possible to create agents of
# different types depending on this information.

const agentids = add_graph!(cgsim,
                            g,
                            _ -> HKAgent(rand()),
                            _ -> Knows());

# Each agent also adds its own opinion to the calculation. We can use
# the ids returned by the [`add_graph!`](@ref) functions for this.

foreach(id -> add_edge!(cgsim, id, id, Knows()), agentids) 

finish_init!(cgsim)

# ## SNAPDataset.jl

# The SNAPDataset.jl package deliver Graphs.jl formatted datasets from
# the [Stanford Large Network Dataset
# Collection](https://snap.stanford.edu/data/index.html).

using SNAPDatasets

# With this package we can use the `loadsnap` function to create the graph
# that is then added to the Vahana graph, e.g. in our example the
# facebook dataset.

const snapids = add_graph!(snapsim,
                           loadsnap(:facebook_combined),
                           _ -> HKAgent(rand()),
                           _ -> Knows());

# Each agent also adds its own opinion to the calculation. We can use
# the ids returned by the [`add_graph!`](@ref) functions for this.

foreach(id -> add_edge!(snapsim, id, id, Knows()), snapids) 

finish_init!(snapsim)




# ## Transition Function


# Opinions are updated synchronously according to 
# ```math
# \begin{aligned}
# x_i(t+1) &= \frac{1}{| \mathcal{N}_i(t) |} \sum_{j \in \mathcal{N}_i(t)} x_j(t)\\
# \textrm{where } \quad \mathcal{N}_i(t) &= \{ j : \| x_j(t) - x_i(t) \| \leq \epsilon \}
# \end{aligned}
# ```

# So we first filter all agents from the neighbors with an opinion
# outside of the confidence bound, and then calculate the mean of the
# opinions of the remaining agents. As we have 

function step(agent, id, sim)
    ε = param(sim, :ε)

    accepted = filter(neighborstates(sim, id, Knows, HKAgent)) do other
        abs(other.opinion - agent.opinion) < ε
    end
    HKAgent(mean(map(a -> a.opinion, accepted)))
end;

# We can now apply the transition function to the complete graph simulation

apply_transition!(cgsim, step, [ HKAgent ], [ HKAgent, Knows ], [])

# Or to our facebook dataset

apply_transition!(snapsim, step, [ HKAgent ], [ HKAgent, Knows ], [])

# # Plot

# Finally, we show the visualization possibilities for graphs, and import the
# necessary packages for this and create a colormap for the nodes.

import CairoMakie, GraphMakie, NetworkLayout, Colors, Makie, Graphs

colors = Colors.range(Colors.colorant"red", stop=Colors.colorant"green", length=100);

# Vahana implements an interactive plot function based on GraphMakie, where
# agents and edges are given different colors per type by default, and
# the state of each agent/edge is displayed via mouse hover
# actions. We are using a helper function to modify the node colors to
# indicate the agent's opinion and add a color bar to the plot.

function plot_opinion(sim)
    vg = vahanagraph(sim)
    f, _, plt = Vahana.plot(vg) 
    plt.node_color[] = [ colors[nodestate(vg, i).opinion * 100 |> ceil |> Int]
                         for i in 1:Graphs.nv(vg) ]
    Makie.Colorbar(f[:, 2]; colormap = colors)
    f
end;

# Since the full graph is very cluttered and the Facebook dataset is
# too large, we construct a Clique graph using Graphs.jl.

const cysim = new_simulation(hkmodel, HKParams(0.25), nothing);
const cyids = add_graph!(cysim,
                         SimpleGraphs.clique_graph(7, 8),
                         _ -> HKAgent(rand()),
                         _ -> Knows());


foreach(id -> add_edge!(cysim, id, id, Knows()), cyids) 

finish_init!(cysim);

# First we plot the initial state

plot_opinion(cysim)

# And then the state after 500 iterations

for _ in 1:500
    apply_transition!(cysim, step, [ HKAgent ], [ HKAgent, Knows ], [])
end

plot_opinion(cysim)

