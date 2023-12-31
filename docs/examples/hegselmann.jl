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
    create_model("Hegselmann-Krause");

# # Add the graph 

# Vahana allows to add `SimpleGraphs` and `SimpleDiGraphs` from the
# [Graphs.jl](https://juliagraphs.org/Graphs.jl/dev/) package via the
# [`add_graph!`](@ref) function. So it's possible to use e.g.
# [SNAPDatasets](https://github.com/JuliaGraphs/SNAPDatasets.jl) to
# run the opinion model on real datasets. Or the SimpleGraphs module
# from Graphs.jl to create synthetic graphs.

# We show here for both use cases one example and are creating for each
# one an own simulation

const cgsim = create_simulation(hkmodel, HKParams(0.2), nothing);
const snapsim = create_simulation(hkmodel, HKParams(0.2), nothing)

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
# [`add_graph!`](@ref). We do not need the Graph.vertex and Graph.edge
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

# Again each agent also adds its own opinion to the calculation.

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

    opinions = map(a -> a.opinion, neighborstates(sim, id, Knows, HKAgent))

    accepted = filter(opinions) do opinion
        abs(opinion - agent.opinion) < ε
    end
    
    HKAgent(mean(accepted))
end;

# We can now apply the transition function to the complete graph simulation

apply!(cgsim, step, HKAgent, [ HKAgent, Knows ], HKAgent)

# Or to our facebook dataset

apply!(snapsim, step, HKAgent, [ HKAgent, Knows ], HKAgent)

# # Create plots

# Finally, we show the visualization possibilities for graphs, and import the
# necessary packages for this and create a colormap for the nodes.

import CairoMakie, GraphMakie, NetworkLayout, Colors, Graphs, Makie

# Since the full graph is very cluttered and the Facebook dataset is
# too large, we construct a Clique graph using Graphs.jl.

const cysim = create_simulation(hkmodel, HKParams(0.25), nothing);
const cyids = add_graph!(cysim,
                         SimpleGraphs.clique_graph(7, 8),
                         _ -> HKAgent(rand()),
                         _ -> Knows());


foreach(id -> add_edge!(cysim, id, id, Knows()), cyids) 

finish_init!(cysim);

# Vahana implements an interactive plot function based on GraphMakie, where
# agents and edges are given different colors per type by default, and
# the state of each agent/edge is displayed via mouse hover
# actions.

vp = create_graphplot(cysim)

figure(vp)

# To modify the created plot, the Makie figure, axis and plot, can be
# accessed via the methods `figure`, `axis` and `plot`. This allows us to modify
# the graph layout and to remove the decorations.

Vahana.plot(vp).layout = NetworkLayout.Stress()

Makie.hidedecorations!(axis(vp))

figure(vp)

# We want that nodes to show the agent's opinion. Instead of modifing
# the Makie plot `node_color` property directly, it's also possible to
# define a helper functions with methods for the different agent and
# edge types, that are called by `create_graphplot` to determine
# properties of the nodes and edges of the plot and also supports
# interactive plot in the case that GLMakie is used as Makie
# backend. For details please check [`create_graphplot`](@ref).

# We define such a function and are calling it modify_vis, and will set the
# `update_fn` keyword of `create_graphplot` to this function. 


colors = Colors.range(Colors.colorant"red", stop=Colors.colorant"green", length=100)


modify_vis(state::HKAgent, _ ,_) = Dict(:node_color => colors[state.opinion * 100 |> ceil |> Int],
                                       :node_size => 15)

modify_vis(_::Knows, _, _, _) = Dict(:edge_color => :lightgrey,
                                     :edge_width => 0.5);


# We are using a helper function to modify the node colors to
# indicate the agent's opinion and add a color bar to the plot.

function plot_opinion(sim)
    vp = create_graphplot(cysim,
                          update_fn = modify_vis)
    Vahana.plot(vp).layout = NetworkLayout.Stress()
    Makie.hidedecorations!(axis(vp))
    Makie.Colorbar(figure(vp)[:, 2]; colormap = colors)
    figure(vp)
end;


# And now we can plot the initial state

plot_opinion(cysim)

# And then the state after 500 iterations

for _ in 1:500
    apply!(cysim, step, [ HKAgent ], [ HKAgent, Knows ], [ HKAgent ])
end

plot_opinion(cysim)

# # Finish the simulation

# As always, it is important to call `finish_simulation` at the end of the
# simulation to avoid memory leaks.

finish_simulation!(cysim);
