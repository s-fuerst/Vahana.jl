
# # Opinion Model

# In this example we demonstrate how we can use the Graphs.jl package
# to add graphs from other sources (or created by the Graphs.jl
# package itself) to a Vahana simulation.

# For this, we implement a simple version of the [Hegselmann and
# Krause (2002)](http://jasss.soc.surrey.ac.uk/5/3/2.html) opinion
# dynamics model. An alternative implementation of the same model
# using the Agents.jl package can be found
# [here](https://juliadynamics.github.io/Agents.jl/v4.0/examples/hk/).

using Vahana, Statistics, BenchmarkTools

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
    register_edgestatetype!(Knows) |>
    create_model("Hegselmann-Krause");

const sim = create_simulation(hkmodel, HKParams(0.2), nothing);



# using SNAPDatasets
# const agentids = add_graph!(sim,
#                            loadsnap(:facebook_combined),
#                            _ -> HKAgent(rand()),
#                            _ -> Knows());


import Graphs.SimpleGraphs
g = SimpleGraphs.complete_graph(1000)
const agentids = add_graph!(sim,
                            g,
                            _ -> HKAgent(rand()),
                            _ -> Knows());


foreach(id -> add_edge!(sim, id, id, Knows()), agentids) 

finish_init!(sim)

@info mpi.rank sim
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

    accepted = filter(edgestates(sim, id, Knows, HKAgent)) do other
        abs(other.opinion - agent.opinion) < ε
    end
    HKAgent(mean(map(a -> a.opinion, accepted)))
end;

# We can now apply the transition function to the complete graph simulation

copy = copy_simulation(sim)

apply!(sim, step, [ HKAgent ], [ HKAgent, Knows ], [ HKAgent ])

#@time for _ in 1:50 apply!(sim, step, [ HKAgent ], [ HKAgent, Knows ], [ HKAgent ]) end


