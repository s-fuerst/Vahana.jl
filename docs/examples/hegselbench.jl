using Vahana, Statistics

num_steps = 10

graphtype = Symbol(get(ARGS, 1, "complete"))
with_hints = Bool(parse(Int64, get(ARGS, 2, "0")))
num_agents = parse(Int64, get(ARGS, 3, "10000"))

import Graphs.SimpleGraphs

enable_asserts(false)

struct HKAgent
    opinion::Float64
end

struct Knows end

struct HKParams
    ε::Float64
end

const model = if with_hints
    ModelTypes() |>
        register_agenttype!(HKAgent, :Immortal) |>
        register_edgetype!(Knows, :Stateless, :SingleType; target=HKAgent) |>
        create_model("Hegselmann-Krause-Hints")
else
    ModelTypes() |>
        register_agenttype!(HKAgent) |>
        register_edgetype!(Knows) |>
        create_model("Hegselmann-Krause")
end

function construct_sim(model)
    sim = create_simulation(model, HKParams(0.2), nothing);
    
    @rootonly begin
        g = if graphtype == :complete
            SimpleGraphs.complete_graph(num_agents)
        elseif graphtype == :regular
            SimpleGraphs.random_regular_graph(num_agents, 10)
        elseif graphtype == :watts
            SimpleGraphs.watts_strogatz(num_agents, 10, 0.2)
        elseif graphtype == :clique
            SimpleGraphs.clique_graph(10, Int64(num_agents / 10))
        else
            @error "Unknown graphtype"
        end

        agentids = add_graph!(sim,
                              g,
                              _ -> HKAgent(rand()),
                              _ -> Knows());

        foreach(id -> add_edge!(sim, id, id, Knows()), agentids) 
    end

    algo = graphtype == :complete ? :EqualAgentNumbers : :Metis
    
    @rootonly @info "finish_init!"
    if mpi.isroot 
        @time finish_init!(sim; partition_algo = algo)
    else
        finish_init!(sim; partition_algo = :EqualAgentNumbers)
    end

    sim
end

function step(agent, id, sim)
    ε = param(sim, :ε)

    accepted = filter(neighborstates(sim, id, Knows, HKAgent)) do other
        abs(other.opinion - agent.opinion) < ε
    end
    HKAgent(mean(map(a -> a.opinion, accepted)))
end;

function step2(agent, id, sim)
    ε = param(sim, :ε)

    sum::Float64 = 0.0
    count::Int64 = 0
    for other in neighborstates(sim, id, Knows, HKAgent)
        if abs(other.opinion - agent.opinion) < ε
            sum += other.opinion
            count += 1
        end
    end
    HKAgent(sum / count)
end;

function runsim(sim)
    @rootonly @info "start"
    GC.gc(true)
    if mpi.isroot
        apply!(sim, step, [ HKAgent ], [ HKAgent, Knows ], [ HKAgent ])
        @time for _ in 1:num_steps
            apply!(sim, step, [ HKAgent ], [ HKAgent, Knows ], [ HKAgent ])
        end
    else
        apply!(sim, step, [ HKAgent ], [ HKAgent, Knows ], [ HKAgent ])
        for _ in 1:num_steps
            apply!(sim, step, [ HKAgent ], [ HKAgent, Knows ], [ HKAgent ])
        end
    end
    sim
end

@rootonly @info "running" model.name num_agents num_steps mpi.size graphtype

construct_sim(model) |> runsim |> finish_simulation!

#construct_sim(model_withhints, :complete) |> runsim |> finish_simulation!

### Without Vahana
if mpi.size == 1
    if num_agents <= 10000
        @info "Without Vahana complete"
        states = [ rand() for i in 1:num_agents ]
        
        function calcnewstate(i::Int64, states)
            own = states[i]
            accepted = filter(states) do other
                abs(other - own) < 0.2
            end
            mean(accepted)
        end

        function updateall(states)
            new = fill(0.0, num_agents)
            for i in 1:num_agents
                new[i] = calcnewstate(i, states)
            end
            new
        end

        states = updateall(states)
        @time for _ in 1:num_steps
            global states = updateall(states)
        end
    end

    import Graphs
    function calcnewstate(i::Int64, states, g)
        own = states[i]
        fstates = states[Graphs.neighbors(g,i)]
        accepted = filter(fstates) do other
            abs(other - own) < 0.2
        end
        mean(accepted)
    end
    function updateall(states, g)
        new = fill(0.0, num_agents)
        for i in 1:num_agents
            new[i] = calcnewstate(i, states, g)
        end
        new
    end

    @info "Without Vahana regular"
    states = [ rand() for i in 1:num_agents ]
    g = Graphs.SimpleGraphs.random_regular_graph(num_agents, 10)
    for i = 1:num_agents
        Graphs.add_edge!(g, i, i)
    end
    states = updateall(states,g)
    @time for _ in 1:num_steps
        global states = updateall(states,g)
    end

    
    @info "Without Vahana clique"
    states = [ rand() for i in 1:num_agents ]
    g = SimpleGraphs.clique_graph(10, Int64(num_agents / 10))
    for i = 1:num_agents
        Graphs.add_edge!(g, i, i)
    end
    @time for _ in 1:num_steps
        global states = updateall(states,g)
    end
end

