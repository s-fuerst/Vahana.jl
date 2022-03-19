export num_edges
export show_network, show_agents, show_agent, show_random_agent

"""
    show_network(sim, ::Type{T}) where {T <: Agent}

Print (some of) the edges of  type T.

In a parallized simulation, only the edges that are in the partition
of the graph associated with the function calling process are shown.

"""
show_network(sim, ::Type{T}) where {T} =
    show(stdout, MIME"text/plain"(), sim.edges[T])

"""
    show_agents(sim, ::Type{T}) where {T <: Agent}

Print (some of) the agents of the type T.

In a parallized simulation, only the agents that are in the partition
of the graph associated with the function calling process are shown.
"""
show_agents(sim, ::Type{T}) where {T <: Agent} =
    show(stdout, MIME"text/plain"(), sim.agents[typeid(sim, T)])


function show_random_agent(sim, ::Type{T}) where {T<:Agent}
    sim.agents[typeid(sim,T)] |> read_container |> keys |> rand |> show_agent(sim)
end


function show_agent(sim, id::AgentID)
    printstyled("Id: $(id)\nState: $(agentstate(sim, id))"; color = :magenta)
    printstyled("\nNetwork(s) (Agent at head position):"; color = :cyan)
    for (k, v) in sim.edges
        if id in read_container(v) |> keys
            d = read_container(v)[id]
            printstyled("\n    $k"; color = :yellow)
            for e in first(d, 5)
                print("\n\t$(e)")
            end
            if length(d) > 5
                println("\n\t... ($(length(d)-5) not shown)")
            end
        end
    end
    # printstyled("\nNetworks (Agent at tail position):"; color = :cyan)
    # for (k, v) in sim.edges
    #     for d in read_container(v) |> keys
    #         if id in read_container(v) |> keys
    #             d = read_container(v)[id]
    #             printstyled("\n    $k"; color = :yellow)
    #             for e in first(d, 5)
    #                 print("\n\t$(e)")
    #             end
    #             if length(d) > 5
    #                 println("\n\t... ($(length(d)-5) not shown)")
    #             end
    #         end
    #     end
    # end
end

show_agent(sim) = id -> show_agent(sim, id)

function num_edges(sim::Simulation, ::Type{T}) where {T} 
    _num_edges(sim.edges[T] |> read_container)
end
