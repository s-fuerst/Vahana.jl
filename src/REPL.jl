export num_edges
export show_network, show_agents, show_agent, show_random_agent

using Printf

"""
    show_network(sim, ::Type{T}) where {T <: Agent}

Display (some of) the edges of  type T.

In a parallized simulation, only the edges that are in the partition
of the graph associated with the function calling process are shown.

"""
show_network(sim, ::Val{T}) where {T} =
    show_network(sim, MIME"text/plain"(), Val(T))

"""
    show_agents(sim, ::Type{T}) where {T <: Agent}

Display (some of) the agents of the type T.

In a parallized simulation, only the agents that are in the partition
of the graph associated with the function calling process are shown.
"""
show_agents(sim, ::Type{T}) where {T <: Agent} =
    show(stdout, MIME"text/plain"(), sim.agents[typeid(sim, T)])


"""
    show_random_agent(sim, ::Type{T}; kwargs...) 

Display detailed information of a random agent with Type T.

The optional arg `nedges` controls the maximal number of edges that are
shown per network (per direction).

The optional `stateof` argument controls whether the state of the edge
(the default) or the state of the adjacent agent (for any value except
:Edge) is printed.

"""
function show_random_agent(sim, ::Type{T}; kwargs...) where {T<:Agent}
    agent = sim.agents[typeid(sim,T)] |>
        read_container |>
        keys |>
        rand

    show_agent(sim, agent; kwargs...)
end


"""
    show_agent(sim, nth, ::Type{T}; kwargs...) 

Display detailed information about the `nth` created agent of type `T`.

The optional arg `nedges` controls the maximal number of edges that are
shown per network (per direction).

The optional `stateof` argument controls whether the state of the edge
(the default) or the state of the adjacent agent (for any value except
:Edge) is printed.
"""
function show_agent(sim, nth, ::Type{T}; kwargs...) where {T<:Agent}
    show_agent(sim,
               agent_id(typeid(sim, T), UInt32(nth));
               kwargs...
                   )
end

"""
    show_agent(sim, id::AgentID; nedges=5, stateof=:Edge) 

Display detailed information about the agent with ID `id`.

The optional arg `nedges` controls the maximal number of edges that are
shown per network (per direction).

The optional `stateof` argument controls whether the state of the edge
(the default) or the state of the adjacent agent (for any value except
:Edge) is printed.
"""
function show_agent(sim,
             id::AgentID;
             nedges=5,
             stateof=:Edge)
    printstyled("Id: "; color = :cyan)
    Printf.@printf "0x%016x" id
    as = agentstate(sim,id)
    if nfields(as) > 0
        printstyled("\nState:"; color = :cyan)
        fnames = as |> typeof |> fieldnames
        for f in fnames
            printstyled("\n    $f=$(getfield(as, f))")
        end
    end
    printstyled("\nNetwork(s):"; color = :cyan)
    for (k, v) in sim.edges
        printstyled("\n    $k"; color = :yellow)
        if id in read_container(v) |> keys
            d = read_container(v)[id]

            if length(d) > 0
                printstyled("\n\tfrom:              "; color = :green)
                if stateof==:Edge
                    printstyled("edge.state:"; color = :green)
                else
                    printstyled("state of edge.from:"; color = :green)
                end
                for e in first(d, nedges)
                    Printf.@printf "\n\t0x%016x" e.from
                    if stateof==:Edge 
                        print(" $(e.state)")
                    else
                        print(" $(agentstate(sim, e.from))")
                    end
                end
                if length(d) > nedges
                    println("\n\t... ($(length(d)-nedges) not shown)")
                end
            end
        end
        # collect the outgoing edges and the IDs of the to agent
        # as this isn't stored in the edge itself
        edges_agents = Vector{Tuple{Edge, AgentID}}()
        containers = map(read_container, all_agentcolls(sim))
        for cont in containers
            for (a, _) in cont
                for e in edges_to(sim, a, k) 
                    if id == e.from
                        #                            print("push")
                        push!(edges_agents, (e,a))
                    end
                end   
            end
        end

        if length(edges_agents) > 0
            printstyled("\n\tto:                "; color = :green)
            if stateof==:Edge
                printstyled("edge.state:"; color = :green)
            else
                printstyled("state of edge.to:"; color = :green)
            end
            for ea in first(edges_agents, nedges)
                Printf.@printf "\n\t0x%016x" ea[2]
                if stateof==:Edge 
                    print(" $(ea[1].state)")
                else
                    print(" $(agentstate(sim, ea[2]))")
                end
            end
            if length(edges_agents) > nedges
                println("\n\t... ($(length(edges_agents)-nedges) not shown)")
            end
        end
    end
end

function num_edges(sim::Simulation, ::Type{T}) where {T} 
    _num_edges(sim.edges[T] |> read_container)
end
