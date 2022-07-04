export num_edges
export show_network, show_agents, show_agent, show_random_agent

using Printf

# as a convention, we print only "complete" agentid as hex
function _printid(id, nrformatted = true)
    if typeof(id) == UInt32 ||  agent_nr(AgentID(id)) == id
        if nrformatted
            Printf.@printf "%18i" id
        else
            print(id)
        end
    else
        Printf.@printf "0x%016x" id
    end
end    


function show_type(sim, t::Type{T}; write = false, max = 5) where T
    readfield = _getread(sim, t)
    if length(readfield) > 0
        printstyled("Read:\n"; color = :cyan)
        _show_collection(edges_iterator(readfield), max)
    end

    if write
        writefield = _getwrite(sim, t)
        if length(writefield) > 0
            printstyled("Write:\n"; color = :cyan)
            _show_collection(edges_iterator(writefield), max)
        end
    end
end

"""
    show_network(sim, ::Type{T}; write = false, max = 5) 

Display (some of) the edges of type T.

In a parallized simulation, only the edges that are in the partition
of the graph associated with the function calling process are shown.

When the keyword `write` is set to true, also the edges that are added in
the initialization phase or current transition function are shown.

The keyword `max` determine the maximal number of shown edges.
"""
show_network(sim, t::Type{T}; kwargs...) where T = show_type(sim, t; kwargs...)

"""
    show_agents(sim, ::Type{T}) where {T <: Agent}

Display (some of) the agents of the type T.

In a parallized simulation, only the agents that are in the partition
of the graph associated with the function calling process are shown.

When the keyword `write` is set to true, also the agents that are added in
the initialization phase or current transition function are shown.

The keyword `max` determine the maximal number of shown agents.

"""
show_agents(sim, t::Type{T}; kwargs...) where T = show_type(sim, t; kwargs...)

"""
    show_random_agent(sim, ::Type{T}; kwargs...) 

Display detailed information of a random agent with Type T.

The optional arg `nedges` controls the maximal number of edges that are
shown per network (per direction).

The optional `stateof` argument controls whether the state of the edge
(the default) or the state of the adjacent agent (for any value except
:Edge) is displayed.

"""
function show_random_agent(sim, t::Type{T}; kwargs...) where T
    if !sim.initialized
        println("show_random_agent can not be called before finish_init!.")
        return
    end
    
    agents = _getread(sim, t) |>
        keys

    if length(agents) == 0
        println("No agent of type $T found.")
        return
    end

    show_agent(sim, rand(agents), t; kwargs...)
end

"""
    show_agent(sim, id, Type{T}; nedges=5, stateof=:Edge) 

Display detailed information about the agent with ID `id`, or in the
case that id is a value < 2^32, the information of the nth agent
of type T created.

The optional arg `nedges` controls the maximal number of edges that are
shown per network (per direction).

The optional `stateof` argument controls whether the state of the edge
(the default) or the state of the adjacent agent (for any value except
:Edge) is displayed.
"""
function show_agent(sim,
             id,
             t::Type{T};
             nedges=5,
             stateof=:Edge) where T
    # id can be always the local nr, so we first ensure that id
    # is the complete agent_id
    if id <= typemax(AgentNr)
        id = agent_id(sim.typeinfos.nodes_type2id[T], UInt32(id))
    end
    # first we print the id of the agent and the state of the agent
    printstyled("Id / Local Nr: "; color = :cyan)
    _printid(id, false)
    print(" / $(agent_nr(id))")
    as = agentstate(sim, id, t)
    if nfields(as) > 0
        printstyled("\nState:"; color = :cyan)
        fnames = as |> typeof |> fieldnames
        for f in fnames
            printstyled("\n    $f=$(getfield(as, f))")
        end
    end
    # the all the edges for the agent
    printstyled("\nNetwork(s):"; color = :cyan)
    for edgeT in sim.typeinfos.edges_types
        # output the network name and derive some information from sim.typeinfos
        printstyled("\n    $edgeT"; color = :yellow)
        read_container = getproperty(sim, readfield(Symbol(edgeT)))
        edgetypetraits = sim.typeinfos.edges_attr[edgeT][:traits]
        
        justcount = :IgnoreFrom in edgetypetraits && :Stateless in edgetypetraits
        
        if :SingleEdge in edgetypetraits && :SingleAgentType in edgetypetraits && !justcount
            printstyled("\n\tIt is not possible to give reliable information " *
                "about the edges of the agent when the\n\tcorresponding edgetype has the " *
                ":SingleEdge and :SingleAgentType trait combination."; color = :red)
            continue
        end
        # unify the agentid. For vector (:SingleAgent) types, this must be the index,
        # and for dict types, the AgentID
        aid = agent_id(sim.typeinfos.nodes_type2id[T], agent_nr(id))
        nid = :SingleAgentType in edgetypetraits ? agent_nr(id) : aid
        # check that this agent has some edges
        if nid in keys(read_container)
            # for vector types we can have #undef entries. to only something
            # for the agent when it is assigned to the vector 
            if !(:SingleAgentType in edgetypetraits && !isassigned(read_container, nid))
                d = read_container[nid]

                if justcount
                    if :SingleEdge in edgetypetraits
                        printstyled("\n\thas_neighbor:  "; color = :green)
                        print("$(has_neighbor(sim, aid, edgeT))")
                    else
                        printstyled("\n\tnum_neighbors: "; color = :green)
                        print("$(num_neighbors(sim, aid, edgeT))")
                    end
                else
                    # write the header for this edgetype, 
                    printstyled("\n\tfrom:              "; color = :green)
                    if stateof == :Edge || :IgnoreFrom in edgetypetraits
                        printstyled("edge.state:"; color = :green)
                    else
                        printstyled("state of edge.from:"; color = :green)
                    end
                    if :SingleEdge in edgetypetraits
                        _show_edge(sim, d, edgetypetraits, stateof, edgeT)
                    else
                        for e in first(d, nedges)
                            _show_edge(sim, e, edgetypetraits, stateof, edgeT)
                        end
                        if length(d) > nedges
                            println("\n\t... ($(length(d)-nedges) not shown)")
                        end
                    end
                end
            end
        end

        if :IgnoreFrom in edgetypetraits
            if !justcount
                printstyled("\n\tFor edgetypes with the :IgnoreFrom trait " *
                    "the edges to an agent can not be determined."; color = :red)
            end
            continue
        end
        # collect the outgoing edges and overwrite the from id
        # with the to id
        edges_agents = Vector{Edge}()
        for (eid, e) in edges_iterator(read_container)
            edge = _reconstruct_edge(e, edgetypetraits, edgeT)
            if id == edge.from
                push!(edges_agents, Edge(AgentID(eid), edge.state))
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
                _show_edge(sim, ea, [], stateof, edgeT)
            end
            if length(edges_agents) > nedges
                println("\n\t... ($(length(edges_agents)-nedges) not shown)")
            end
        end
    end
end

function num_edges(sim, t::Type{T}) where {T}
    _num_edges(sim, t)
end
