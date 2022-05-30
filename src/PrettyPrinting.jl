_getread(sim, ::Val{T}) where {T} = getproperty(sim, readfield(Symbol(T)))
_getwrite(sim, ::Val{T}) where {T} = getproperty(sim, writefield(Symbol(T)))

######################################## <: EdgeState

function Base.show(io::IO, mime::MIME"text/plain", edge::Edge{T}) where {T}
    show(io, mime, edge.from)
    if fieldnames(T) != ()
        print(io, ": ")
        show(io, mime, edge.state)
    end
end 

######################################## Simulation

function construct_prettyprinting_functions()
    @eval function Base.show(io::IO, ::MIME"text/plain", sim::Simulation)
        function show_agent_types(io::IO, sim)
            nodes_types = sim.typeinfos.nodes_types
            if length(nodes_types) >= 1
                printstyled(io, "\nAgent(s):"; color = :cyan)
            end
            for t in nodes_types
                print(io, "\n\t Type $t \
                           with $(_show_length(sim, Val(t))) Agent(s)")
            end
        end

        function show_edge_types(io::IO, sim)
            edges_types = sim.typeinfos.edges_types
            if length(edges_types) >= 1
                printstyled(io, "\nNetworks(s):"; color = :cyan)
            end
            for t in edges_types
                edgetypeprops = sim.typeinfos.edges_attr[Symbol(t)][:props]
                if (:SingleEdge in edgetypeprops &&
                    :SingleAgentType in edgetypeprops) ||
                    (:SingleAgentType in edgetypeprops &&
                    :size in keys(sim.typeinfos.edges_attr[Symbol(t)]))
                    print(io, "\n\t Type $t") 
                else 
                    print(io, "\n\t Type $t \
                               with $(_show_num_edges(sim, Val(t))) Edges(s) for $(_show_length(sim, Val(t))) Agent(s)")
                end
            end
        end

        function show_struct(io::IO, s, name)
            if nfields(s) >= 1 
                printstyled(io, "\n$(name)(s):"; color = :cyan)
                for k in typeof(s) |> fieldnames
                    print(io, "\n\t $k: $(getfield(s, k))")
                end
            end
        end
        
        printstyled(io, "Simulation Name: ", sim.name; color = :magenta)
        show_struct(io, sim.params, "Parameter")
        show_agent_types(io, sim)
        show_edge_types(io, sim)
        #   print(io, "Globals: ", sim.globals)
        show_struct(io, sim.globals, "Global")
    end
end

######################################## Collections


function _show_collection(iter, max)
    count = 1
    for (k, v) in iter
        _printid(k)
        print(" => ")
        println(v)
        count += 1
        if count > max
            break
        end
    end
    if count > max
        println("...")
    end
end

# In the :IgnoreFrom case we set edge.from to 0.
function _reconstruct_edge(e, edgetypeprops, edgeT)
    if :Stateless in edgetypeprops
        if :IgnoreFrom in edgetypeprops
            Edge(AgentID(0), edgeT())
        else
            Edge(e, edgeT())
        end
    elseif :IgnoreFrom in edgetypeprops
        Edge(AgentID(0), e)
    else
        e
    end
end        

function _show_edge(sim, e, edgetypeprops, stateof, edgeT)
    e = _reconstruct_edge(e, edgetypeprops, edgeT)
    if !(:IgnoreFrom in edgetypeprops) && e.from == 0
        return
    end
    print("\n\t")
    if :IgnoreFrom in edgetypeprops
        print("unknown           ")
    else
        _printid(e.from)
    end
    if stateof == :Edge || :IgnoreFrom in edgetypeprops
        print(" $(e.state)")
    else
        # for the to edges we get an empty edgetype props, as we constructed
        # those edges they don't have the same properties
        # But here we need the original props, so we access them directly
        if :SingleAgentType in sim.typeinfos.edges_attr[Symbol(edgeT)][:props]
            agentT = sim.typeinfos.edges_attr[Symbol(edgeT)][:to_agenttype]
            aid = agent_id(sim.typeinfos.nodes_type2id[agentT], agent_nr(e.from))
            print(" $(agentstate(sim, aid, Val(agentT)))")
        else
            print(" $(agentstate_flexible(sim, e.from)))")
        end
    end
end

######################################## Buffered Collections


function _show_length(sim, ::Val{T}) where {T} 
    read = getproperty(sim, readfield(Symbol(T)))
    write = getproperty(sim, writefield(Symbol(T)))
    if !sim.initialized 
        "$(length(read))/$(length(write)) (R/W)"
    else
        "$(length(read))"
    end
end

function _show_num_edges(sim, t::Val{T}) where {T}
    if !sim.initialized 
        "$(_num_edges(sim, t))/$(_num_edges(sim, t, true)) (R/W)"
    else
        "$(_num_edges(sim, t))"
    end
end

