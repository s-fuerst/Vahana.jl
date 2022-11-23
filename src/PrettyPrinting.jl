######################################## <: EdgeState

function Base.show(io::IO, mime::MIME"text/plain", edge::Edge{T}) where {T}
    show(io, mime, edge.from)
    if fieldnames(T) != ()
        print(io, ": ")
        show(io, mime, edge.state)
    end
end 

######################################## Simulation

function construct_prettyprinting_functions(simsymbol)
    @eval function Base.show(io::IO, ::MIME"text/plain", sim::$simsymbol)
        function show_agent_types(io::IO, sim)
            nodes_types = sim.typeinfos.nodes_types
            if length(nodes_types) >= 1
                printstyled(io, "\nAgent(s):"; color = :cyan)
            end
            for t in nodes_types
                print(io, "\n\t Type $t \
                           with $(_show_num_agents(sim, t)) Agent(s)")
            end
        end

        function show_edge_types(io::IO, sim)
            edges_types = sim.typeinfos.edges_types
            if length(edges_types) >= 1
                printstyled(io, "\nNetworks(s):"; color = :cyan)
            end
            for t in edges_types
                edgetypetraits = sim.typeinfos.edges_attr[t][:traits]
                if (:SingleEdge in edgetypetraits &&
                    :SingleAgentType in edgetypetraits) ||
                    (:SingleAgentType in edgetypetraits &&
                    :size in keys(sim.typeinfos.edges_attr[t]))
                    print(io, "\n\t Type $t") 
                else
                    print(io, "\n\t Type $t \
                               with $(_show_num_edges(sim, t)) Edges(s)")
                    if ! (:SingleAgentType in edgetypetraits)
                        print(io, " for $(_show_num_a_with_e(sim, t)) Agent(s)")
                    end
                end
            end
        end

        function show_raster(io::IO, s)
            if length(s.rasters) >= 1 
                printstyled(io, "\nRaster(s):"; color = :cyan)
                for (k,v) in sim.rasters
#                    print(io, "\n\t $k: $(getfield(s, k))")
                    print(io, "\n\t :$k with dimension $(size(v))")
                end
            end
        end

        
        function show_struct(io::IO, s, name)
            if nfields(s) >= 1 
                printstyled(io, "\n$(name)(s):"; color = :cyan)
                for k in typeof(s) |> fieldnames
                    f = getfield(s, k)
                    if typeof(f) <: Array
                         print(io, "\n\t :$k |> last: $(last(f)) (length: $(length(f)))")
                        printstyled(io, " "; color = :yellow)
                    else
                        print(io, "\n\t :$k : $f")
                    end
                end
            end
        end
        
        printstyled(io, "Model Name: ", sim.modelname; color = :magenta)
        if sim.modelname != sim.name
            printstyled(io, "\nSimulation Name: ", sim.name; color = :magenta)
        end
        show_struct(io, sim.params, "Parameter")
        show_agent_types(io, sim)
        show_edge_types(io, sim)
        show_raster(io, sim)
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
function _reconstruct_edge(e, edgetypetraits, edgeT)
    if :Stateless in edgetypetraits
        if :IgnoreFrom in edgetypetraits
            Edge(AgentID(0), edgeT())
        else
            Edge(e, edgeT())
        end
    elseif :IgnoreFrom in edgetypetraits
        Edge(AgentID(0), e)
    else
        e
    end
end        

function _show_edge(sim, e, edgetypetraits, stateof, edgeT)
    e = _reconstruct_edge(e, edgetypetraits, edgeT)
    if !(:IgnoreFrom in edgetypetraits) && e.from == 0
        return
    end
    print("\n\t")
    if :IgnoreFrom in edgetypetraits
        print("unknown           ")
    else
        _printid(e.from)
    end
    if stateof == :Edge || :IgnoreFrom in edgetypetraits
        print(" $(e.state)")
    else
        # for the to edges we get an empty edgetype traits, as we constructed
        # those edges they don't have the same traits
        # But here we need the original traits, so we access them directly
        if :SingleAgentType in sim.typeinfos.edges_attr[edgeT][:traits]
            agentT = sim.typeinfos.edges_attr[edgeT][:to_agenttype]
            aid = agent_id(sim, agent_nr(e.from), agentT)
            # We disable assertions to call agentstate from the REPL
            # but this is only done temporary and in a newer world age
            print(" $(Base.invokelatest(agentstate, sim, aid, agentT))")
        else
            print(" $(Base.invokelatest(agentstate_flexible, sim, e.from))")
        end
    end
end


######################################## 

function _show_num_a_with_e(sim, ::Type{T}) where T
    if !sim.initialized 
        "$(length(read(sim, T)))/$(length(write(sim, T))) (R/W)"
    else
        "$(length(read(sim, T)))"
    end
end

function _show_num_agents(sim, t::Type{T}) where {T}
    if !sim.initialized 
        "$(num_agents(sim, t))/$(num_agents(sim, t; write = true)) (R/W)"
    else
        "$(num_agents(sim, t))"
    end
end


function _show_num_edges(sim, t::Type{T}) where {T}
    if !sim.initialized 
        "$(num_edges(sim, t))/$(num_edges(sim, t; write = true)) (R/W)"
    else
        "$(num_edges(sim, t))"
    end
end

