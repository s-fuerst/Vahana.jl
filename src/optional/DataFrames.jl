import DataFrames: DataFrame, subset!, nrow

export DataFrame

function DataFrame(sim::Simulation, T::DataType; show_types = false, show_agentnr = false)
    if mpi.size > 1
        @info "The created dataframe contains only data from rank $(mpi.rank)"
    end

    df = DataFrame()
    tinfos = sim.typeinfos
    if T in tinfos.nodes_types # Agents
        read = sim.initialized ?
            getproperty(sim, Symbol(T)).read :
            getproperty(sim, Symbol(T)).write 
            
        tid = tinfos.nodes_type2id[T]
        df.id = map(nr -> agent_id(tid, agent_nr(AgentID(nr))),
                     1:length(read.state))
        if fieldcount(T) > 0
            df = hcat(df, DataFrame(read.state))
        end
        if ! has_hint(sim, T, :Immortal, :Agent)
            subset!(df, :id => id -> .! read.died[agent_nr.(id)])
        end
        if show_agentnr
            df.id = map(agent_nr, df.id)
        end
    elseif T in sim.typeinfos.edges_types # Networks
        read = sim.initialized ?
            getproperty(sim, Symbol(T)).read :
            getproperty(sim, Symbol(T)).write 
        # First check the Num/HasNeighborsOnly case
        if has_hint(sim, T, :IgnoreFrom) && has_hint(sim, T, :Stateless)
            df.to = collect(keys(read))
            if has_hint(sim, T, :SingleEdge)
                df.has_edge = map(>(0), values(read))
                subset!(df, :has_edge => b -> b)
            else
                df.num_edges = collect(values(read))
                subset!(df, :num_edges => n -> n .> 0)
            end
        else
            # for the remaining hints we can use the edges iterator
            edges = collect(edges_iterator(sim, T, sim.initialized))
            if ! has_hint(sim, T, :IgnoreFrom)
                if has_hint(sim, T, :Stateless)
                    df.from = last.(edges)
                else    
                    df.from = map(e -> e.from, last.(edges))
                end
            end
            df.to = first.(edges)
            if fieldcount(T) > 0
                if ! has_hint(sim, T, :IgnoreFrom)
                    states = map(e -> e.state, last.(edges))
                    df = hcat(df, DataFrame(states); makeunique = true)
                else
                    df = hcat(df, DataFrame(last.(edges)); makeunique = true)
                end
            end
        end
        # when the SingleType hint is active, we must transform the ids
        # from the vector index to the correct agent id
        if has_hint(sim, T, :SingleType)
            totype = tinfos.edges_attr[T][:target]
            totypeid = tinfos.nodes_type2id[totype]
            df.to = map(id -> agent_id(totypeid, AgentNr(id)), df.to)
            # if ! has_hint(sim, T, :IgnoreFrom)
            #     df.from = map(id -> agent_id(totypeid, AgentNr(id)), df.from)
            # end
        end
        if show_types
            if ! has_hint(sim, T, :IgnoreFrom)
                df.from_type = map(id -> tinfos.nodes_id2type[type_nr(id)],
                                   df.from)
            end
            df.to_type = map(id -> tinfos.nodes_id2type[type_nr(id)], df.to)
        end
        if show_agentnr
            df.to = map(agent_nr, df.to)
            if ! has_hint(sim, T, :IgnoreFrom)
                df.from = map(agent_nr, df.from)
            end
        end
    elseif T == typeof(sim.globals)
        for i in 1:fieldcount(T)
            field = getfield(sim.globals, i)
            if typeof(field) <: Vector
                if nrow(df) > 0 && nrow(df) != length(field)
                    printstyled("""
Length of :$(fieldname(T,i)) does not match the length of the other vectors, 
so this field will not be added to the dataframe"""; color = :red)
                else
                    df[!,fieldname(T,i)] = field
                end
            end
        end
    else
        printstyled("Type $T is neither an agent type nor an edge nor the global type\n";
                    color = :red)
    end
    df
end
