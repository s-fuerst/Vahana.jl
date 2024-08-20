import DataFrames: DataFrame, subset!, nrow

export DataFrame, GlobalsDataFrame

show_parallel_dataframe_usage_warning = true

"""
    DataFrame(sim::Simulation, T::DataType; types = false, localnr = false)

Creates a DataFrame with the current state of agents or edges of type
`T`.  By default, the ID columns show the complete
[`AgentID`](@ref). for readability (and the use of
[`show_agent`](@ref)) it may be useful to show only the lowest 36
bits, which gives a much more readable value, by setting the `localnr`
argument to true. Since for edges the type information of the source
and target agents is no longer available in this case, the `types`
argument can be used to create additional columns containing the types
of these agents.

!!! info

    `DataFrame` is only available when the DataFrame.jl package is imported
    by the model implementation. 

!!! warning

    In parallel simulations, the `DataFrame` method converts only the
    local graph partition (agents or edges) residing on the current
    MPI rank. It does not provide a global view of the entire
    distributed graph. To obtain a complete list of all agent states
    across all ranks, use the [`all_agents`](@ref) function. Similarly, for
    edges, use the [`all_edges`](@ref) function to get a global view of all
    edges in the distributed graph.

"""
function DataFrame(sim::Simulation, T::DataType; types = false, localnr = false)
    if localnr
        types = true
    end

    if mpi.active && mpi.rank == 0 &&
        show_parallel_dataframe_usage_warning == true &&
        config.quiet == false
        print("""

In a parallel simulation, the `DataFrame(sim...)` function call returns only
the agents or edges corresponding to the local graph partition for each
rank.

This warning is displayed once per Julia session and can be suppressed
by invoking `suppress_warnings(true)` after importing the Vahana package.
    
""")
        global show_parallel_dataframe_usage_warning = false
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
        if localnr
            df.id = map(agent_nr, df.id)
        end
    elseif T in sim.typeinfos.edges_types # Networks
        if sim.initialized
            prepare_read!(sim, [], T)
        end
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
        if types
            if ! has_hint(sim, T, :IgnoreFrom)
                df.from_type = map(id -> tinfos.nodes_id2type[type_nr(id)],
                                   df.from)
            end
            df.to_type = map(id -> tinfos.nodes_id2type[type_nr(id)], df.to)
        end
        if localnr
            df.to = map(agent_nr, df.to)
            if ! has_hint(sim, T, :IgnoreFrom)
                df.from = map(agent_nr, df.from)
            end
        end
        if sim.initialized
            finish_read!(sim, T)
        end
    elseif T == typeof(sim.globals)
        for i in 1:fieldcount(T)
            field = getfield(sim.globals, i)
            if typeof(field) <: Vector
                if nrow(df) > 0 && nrow(df) != length(field)
#                     @rootonly printstyled("""
# Length of :$(fieldname(T,i)) does not match the length of the other vectors, 
# so this field will not be added to the dataframe
# """; color = :red)
                else
                    df[!,fieldname(T,i)] = field
                end
            end
        end
    else
        printstyled("Type $T is neither an agent type nor an edge type\n";
                    color = :red)
    end
    df
end

GlobalsDataFrame(sim::Simulation) = DataFrame(sim, typeof(sim.globals))
