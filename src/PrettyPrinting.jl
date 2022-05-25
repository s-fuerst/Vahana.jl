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
                try 
                    print(io, "\n\t Type $t \
                               with $(_show_num_edges(sim, Val(t))) Edges(s) for $(_show_length(sim, Val(t))) Agent(s)")
                catch  e
                    print(io, "\n\t Type $t \
                               (for fixed sized agent types the actual numbers are unknown)")
                end
            end
            # if length(edges) >= 1 
            #     printstyled(io, "\nNetwork(s):"; color = :cyan)
            #     for (k, v) in edges
            #         print(io, "\n\t $k \
            #                with $(show_num_edges(v)) Edge(s) for $(show_length(v)) Agent(s)")
            #     end
            # end
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

# we assume that :IgnoreFrom ist not set, as we can not construct
# the edge in this case
function _reconstruct_edge(e, edgetypeprops, edgeT)
    if :Stateless in edgetypeprops
        Edge(e, edgeT())
    else
        e
    end
end        

function _show_edge(e, edgetypeprops, stateof, edgeT) 
    print("\n\t")
    if :Ignorefrom in edgetypeprops
        print("unknown           ")
    elseif :Stateless in edgetypeprops
        _printid(e)
    else
        _printid(e.from)
    end
    if stateof == :Edge || :IgnoreFrom in edgetypeprops
        if :Stateless in edgetypeprops
            print(" $edgeT()")
        else
            print(" $(e.state)")
        end
    else
        if :Stateless in edgetypeprops
            print(" $(agentstate_flexible(sim, e))")
        else
            print(" $(agentstate_flexible(sim, e.from))")
        end
    end
    
end

######################################## Buffered Collections


function _show_length(sim, ::Val{T}) where {T} 
    read = getproperty(sim, readfield(Symbol(T)))
    write = getproperty(sim, writefield(Symbol(T)))
    if read != write
        "$(length(read))/$(length(write)) (R/W)"
    else
        "$(length(read))"
    end
end

function _num_edges(coll)
    if length(coll) > 0
        mapreduce(length, +, values(coll))
    else
        0
    end
end

function _show_num_edges(sim, ::Val{T}) where {T}
    read = getproperty(sim, readfield(Symbol(T)))
    write = getproperty(sim, writefield(Symbol(T)))
    # cs = coll.containers
    if read != write
        "$(_num_edges(read))/$(_num_edges(write)) (R/W)"
    else
        "$(_num_edges(read))"
    end
end

