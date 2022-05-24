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

function Base.show(io::IO, ::MIME"text/plain", sim::Simulation)
    function show_agent_types(io::IO, typeids, coll)
        if length(typeids) >= 1 
            printstyled(io, "\nAgent(s):"; color = :cyan)
            for (k, v) in typeids
                print(io, "\n\t Type $k (ID: $(typeids[k])) \
                           with $(show_length(coll[v])) Agent(s)")
            end
        end
    end

    function show_edge_types(io::IO, edges)
        if length(edges) >= 1 
            printstyled(io, "\nNetwork(s):"; color = :cyan)
            for (k, v) in edges
                print(io, "\n\t $k \
                           with $(show_num_edges(v)) Edge(s) for $(show_length(v)) Agent(s)")
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
    show_agent_types(io, sim.agent_typeids, sim.agents)
    show_edge_types(io, sim.edges)
 #   print(io, "Globals: ", sim.globals)
    show_struct(io, sim.globals, "Global")
end

######################################## Collections

function show_collection(coll)
    count = 1
    for (k, v) in edges_iterator(coll)
        show(k)
        print(" => ")
        show(v)
        println()
        count += 1
        if count > 5
            break
        end
    end
    if count > 5
        println("...")
    end
end

function show_network(sim, mime, t::Val{T}) where {T}
    read = _getread(sim, t)
    if length(read) > 0
        printstyled("Read:\n"; color = :cyan)
        show_collection(read)
    end

    write = _getwrite(sim, t)
    if length(write) > 0
        printstyled("Write:\n"; color = :cyan)
        show_collection(write)
    end
end

# function show_length(coll)
#     string(length(coll))
# end

######################################## Buffered Collections


function show_length(sim, ::Val{T}) where {T} 
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

function show_num_edges(sim, ::Val{T}) where {T}
    read = getproperty(sim, readfield(Symbol(T)))
    write = getproperty(sim, writefield(Symbol(T)))
    # cs = coll.containers
    if read != write
        "$(_num_edges(read))/$(_num_edges(write)) (R/W)"
    else
        "$(_num_edges(read))"
    end
end
    
