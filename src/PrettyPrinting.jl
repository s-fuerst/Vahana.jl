######################################## <: AbstractEdge

function Base.show(io::IO, mime::MIME"text/plain", edge::StatelessEdge)
    show(io, mime, edge.from)
    print(" -> ")
    show(io, mime, edge.to)
end

function Base.show(io::IO, mime::MIME"text/plain", edge::Edge{T}) where { T }
    show(io, mime, edge.from)
    print(io, " -> ")
    show(io, mime, edge.to)
    print(io, ": ")
    show(io, mime, edge.state)
end 

######################################## Simulation

function Base.show(io::IO, ::MIME"text/plain", sim::Simulation)
    function show_agent_types(io::IO, typeids, coll)
        if length(typeids) >= 1 
            printstyled(io, "Agent Type(s):"; color = :cyan)
            for (k, v) in typeids
                print(io, "\n\t $k (ID: $(typeids[k])) \
                           with $(show_length(coll[v])) Agent(s)")
            end
            println()
        end
    end

    function show_edge_types(io::IO, edges)
        if length(edges) >= 1 
            printstyled(io, "Network Type(s):"; color = :cyan)
            for (k, v) in edges
                print(io, "\n\t $k \
                           with $(show_length(edges[k])) Agent(s)")
            end
            println()
        end
    end
    
    printstyled(io, "Simulation Name: ", sim.name, "\n"; color = :blue)
    println(io, "Parameters: ", sim.params)
    show_agent_types(io, sim.agent_typeids, sim.agents)
    show_edge_types(io, sim.edges)
end

######################################## Collections

function show_collection(io, mime, coll)
    for (k, v) in first(coll, 5)
        show(io, mime, k)
        print(io, " => ")
        show(io, mime, v)
        println()
    end
    if length(coll) > 5
        println("...")
    end
end

function show_length(coll)
    string(length(coll))
end

######################################## Buffered Collections

function show_buffered_collection(io::IO, mime::MIME"text/plain", coll) 
    printstyled(io, typeof(coll), "\n"; color = :blue)
    if length(coll.containers[coll.read]) > 0 
        printstyled(io, "Read:\n"; color = :cyan)
        show_collection(io, mime, coll.containers[coll.read])
    end
    if length(coll.containers[coll.write]) > 0 
        printstyled(io, "Write:\n"; color = :cyan)
        show_collection(io, mime, coll.containers[coll.write])
    end
end

function Base.show(io::IO, mime::MIME"text/plain", bad::BufferedAgentDict{T}) where { T }
    show_buffered_collection(io, mime, bad)
end

function Base.show(io::IO, mime::MIME"text/plain", bed::BufferedEdgeDict{T}) where { T }
    show_buffered_collection(io, mime, bed)
end

function show_buffered_length(coll) 
    cs = coll.containers
    if coll.read != coll.write
        "$(length(cs[coll.read]))/$(length(cs[coll.write])) (R/W)"
    else
        "$(length(cs[coll.read]))"
    end
end

function show_length(coll::BufferedEdgeDict{T}) where { T }
    show_buffered_length(coll)
end

function show_length(coll::BufferedAgentDict{T}) where { T }
    show_buffered_length(coll)
end


