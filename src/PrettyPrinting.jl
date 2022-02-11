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
    function show_types(io::IO, typeids, coll, name)
        function num_elements(coll::Vector{AgentCollection}, tnr)
            length(coll[tnr])
        end
        
        function num_elements(coll::Vector{EdgeCollection}, tnr)
            if length(coll[tnr]) > 0
                [ length(v) for (_, v) in coll[tnr] ] |> sum
            else 
                0
            end
        end
        
        len = length(typeids)
        if len == 1 
            printstyled(io, "$name Type: "; color = :cyan)
            (k, v) = first(typeids)
            print(io, "$k (ID: $(typeids[k])) \
                       with $(num_elements(coll, v)) $(name)(s)\n")
        elseif len > 1 
            printstyled(io, "$name Types:"; color = :cyan)
            for (k, v) in typeids
                print(io, "\n\t $k (ID: $(typeids[k])) \
                           with $(num_elements(coll, v)) $(name)(s)")
            end
            println()
        end
    end
    
    printstyled(io, "Simulation Name: ", sim.name, "\n"; color = :blue)
    println(io, "Parameters: ", sim.params)
    show_types(io, sim.agent_typeids, sim.agents, "Agent")
    show_types(io, sim.edge_typeids, sim.edges, "Edge")
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
