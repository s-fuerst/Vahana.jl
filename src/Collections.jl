

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


function show_buffered_collection(io::IO, mime::MIME"text/plain", coll) where { T }
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

