export checked_foreach, checked_map, checked_filter

"""
    checked_foreach(f, itr)

Calls foreach(f, itr) but only when itr != nothing.

Example:
    ```checked_foreach(neighborids(sim, id, Contact)) do nid
        add_edge!(sim, id, nid, Inform())
    end```
"""
function checked_foreach(f, itr)
    if !isnothing(itr)
        foreach(f, itr)
    else
        nothing
    end
end

"""
TODO DOC
"""
function checked_map(f, itr)
    if !isnothing(itr)
        map(f, itr)
    else
        nothing
    end
end

"""
TODO DOC
"""
function checked_filter(f, itr)
    if !isnothing(itr)
        filter(f, itr)
    else
        nothing
    end
end

######################################## internal

#################### field symbols
# create symbol for the different fields of an agent/edgetype

writefield(T) = Symbol(T, "_write")

readfield(T) = Symbol(T, "_read")

nextidfield(T) = Symbol(T, "_nextid")

