export checked_foreach, checked_map, checked_filter

"""
TODO DOC
"""
function checked_foreach(f, cont)
    if !isnothing(cont)
        foreach(f, cont)
    end
end

"""
TODO DOC
"""
function checked_map(f, cont)
    if !isnothing(cont)
        map(f, cont)
    end
end

"""
TODO DOC
"""
function checked_filter(f, cont)
    if !isnothing(cont)
        map(f, cont)
    end
end
