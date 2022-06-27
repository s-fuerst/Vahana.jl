import Base.length
import Base.iterate

# When we iterate over all edges, we have two nested iterators (the outer over
# all the agents, and one over the edges for those agents). 

struct IterEdgesState
    agentiter # the outer agent iterator
    currentagentid # the current agent for the inner edge iterator
    nextedgeidx::Int64 # the next edge of in the inner edge iterator
end

struct IterEdgesWrapper
    field
end

"""
    edges_iterator(field)

(Vahana internal function)

Creates an wrapped type for an %EDGETYPE%_read/write field. For this
type their exists iterate methods, which handle all the edgetype traits.
"""
function edges_iterator(field)
    IterEdgesWrapper(field)
end

function iterate(iw::IterEdgesWrapper)
    if length(iw.field) == 0
        return nothing
    end
    field = iw.field
    ks = keys(field)
    # If the container is a vector, remove all #undefs 
    if hasmethod(isassigned, (typeof(field), Int64))
        ks = filter(i -> isassigned(field, i), ks)
    end
    # create an stateful iterator for the keys (the agentids), which
    # is added the the state of the outer iterator 
    agentiter = Iterators.Stateful(ks)
    currentagentid = Iterators.take(agentiter, 1) |> only
    iterate(iw, IterEdgesState(agentiter, currentagentid, 1))
end


function iterate(iw::IterEdgesWrapper, is::IterEdgesState)
    field = iw.field
    # innerisvec is false, if the edgetype has the trait :SingleEdge
    innerisvec = hasmethod(length, (typeof(field[is.currentagentid]),))
    len = 1
    if innerisvec
        len = length(field[is.currentagentid])
    end
    if is.nextedgeidx <= len
        if innerisvec
            return ((is.currentagentid, field[is.currentagentid][is.nextedgeidx]),
               IterEdgesState(is.agentiter, is.currentagentid, is.nextedgeidx + 1))
        else
            return ((is.currentagentid, field[is.currentagentid]),
               IterEdgesState(is.agentiter, is.currentagentid, is.nextedgeidx + 1))
        end
    elseif length(is.agentiter) == 0
        return nothing
    end
    # this code is only reaced for is.nextedgeidx > len, which means
    # we move on our outer iterator to the next agent and call
    # the iterate function recursivly.
    currentagentid = Iterators.take(is.agentiter, 1) |> only
    iterate(iw, IterEdgesState(is.agentiter, currentagentid, 1))
end
