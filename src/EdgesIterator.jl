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
    sim
    edgetype::DataType
    read::Bool
    field
    singleedge::Bool # the inner container is not a vector
end

"""
    edges_iterator(sim, edgetype)

(Vahana internal function)

Creates an wrapped type for an %EDGETYPE%_read/write field. This type 
implements the Iterator interface.
"""
function edges_iterator(sim, t::DataType, read::Bool = true)
    @assert !(has_trait(sim, t, :Stateless) &&
        has_trait(sim, t, :IgnoreFrom)) 

    field = read ? _getread(sim, t) : _getwrite(sim, t)
    if length(field) == 0
        # for empty field, we can not detect singleedge, but this is also not
        # necessary as the iteratore will return nothing immediately
        IterEdgesWrapper(sim, t, read, field, false)
    else
        IterEdgesWrapper(sim, t, read, field,
                         :SingleEdge in sim.typeinfos.edges_attr[t][:traits])
    end
end

function length(iw::IterEdgesWrapper)
    _num_edges(iw.sim, iw.edgetype, ! iw.read)
end

function iterate(iw::IterEdgesWrapper)
    if length(iw.field) == 0
        return nothing
    end
    field = iw.field
    ks = keys(field)
    # If the agent container is a vector, remove all #undefs 
    if hasmethod(isassigned, (typeof(field), Int64))
        ks = filter(i -> isassigned(field, i), ks)
    end
    # in the case that no key is left, we also return immediately
    if length(ks) == 0
        return nothing
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
    len = iw.singleedge ? 1 : length(field[is.currentagentid])
    if is.nextedgeidx <= len
        if iw.singleedge
            return ((is.currentagentid, field[is.currentagentid]),
               IterEdgesState(is.agentiter, is.currentagentid, is.nextedgeidx + 1))
        else
            return ((is.currentagentid, field[is.currentagentid][is.nextedgeidx]),
               IterEdgesState(is.agentiter, is.currentagentid, is.nextedgeidx + 1))
        end
    # this code is only reaced for is.nextedgeidx > len, which means
    # we move on our outer iterator to the next agent and call
    # the iterate function recursivly.
    elseif length(is.agentiter) == 0
        return nothing
    end
    currentagentid = Iterators.take(is.agentiter, 1) |> only
#    @show length(is.agentiter), currentagentid    
    iterate(iw, IterEdgesState(is.agentiter, currentagentid, 1))
end
