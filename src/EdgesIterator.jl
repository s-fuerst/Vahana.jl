import Base.length

import Base.iterate

# When we iterate over all edges, we have two nested iterators (the outer over
# all the agents, and one over the edges for those agents). 

struct IterEdgesState
    agentiter # the outer agent iterator
    currentagentid # the current agent for the inner edge iterator
    nextedgeidx::Int64 # the next edge of in the inner edge iterator
end

# TODO: maybe we can also specify field
struct IterEdgesWrapper{SIM, T} 
    sim::SIM
    read::Bool
    field
end

function length(iw::IterEdgesWrapper{SIM, T}) where {SIM, T}
    _num_edges(iw.sim, T, ! iw.read)
end

function construct_edges_iter_functions(T::DataType, attr, simsymbol)
    ignorefrom = :IgnoreFrom in attr[:traits]
    singleedge = :SingleEdge in attr[:traits]
    singletype = :SingleAgentType in attr[:traits]
    stateless = :Stateless in attr[:traits]
    
    # for the singletype case we can access the type of the agent via AT
    if singletype
        AT = attr[:to_agenttype]
    end

    @eval function edges_iterator(sim::$simsymbol, t::Type{$T}, r::Bool = true)
        @assert ! ($stateless && $ignorefrom)

        field = r ? @read($T) : @write($T)
        if length(field) == 0
            # we can not return nothing, but an empty vector will return nothing when
            # iterate is called on it
            []
        else
            IterEdgesWrapper{$simsymbol, $T}(sim, r, field)
        end
    end

    @eval function iterate(iw::IterEdgesWrapper{$simsymbol, $T})
        if length(iw.field) == 0
            return nothing
        end
        field = iw.field
        ks = keys(field)
        # If the agent container is a vector, remove all #undefs
        # if hasmethod(isassigned, (typeof(field), Int64))
        @info "before" ks
        if $singletype
            ks = filter(i -> isassigned(field, i), ks)
        end
        @info "after" ks length(ks)
        # in the case that no key is left, we also return immediately
        if length(ks) == 0
            @info "return nothing"
            return nothing
        end
        @info "behind nothing"
        # create an stateful iterator for the keys (the agentids), which
        # is added the the state of the outer iterator 
        agentiter = Iterators.Stateful(ks)
        currentagentid = Iterators.take(agentiter, 1) |> only
        iterate(iw, IterEdgesState(agentiter, currentagentid, 1))
    end

    @eval function iterate(iw::IterEdgesWrapper{$simsymbol, $T}, is::IterEdgesState)
        field = iw.field
        # innerisvec is false, if the edgetype has the trait :SingleEdge
        len = $singleedge ? 1 : length(field[is.currentagentid])
        if is.nextedgeidx <= len 
            if $singleedge
                return ((is.currentagentid, field[is.currentagentid]),
                   IterEdgesState(is.agentiter, is.currentagentid, is.nextedgeidx + 1))
            else
                return ((is.currentagentid, field[is.currentagentid][is.nextedgeidx]),
                   IterEdgesState(is.agentiter, is.currentagentid, is.nextedgeidx + 1))
            end
            # this code is only reached for is.nextedgeidx > len, which means
            # we move on our outer iterator to the next agent and call
            # the iterate function recursivly.
        elseif length(is.agentiter) == 0
            return nothing
        end
        currentagentid = Iterators.take(is.agentiter, 1) |> only
        iterate(iw, IterEdgesState(is.agentiter, currentagentid, 1))
    end
end

