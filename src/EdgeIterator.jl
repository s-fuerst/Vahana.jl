import Base.length

import Base.iterate

# When we iterate over all edges, we have two nested iterators (the outer over
# all the agents, and one over the edges for those agents). 

struct IterEdgesState{IT}
    agentiter::IT # the outer agent iterator
    currentagentid::AgentID # the current agent for the inner edge iterator
    nextedgeidx::Int64 # the next edge of in the inner edge iterator
    length::Int64
end

struct IterEdgesWrapper{SIM, FT, T} 
    sim::SIM
    read::Bool
    field::FT
end

function length(iw::IterEdgesWrapper{SIM, FT, T}) where {SIM, FT, T}
    _num_edges(iw.sim, T, ! iw.read)
end

function construct_edges_iter_methods(T::DataType, attr, simsymbol, FT)
    ignorefrom = :IgnoreFrom in attr[:hints]
    singleedge = :SingleEdge in attr[:hints]
    singletype = :SingleType in attr[:hints]
    stateless = :Stateless in attr[:hints]

    # for whatever reason, we cannot just use the field type in the
    # :SingleType case
    if singletype
        IT = Base.Iterators.Stateful
    else
        IT = Base.Iterators.Stateful{Base.KeySet{UInt64, FT},
                                     Union{Nothing, Tuple{UInt64, Int64}}}
    end
    
    # for the singletype case we can access the type of the agent via AT
    if singletype
        AT = attr[:target]
    end
    
    @eval function edges_iterator(sim::$simsymbol, ::Type{$T}, r::Bool = true)
        @assert ! ($stateless && $ignorefrom)

        field = r ? @edgeread($T) : @edgewrite($T)
        if length(field) == 0
            # we can not return nothing, but an empty vector will return nothing when
            # iterate is called on it
            []
        else
            IterEdgesWrapper{$simsymbol, $FT, $T}(sim, r, field)
        end
    end

    @eval function iterate(iw::IterEdgesWrapper{$simsymbol, $FT, $T})
        if length(iw.field) == 0
            return nothing
        end
        field = iw.field
        ks = keys(field)
        # If the agent container is a vector, remove all #undefs
        if $singletype
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
        len = $singleedge ? 1 : length(field[currentagentid])
        iterate(iw, IterEdgesState{$IT}(agentiter, currentagentid, 1, len))
    end

    @eval function iterate(iw::IterEdgesWrapper{$simsymbol, $FT, $T}, is::IterEdgesState{$IT})
        field = iw.field
        # innerisvec is false, if the edgetyspe has the hint :SingleEdge
        if is.nextedgeidx <= is.length
            if $singleedge
                return ((is.currentagentid, field[is.currentagentid]),
                   IterEdgesState{$IT}(is.agentiter,
                                       is.currentagentid,
                                       2,
                                       1))
            else
                return ((is.currentagentid, field[is.currentagentid][is.nextedgeidx]),
                   IterEdgesState{$IT}(is.agentiter,
                                       is.currentagentid,
                                       is.nextedgeidx + 1,
                                       is.length))
            end
            # this code is only reached for is.nextedgeidx > len, which means
            # we move on our outer iterator to the next agent and call
            # the iterate function recursivly.
        elseif isempty(is.agentiter)
            return nothing
        end
        currentagentid = Iterators.take(is.agentiter, 1) |> only
        len = $singleedge ? 1 : length(field[currentagentid])
        iterate(iw, IterEdgesState{$IT}(is.agentiter, currentagentid, 1, len))
    end
end

