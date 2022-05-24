import Base.length
import Base.iterate

struct IterEdgesState
    agentiter
    currentagentid
    nextedgeidx::Int64
end

struct IterEdgesWrapper
    field
end

function edges_iterator(field)
    IterEdgesWrapper(field)
end

# function length(iw::IterEdgesWrapper)
#     if length(iw.field) == 0
#         return 0
#     end
    
#     if hasmethod(isassigned, (typeof(iw.field), Int64))
#         field = iw.field
#         ks = keys(field)
#         ks = filter(i -> isassigned(field, i), ks)
        
#         mapreduce(length, +, map(i -> field[i], ks))
#     else
#         mapreduce(length, +, values(iw.field))
#     end
# end
    

function iterate(iw::IterEdgesWrapper)
    if length(iw.field) == 0
        return nothing
    end
    field = iw.field
    ks = keys(field)
    if hasmethod(isassigned, (typeof(field), Int64))
        ks = filter(i -> isassigned(field, i), ks)
    end
    agentiter = Iterators.Stateful(ks)
    currentagentid = Iterators.take(agentiter, 1) |> only
    iterate(iw, IterEdgesState(agentiter, currentagentid, 1))
end


function iterate(iw::IterEdgesWrapper, is::IterEdgesState)
    field = iw.field
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
    currentagentid = Iterators.take(is.agentiter, 1) |> only
    iterate(iw, IterEdgesState(is.agentiter, currentagentid, 1))
end
