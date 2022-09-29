# as in the edge we do not store the to-id, we use the Tuple to attach this
# information
function edgestorage_type(T, info) 
    type_strings = construct_types(T, info)
    CE = Meta.parse(type_strings[3]) |> eval
    Vector{Vector{Tuple{AgentID, CE}}}
end

function edgestorage_constructor(T, info)
    type_strings = construct_types(T, info)
    CE = Meta.parse(type_strings[3]) |> eval
    Vector{Vector{Tuple{AgentID, CE}}}()
end

function construct_sendedge_functions(T::DataType, typeinfos, simsymbol)
    stateless = fieldcount(T) == 0

    # outer vector is rank, this vector has size mpi.size
    
    # TODO?: better name would be init_agentcontainer! 
    @eval function init_storage!(sim::$simsymbol, ::Type{$T})
        @storage($T) = [ Vector{Tuple{AgentID, Edge{$T}}}() for _ in 1:mpi.size ]
    end

    @eval function send_storage!(sim::$simsymbol, ::Type{$T})
        
    end
 end



#function send_storage!(sim, T::DataType)
    
