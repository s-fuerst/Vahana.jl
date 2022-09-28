edgestorage_type(T, info) = Vector{Vector{Tuple{AgentID, Edge{T}}}}

edgestorage_constructor(T, info) = Vector{Vector{Tuple{AgentID, Edge{T}}}}()


function construct_sendedge_functions(T::DataType, typeinfos, simsymbol)
    stateless = fieldcount(T) == 0

    # outer vector is rank, this vector has size mpi.size
    
    # TODO?: better name would be init_agentcontainer! 
    @eval function init_storage!(sim::$simsymbol, ::Type{$T})
        @storage($T) = [ Vector{Tuple{AgentID, Edge{$T}}}() for _ in 1:mpi.size ]
    end
 end
