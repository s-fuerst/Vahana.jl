export ModelTypes
export add_agenttype!, add_edgetype!

# TODO DOC
Base.@kwdef struct ModelTypes
    edges = Dict{Symbol, Symbol}()
    edges_attr = Dict{Symbol, Dict{Symbol, Any}}()
    edges_types = Vector{DataType}()
    nodes = Dict{Symbol, Symbol}()
    nodes_attr = Dict{Symbol, Dict{Symbol, Any}}()
    nodes_types = Vector{DataType}()
    nodes_type2id::Dict{DataType, TypeID} = Dict{DataType, TypeID}()
    nodes_id2type::Vector{DataType} = Vector{DataType}(undef, typemax(TypeID))
end

"""
    add_agenttype!(types, ::Type{T}, ::Type{C}; size) where { T, C }

TODO DOC

Register an additional agent type to `sim`. 

An agent type is an struct that define the state for agents of type `T`.
These structs must be a subtype of `Agent`, and also bits
types, meaning the type is immutable and contains only primitive types
and other bits types.

Can only be called before [`finish_init!`](@ref)

See also [`add_agent!`](@ref) and [`add_agents!`](@ref) 
"""
function add_agenttype!(types::ModelTypes, ::Type{T}, C::Symbol = :Dict;
                 size::Int64 = 0) where T
    @assert !(Symbol(T) in keys(types.nodes)) "Each type can be added only once"
    @assert isbitstype(T)
    type_number = length(types.nodes_type2id) + 1
    @assert type_number < typemax(TypeID) "Can not add new type, 
                                maximal number of types already registered"
    push!(types.nodes_types, T)
    types.nodes_type2id[T] = type_number
    types.nodes_id2type[type_number] = T
    
    types.nodes[Symbol(T)] = C
    attr = Dict{Symbol,Any}()
    types.nodes_attr[Symbol(T)] = attr
    if size > 0
        attr[:size] = size
    end
    types
end

add_agenttype!(t::Type{T}) where T = types -> add_agenttype!(types, t) 

add_agenttype!(t::Type{T}, c::Symbol; kwargs...) where T where C =
    types -> add_agenttype!(types, t, c; kwargs...) 

"""
    add_edgetype!(sim, ::Type{T}) where {T <: EdgeState}

TODO DOC

Register an additional edge type to `sim`. 

An edge type is an struct that define the state for edges of type `T`.
These structs must be a subtype of `EdgeState`, and also bits
types, meaning the type is immutable and contains only primitive types
and other bits types.

Can only be called before [`finish_init!`](@ref)

See also [`add_edge!`](@ref) and [`add_edges!`](@ref) 
"""
function add_edgetype!(types::ModelTypes, ::Type{T}, C::Symbol = :Dict;
                size::Int64 = 0) where T  
    @assert !(Symbol(T) in keys(types.edges)) "Each type can be added only once"
    @assert isbitstype(T)
    types.edges[Symbol(T)] = C
    push!(types.edges_types, T)
    attr = Dict{Symbol,Any}()
    types.edges_attr[Symbol(T)] = attr
    if size > 0
        attr[:size] = size
    end
    types
end

add_edgetype!(t::Type{T}) where T = types -> add_edgetype!(types, t) 

add_edgetype!(t::Type{T}, c::Symbol; kwargs...) where T where C =
    types -> add_edgetype!(types, t, c; kwargs...) 

