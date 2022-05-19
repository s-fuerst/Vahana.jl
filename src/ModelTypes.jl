export ModelTypes
export add_agenttype!, add_edgetype!

# TODO DOC
Base.@kwdef struct ModelTypes
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


show_single_edge_and_type_warning = true

"""
    add_edgetype!(sim, ::Type{T}) where {T <: EdgeState}

TODO DOC

Register an additional edge type to `sim`. 

An edge type is an struct that define the state for edges of type `T`.
These structs must be a subtype of `EdgeState`, and also bits
types, meaning the type is immutable and contains only primitive types
and other bits types.

Can only be called before [`finish_init!`](@ref)

Props:
SingleAgentType
SingleEdge
IgnoreFrom

kwargs (for SingleTAgentype):
num_agents (optional)
agent_type (optional?)

See also [`add_edge!`](@ref) and [`add_edges!`](@ref) 
"""
function add_edgetype!(types::ModelTypes, ::Type{T}, props...;
                kwargs...)  where T
    global show_single_edge_and_type_warning
    @assert !(T in types.edges_types) "Each type can be added only once"
    @assert isbitstype(T)
    push!(types.edges_types, T)
    types.edges_attr[Symbol(T)] = kwargs
    p = Set{Symbol}(props)
    for prop in p
        @assert prop in [:Stateless, :IgnoreFrom, :SingleEdge, :SingleAgentType] """

        The edge type property $prop is unknown. The following properties are
        supported: 
            :Stateless
            :IgnoreFrom
            :SingleEdge
            :SingleAgentType    
        """
    end
    if :SingleEdge in p && :SingleAgentType in p && show_single_edge_and_type_warning
        show_single_edge_and_type_warning = false
        printstyled("""

        Using the :SingleEdge and :SingleAgentType property at the same time 
        is risky. Please read the documentation (TODO add link) and make
        sure that you understand the possible pitfalls. 

        """; color = :red)
    end
    if :SingleAgentType in p
        @assert haskey(kwargs, :to_agenttype) """
        If the :SingleAgentType property is set, the agent type must also be 
        specified via the to_agenttype keyword.        
        """
    end
    # if fieldcount(T) == 0
    #     push!(p, :Stateless)
    # end
    types.edges_attr[Symbol(T)][:props] = p
    types
end


#add_edgetype!(t::Type{T}) where T = types -> add_edgetype!(types, t) 

add_edgetype!(t::Type{T}, props...; kwargs...) where T =
    types -> add_edgetype!(types, t, props...; kwargs...) 

