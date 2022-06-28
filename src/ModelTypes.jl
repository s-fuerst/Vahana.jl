export ModelTypes
export register_agenttype!, register_edgetype!

    

"""
# TODO DOC
"""
Base.@kwdef struct ModelTypes
    edges_attr = Dict{DataType, Dict{Symbol, Any}}()
    edges_types = Vector{DataType}()
    nodes = Dict{DataType, Symbol}()
    nodes_attr = Dict{DataType, Dict{Symbol, Any}}()
    nodes_types = Vector{DataType}()
    nodes_type2id::Dict{DataType, TypeID} = Dict{DataType, TypeID}()
    nodes_id2type::Vector{DataType} = Vector{DataType}(undef, MAX_TYPES)
end

struct Model
    types::ModelTypes
    name::String
end

"""
    register_agenttype!(types, ::Type{T}, C::Symbol = :Dict; size) where T

Register an additional agent type to `sim`. 

An agent type is an struct that define the state for agents of type `T`.
These structs must be a subtype of `Agent`, and also bits
types, meaning the type is immutable and contains only primitive types
and other bits types.

!!! warning

    In the current version, per default the agents are stored in a
    dictonary. In the case, that agents of this type will be never
    removed in a simulation, this can be changed to increase the
    performance by setting the optional third paramter to :Vector. If
    the size of the Vector is fixed, the size can be given as keyword
    argument. This will change in the upcoming MPI-Version, so it's better
    to ignore this for now.


Can only be called before [`finish_init!`](@ref)

See also [`add_agent!`](@ref) and [`add_agents!`](@ref) 
"""
function register_agenttype!(types::ModelTypes, ::Type{T}, C::Symbol = :Dict;
                 size::Int64 = 0) where T
    @assert !(Symbol(T) in keys(types.nodes)) "Each type can be added only once"
    @assert isbitstype(T)
    type_number = length(types.nodes_type2id) + 1
    @assert type_number < MAX_TYPES "Can not add new type, 
                                maximal number of types already registered"
    push!(types.nodes_types, T)
    types.nodes_type2id[T] = type_number
    types.nodes_id2type[type_number] = T
    
    types.nodes[T] = C
    attr = Dict{Symbol,Any}()
    types.nodes_attr[T] = attr
    if size > 0
        attr[:size] = size
    end
    types
end

register_agenttype!(t::Type{T}) where T = types -> register_agenttype!(types, t) 

register_agenttype!(t::Type{T}, c::Symbol; kwargs...) where T =
    types -> register_agenttype!(types, t, c; kwargs...) 


show_single_edge_and_type_warning = true

"""
    register_edgetype!(sim, ::Type{T}) where {T <: EdgeState}

TODO DOC

Register an additional edge type to `sim`. 

An edge type is an struct that define the state for edges of type `T`.
These structs must be a subtype of `EdgeState`, and also bits
types, meaning the type is immutable and contains only primitive types
and other bits types.

Can only be called before [`finish_init!`](@ref)

Traits:
Stateless
SingleAgentType
SingleEdge
IgnoreFrom

kwargs (for SingleTAgentype):
num_agents (optional)
agent_type (optional?)

See also [`add_edge!`](@ref) and [`add_edges!`](@ref) 
"""
function register_edgetype!(types::ModelTypes, ::Type{T}, traits...;
                kwargs...)  where T
    global show_single_edge_and_type_warning
    @assert !(T in types.edges_types) "Each type can be added only once"
    @assert isbitstype(T)
    push!(types.edges_types, T)
    types.edges_attr[T] = kwargs
    p = Set{Symbol}(traits)
    for trait in p
        @assert trait in [:Stateless, :IgnoreFrom, :SingleEdge, :SingleAgentType,
                         :NumEdgesOnly, :HasEdgeOnly] """

        The edge type trait $trait is unknown for type $T. The following traits are
        supported: 
            :Stateless
            :IgnoreFrom
            :SingleEdge
            :SingleAgentType  
            :NumEdgesOnly (which is equal to :Stateless & :IgnoreFrom)
            :HasEdgeOnly (which is equal to :Stateless & :IgnoreFrom & :SingleEdge)

        """
    end
    if :NumEdgesOnly in traits
        push!(traits, [:Stateless, :IgnoreFrom])
    end
    if :HasEdgeOnly in traits
        push!(traits, [:Stateless, :IgnoreFrom, :SingleEdge])
    end
    if show_single_edge_and_type_warning && :SingleEdge in p && !config.quiet
        :SingleAgentType in p && !(:IgnoreFrom in p && :Stateless in p) 
        
        show_single_edge_and_type_warning = false
        printstyled("""

        Using the :SingleEdge and :SingleAgentType traits at the same time 
        is risky. Please read the documentation (TODO add link) and make
        sure that you understand the possible pitfalls. 

        """; color = :red)
    end
    if :SingleAgentType in p
        @assert haskey(kwargs, :to_agenttype) """

        For type $T the :SingleAgentType property is set, but in this
        case the agent type must also be specified via the
        to_agenttype keyword.

        """
    end
    if fieldcount(T) == 0 && !(:Stateless in traits)
        if config.detect_stateless
            push!(traits, :Stateless)
        elseif !config.quiet
        printstyled("""

        Edgetype $T is a struct without any field, so you can increase the
        performance by setting the :Stateless trait. You can also
        calling detect_stateless_trait() before calling register_edgetype!, 
        then the :Stateless trait will be set automatically for structs without
        a field.

        """; color = :red)
        end
    end
    types.edges_attr[T][:traits] = p
    types
end

register_edgetype!(t::Type{T}, props...; kwargs...) where T =
    types -> register_edgetype!(types, t, props...; kwargs...) 

