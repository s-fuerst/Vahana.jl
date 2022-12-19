export ModelTypes
export register_agenttype!, register_edgetype!

"""
    ModelTypes

This struct stores all (internal) information about the agent and edge
types of the model and is therefore the starting point of the model
definition. 

Call the ModelType constructor without parameters, and then use the
created object as the first parameter in [`register_agenttype!`](@ref)
and [`register_edgetype!`](@ref), whereby the |> operator can be used to
concatenate these registrations.

See also [`construct_model`](@ref),
"""
Base.@kwdef struct ModelTypes
    edges_attr = Dict{DataType, Dict{Symbol, Any}}()
    edges_types = Vector{DataType}()
    nodes_attr = Dict{DataType, Dict{Symbol, Any}}()
    nodes_types = Vector{DataType}()
    nodes_type2id::Dict{DataType, TypeID} = Dict{DataType, TypeID}()
    nodes_id2type::Vector{DataType} = Vector{DataType}(undef, MAX_TYPES)
end

"""
    Model

A Model instance is created by [`construct_model`](@ref) and can then be
used to create one or multiple simulations via [`new_simulation`](@ref).
"""
struct Model
    types::ModelTypes
    name::String
end

"""
    register_agenttype!(types::ModelTypes, ::Type{T}, traits...; size) 

Register an additional agent type to `types`. 

An agent type is an struct that define the state for agents of type `T`.
These structs must be "bits types", meaning the type is immutable and
contains only primitive types and other bits types.

Per default it is assumed, that an agent can die (removed from the
simulation), by returning `nothing` in the transition function (see
[`apply_transition!`](@ref). In the case that agents are never
removed, the trait :Immortal can be given to improve the performance
of the simulation. When the size of the population is constant

TODO DOC traits. ConstantSize => Immortal

See also [`add_agent!`](@ref) and [`add_agents!`](@ref) 
"""
function register_agenttype!(types::ModelTypes, ::Type{T}, traits...;
                      size::Int64 = 0) where T
    @assert !(Symbol(T) in types.nodes_types) "Each type can be added only once"
    @assert isbitstype(T)
    type_number = length(types.nodes_type2id) + 1
    @assert type_number < MAX_TYPES "Can not add new type, 
                                maximal number of types already registered"
    push!(types.nodes_types, T)
    types.nodes_type2id[T] = type_number
    types.nodes_id2type[type_number] = T

    types.nodes_attr[T] = Dict{Symbol,Any}()

    traits = Set{Symbol}(traits)
    for trait in traits
        @assert trait in [:Immortal] """

        The agent type trait $trait is unknown for type $T. The following traits are
        supported: 
            :Immortal
            :ConstantSize

        A fixed (maximal) number of agents can be given via the optional size keyword.

        """
    end

    if size > 0
        @assert :Immortal in traits """

        The size of the population can only be specified for types with the 
        type trait :Immortal.

        """
    end

    types.nodes_attr[T][:traits] = traits
    
    if size > 0
        types.nodes_attr[T][:size] = size
    end

    types
end

register_agenttype!(t::Type{T}) where T = types -> register_agenttype!(types, t) 

register_agenttype!(t::Type{T}, traits...; kwargs...) where T =
    types -> register_agenttype!(types, t, traits...; kwargs...) 


show_single_edge_and_type_warning = true

"""
    register_edgetype!(types::ModelTypes, ::Type, traits..., kwargs...)  

Register an additional edge type to `types`. 

An edge type is an struct that define the state for edges of type `T`.  These
structs must be "bits types", meaning the type is immutable and
contains only primitive types and other bits types.

The internal data structures used to store the graph in memory can be modified by 
the traits parameters:

- `:IgnoreFrom`: The ID of the source agent is not stored. This
  implies that the state of the agents on the source of the edge is
  not accessible via the [`neighborstates`](@ref) function.
- `:Stateless`: Store only the ID of the source agent. 
- `:SingleAgentType`: All target agents have the same type.
- `:SingleEdge`: Each agent can be the target for max. one edge.
- `:IgnoreSourceState`: The ID of the source agent is not used to
  access the state of the agent with this ID.
- `:NumNeighborsOnly`: Combines `:IgnoreFrom` and `:Stateless`
- `:HasNeighborOnly`: Combines `:IgnoreFrom`, `:Stateless` and `:SingleEdge`

When `:SingleAgentType` is set it is necessary to add to the
`to_agenttype` keyword argument. The value of this argument must be
the type of the target nodes. In the case that it's known how many
agents of this type exists, this can be also given via the optional
keyword `size`.

See also [Edge Traits](./performance.md#Edge-Traits), [`add_edge!`](@ref) and 
[`add_edges!`](@ref) 
"""
function register_edgetype!(types::ModelTypes, ::Type{T}, traits...;
                     kwargs...)  where T
    global show_single_edge_and_type_warning
    @assert !(T in types.edges_types) "Each type can be added only once"
    @assert isbitstype(T)
    push!(types.edges_types, T)
    types.edges_attr[T] = kwargs
    traits = Set{Symbol}(traits)
    for trait in traits
        @assert trait in [:Stateless, :IgnoreFrom, :SingleEdge, :SingleAgentType,
                         :NumNeighborsOnly, :HasNeighborOnly, :IgnoreSourceState] """

        The edge type trait $trait is unknown for type $T. The following traits are
        supported: 
            :Stateless
            :IgnoreFrom
            :SingleEdge
            :SingleAgentType  
            :NumNeighborsOnly (which is equal to :Stateless & :IgnoreFrom)
            :HasNeighborOnly (which is equal to :Stateless & :IgnoreFrom & :SingleEdge)
            :IgnoreSourceState
    
        """
    end
    if :NumNeighborsOnly in traits
        union!(traits, Set([:Stateless, :IgnoreFrom]))
    end
    if :HasNeighborOnly in traits
        union!(traits, Set([:Stateless, :IgnoreFrom, :SingleEdge]))
    end
    if show_single_edge_and_type_warning && :SingleEdge in traits && !config.quiet &&
        :SingleAgentType in traits && !(:IgnoreFrom in traits && :Stateless in traits) 
        
        show_single_edge_and_type_warning = false
        printstyled("""

        Using the :SingleEdge and :SingleAgentType traits at the same time is
        risky. Please read the Danger box in the Performance Tuning page of the
        Vahana documentation and make sure that you understand the possible pitfalls. 

        """; color = :red)
    end
    if :SingleAgentType in traits
        @assert haskey(kwargs, :to_agenttype) """

        For type $T the :SingleAgentType trait is set, but in this
        case the agent type must also be specified via the
        to_agenttype keyword.

        """
    end
    if fieldcount(T) == 0 && !(:Stateless in traits)
        if config.detect_stateless
            union!(traits, Set([:Stateless]))
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
    @assert !(:SingleAgentType in traits && :SingleEdge in traits &&
        !(:Stateless in traits && :IgnoreFrom in traits)) """

        The traits :SingleEdge and :SingleAgentType can be only combined
        when the type $T has also the traits :Stateless and :IgnoreFrom.
        
        """
    
    types.edges_attr[T][:traits] = traits
    types
end

register_edgetype!(t::Type{T}, props...; kwargs...) where T =
    types -> register_edgetype!(types, t, props...; kwargs...) 

has_trait(sim, T::DataType, trait::Symbol) = 
    trait in sim.typeinfos.edges_attr[T][:traits]
