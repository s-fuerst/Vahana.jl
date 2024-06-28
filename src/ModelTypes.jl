export ModelTypes
export register_agenttype!, register_edgetype!, register_param!, register_global!


Base.@kwdef struct Param{T}
    name::Symbol
    default_value::T
end

Base.@kwdef struct Global{T}
    name::Symbol
    init_value::T
end

#Param(name, default::T) where T = Param{T}(name, default, default)


"""
    ModelTypes

This struct stores all (internal) information about the agent and edge
types of the model and is therefore the starting point of the model
definition. 

Call the ModelType constructor without parameters, and then use the
created object as the first parameter in [`register_agenttype!`](@ref)
and [`register_edgetype!`](@ref), whereby the |> operator can be used to
concatenate these registrations.

See also [`create_model`](@ref),
"""
Base.@kwdef struct ModelTypes
    edges_attr = Dict{DataType, Dict{Symbol, Any}}()
    edges_types = Vector{DataType}()
    nodes_attr = Dict{DataType, Dict{Symbol, Any}}()
    nodes_types = Vector{DataType}()
    nodes_type2id::Dict{DataType, TypeID} = Dict{DataType, TypeID}()
    nodes_id2type::Vector{DataType} = Vector{DataType}(undef, MAX_TYPES)
    params::Vector{Param} = Vector{Param}()
    globals::Vector{Global} = Vector{Global}()
end

"""
    Model

A Model instance is created by [`create_model`](@ref) and can then be
used to create one or multiple simulations via [`create_simulation`](@ref).
"""
struct Model
    types::ModelTypes
    name::String
    immortal::Vector{Bool}
end

"""
    register_agenttype!(types::ModelTypes, ::Type{T}, [hints...]) 

Register an additional agent type to `types`. 

An agent type is a struct that define the state space for agents of type `T`.
These structs must be "bits types", meaning the type is immutable and
contains only primitive types and other bits types.

By default, it is assumed that an agent can die (be removed from the
simulation) by returning `nothing` in the transition function (see
[`apply!`](@ref). In the case that agents are never removed, the hint
:Immortal can be given to improve the performance of the simulation.

See also [`add_agent!`](@ref) and [`add_agents!`](@ref) 
"""
function register_agenttype!(types::ModelTypes, ::Type{T}, hints...) where T
    @assert !(Symbol(T) in types.nodes_types) "Type $T is already registered"
    @assert isbitstype(T) "Agenttypes $T must be bitstypes"
    type_number = length(types.nodes_type2id) + 1
    @assert type_number < MAX_TYPES "Can not add new type, 
                                maximal number of types already registered"
    push!(types.nodes_types, T)
    types.nodes_type2id[T] = type_number
    types.nodes_id2type[type_number] = T

    types.nodes_attr[T] = Dict{Symbol,Any}()

    hints = Set{Symbol}(hints)
    for hint in hints
        @assert hint in [:Immortal, :Independent] """\n
        The agent type hint $hint is unknown for type $T. The following hints are
        supported: 
            :Immortal
            :Independent
        """
    end

    types.nodes_attr[T][:hints] = hints
    
    types
end

register_agenttype!(t::Type{T}) where T = types -> register_agenttype!(types, t) 

register_agenttype!(t::Type{T}, hints...; kwargs...) where T =
    types -> register_agenttype!(types, t, hints...; kwargs...) 


"""
    register_edgetype!(types::ModelTypes, ::Type{T}, [hints...; kwargs...])  

Register an additional edge type to `types`. 

An edge type T is a structure that defines the state (field) of
[`Edge{T}`](@ref).  These structs must be "bit types", that is, the
type is immutable and contains only primitive types and other bit
types. Often these types T are stateless, in which case they are used
as a tag to distinguish between the existing types of edges of the
model.

The internal data structures used to store the graph in memory can be modified by 
the hints parameters:

- `:IgnoreFrom`: The ID of the source agent is not stored. This
  implies that the state of the agents on the source of the edge is
  not accessible via e.g. the [`neighborstates`](@ref) function.
- `:Stateless`: Store only the ID of the source agent. 
- `:SingleType`: All target agents have the same type, needs also keyword
  `target` (see below).
- `:SingleEdge`: Each agent can be the target for max. one edge.
- `:IgnoreSourceState`: The ID of the source agent is not used to
  access the state of the agent with this ID.
- `:NumEdgesOnly`: Combines `:IgnoreFrom` and `:Stateless`
- `:HasEdgeOnly`: Combines `:IgnoreFrom`, `:Stateless` and `:SingleEdge`

If `:SingleType` is set, the keyword argument `target` must be
added. The value of this argument must be the type of the target
node. If the `target` keyword exists, but the `:SingleType` hint is
not explicitly specified, it will be set implicitly

If it is known how many agents of this type exist, this can also be
specified via the optional `size` keyword. This can improve
performance, but can also increase the memory usage.

See also [Edge Hints](./performance.md#Edge-Hints), [`add_edge!`](@ref) and 
[`add_edges!`](@ref) 
"""
function register_edgetype!(types::ModelTypes, ::Type{T}, hints...;
                     kwargs...)  where T
    @assert !(T in types.edges_types) "Type $T is already registered"
    @assert isbitstype(T) "Edgetypes $T must be bitstypes"
    push!(types.edges_types, T)
    types.edges_attr[T] = kwargs
    hints = Set{Symbol}(hints)
    for hint in hints
        @assert hint in [:Stateless, :IgnoreFrom, :SingleEdge, :SingleType,
                         :NumEdgesOnly, :HasEdgeOnly, :IgnoreSourceState] """

        The edge type hint $hint is unknown for type $T. The following hints are
        supported: 
            :Stateless
            :IgnoreFrom
            :SingleEdge
            :SingleType  
            :NumEdgesOnly (which is equal to :Stateless & :IgnoreFrom)
            :HasEdgeOnly (which is equal to :Stateless & :IgnoreFrom & :SingleEdge)
            :IgnoreSourceState
    
        """
    end
    if :NumEdgesOnly in hints
        union!(hints, Set([:Stateless, :IgnoreFrom]))
    end
    if :HasEdgeOnly in hints
        union!(hints, Set([:Stateless, :IgnoreFrom, :SingleEdge]))
    end
    if :SingleType in hints
        @assert haskey(kwargs, :target) """

        For type $T the :SingleType hint is set, but in this
        case the agent type must also be specified via the
        target keyword.

        """
    end
    if haskey(kwargs, :target) && !(:SingleType in hints)
        if ! config.quiet
            @rootonly printstyled("""

            Since the `target` keyword exists for type $T, the :SingleType hint is added for this type.

            """; color = :red)
        end

        push!(hints, :SingleType)
    end
    if fieldcount(T) == 0 && !(:Stateless in hints)
        if config.detect_stateless
            union!(hints, Set([:Stateless]))
        elseif ! config.quiet
            @rootonly printstyled("""

        Edgetype $T is a struct without any field, so you can increase the
        performance by setting the :Stateless hint. You can also
        calling detect_stateless() before calling register_edgetype!, 
        then the :Stateless hint will be set automatically for structs without
        a field.

        """; color = :red)
        end
    end
    @assert !(:SingleType in hints && :SingleEdge in hints &&
        !(:Stateless in hints && :IgnoreFrom in hints)) """\n
        The hints :SingleEdge and :SingleType can be only combined
        when the type $T has also the hints :Stateless and :IgnoreFrom.
        """
    
    types.edges_attr[T][:hints] = hints
    types
end

register_edgetype!(t::Type{T}, props...; kwargs...) where T =
    types -> register_edgetype!(types, t, props...; kwargs...) 

has_hint(sim, T::DataType, hint::Symbol, ge = :Edge) =
    if ge == :Edge
        hint in sim.typeinfos.edges_attr[T][:hints]
    elseif ge == :Agent 
        hint in sim.typeinfos.nodes_attr[T][:hints]
    else
        @error "Unknown graph element, use :Agent or :Edge"
    end

"""
DOCTODO
"""
function register_param!(types::ModelTypes, name::Symbol, default_value::T) where T
    push!(types.params, Param{T}(name, default_value))
    types
end

register_param!(name, default_value::T) where T =
    types -> register_param!(types, name, default_value) 

"""
DOCTODO
"""
function register_global!(types::ModelTypes, name::Symbol, init_value::T) where T
    push!(types.globals, Global{T}(name, init_value))
    types
end

register_global!(name, init_value::T) where T =
    types -> register_global!(types, name, init_value) 

