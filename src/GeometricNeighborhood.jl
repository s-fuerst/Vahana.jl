export GeometricNeighborhood, ReplicatedKDTree

struct GeometricNeighborhood{
    S<:GeometricSpace,
    F<:Function,
    NF<:Union{Nothing,Function},
    P<:Function,
}
    space::S
    posfunc::F
    query_types::Vector{DataType}
    neighbor_types::Vector{DataType}
    neighbor_filter::NF
    radius::Float64
    project::P
    project_type::Union{Nothing,DataType}
    weight::Float64
end

abstract type GeometricBackend end

struct GeometricRegistration{D<:GeometricNeighborhood,B<:GeometricBackend}
    name::Symbol
    definition::D
    backend::B
    coordinate_type::DataType
    project_type::DataType
end

mutable struct NeighborhoodRuntime{R<:GeometricRegistration,P}
    registration::R
    prepared::Union{Nothing,P}
end

function construct_geom_fields(registrations, simsymbol)
    geomsymbol = Symbol("GeomNeighborhoods_", simsymbol)
    fields = map(registrations) do registration
        R = typeof(registration)
        P = geom_prepared_type(registration)
        F = NeighborhoodRuntime{R,P}
        :($(registration.name)::$F = $F($(QuoteNode(registration)), nothing))
    end

    strukt = Expr(:struct, false, geomsymbol, Expr(:block, fields...))
    eval(:(Base.@kwdef $strukt))
    geomsymbol
end

function GeometricNeighborhood(
    space::GeometricSpace,
    posfunc::Function,
    query_types,
    neighbor_types;
    neighbor_filter::Union{Nothing,Function}=nothing,
    radius::Real=1,
    project::Function=identity,
    project_type::Union{Nothing,DataType}=nothing,
    weight::Real=1.0,
)
    query_types = applicable(iterate, query_types) ? query_types : [query_types]
    neighbor_types = applicable(iterate, neighbor_types) ? neighbor_types :
        [ neighbor_types ]

    GeometricNeighborhood(
        space,
        posfunc,
        query_types,
        neighbor_types,
        neighbor_filter,
        Float64(radius),
        project,
        project_type,
        Float64(weight),
    )
end
