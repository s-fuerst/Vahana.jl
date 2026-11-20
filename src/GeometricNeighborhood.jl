export GeometricNeighborhood, ReplicatedKDTree

struct GeometricNeighborhood{
    S<:GeometricSpace,
    F<:Function,
    QF<:Union{Nothing,Function},
    NF<:Union{Nothing,Function},
    P<:Function,
}
    space::S
    posfunc::F
    query_types::Vector{DataType}
    neighbor_types::Vector{DataType}
    query_filter::QF
    neighbor_filter::NF
    radius::Float64
    project::P
    project_type::Union{Nothing,DataType}
    weight::Float64
end

Base.@kwdef struct ReplicatedKDTree
    leafsize::Int = 25
    reorder::Bool = true
end

struct GeometricRegistration
    name::Symbol
    definition::GeometricNeighborhood
    backend::ReplicatedKDTree
    coordinate_type::DataType
    project_type::DataType
end

function GeometricNeighborhood(;
    space::GeometricSpace,
    posfunc::Function,
    query_types,
    neighbor_types,
    query_filter::Union{Nothing,Function}=nothing,
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
        query_filter,
        neighbor_filter,
        Float64(radius),
        project,
        project_type,
        Float64(weight),
    )
end
