Base.@kwdef struct ReplicatedKDTree <: GeometricBackend
    leafsize::Int = 25
    reorder::Bool = true
end

import NearestNeighbors

struct ReplicatedKDPrepared{V,T,K}
    positions::Vector{V}
    ids::Vector{AgentID}
    projections::Vector{T}
    tree::Union{Nothing,K}
end

const ReplicatedKDRuntime =
    NeighborhoodRuntime{<:GeometricRegistration{<:GeometricNeighborhood,ReplicatedKDTree}}

function geom_validate_backend(
    registration::GeometricRegistration{D,ReplicatedKDTree},
) where {D}
    if registration.backend.leafsize < 1
        throw(ArgumentError("ReplicatedKDTree requires a positive leafsize."))
    end
    if !isbitstype(registration.project_type)
        throw(ArgumentError(
            "ReplicatedKDTree requires a bits-type projection for neighborhood " *
            "$(registration.name).",
        ))
    end
    nothing
end

function geom_prepared_type(
    registration::GeometricRegistration{D,ReplicatedKDTree},
) where {D}
    N = length(registration.definition.space.bounds)
    V = SVector{N,registration.coordinate_type}
    W = SVector{N,Float64}
    K = NearestNeighbors.KDTree{W,NearestNeighbors.Chebyshev,Float64,W}
    P = any(registration.definition.space.periodic) ?
        NearestNeighbors.PeriodicTree{W,NearestNeighbors.Chebyshev,K,N,Float64} : K
    ReplicatedKDPrepared{V,registration.project_type,P}
end

function _kd_collect_neighbors(sim, registration, agent_type)
    failure = nothing
    local_data = try
        geom_collect_local_neighbors(sim, registration)
    catch exception
        failure = exception
        nothing
    end

    if mpi.active
        failed = MPI.Allreduce(Int(failure !== nothing), max, mpi.comm)
        if failed != 0 && failure === nothing
            throw(ErrorException(
                "Could not collect neighbors for $(registration.name) on another rank.",
            ))
        end
    end
    if failure !== nothing
        throw(failure)
    end

    positions, ids, projections = local_data
    if mpi.active
        count = MPI.Allreduce(Int64(length(ids)), +, mpi.comm)
        if count > typemax(Int32)
            throw(ArgumentError("Too many neighbors for the MPI join in $(registration.name)."))
        end

        positions = join(positions; sim, label=:join_spatial_poss, type=agent_type)
        ids = join(ids; sim, label=:join_spatial_ids, type=agent_type)
        projections = if sizeof(eltype(projections)) == 0
            similar(projections, length(ids))
        else
            join(projections; sim, label=:join_spatial_edges, type=agent_type)
        end
    end

    (positions, ids, projections)
end

function geom_prepare!(sim, runtime::NeighborhoodRuntime{R,P}) where {
    R<:GeometricRegistration{<:GeometricNeighborhood,ReplicatedKDTree},P
}
    registration = runtime.registration
    definition = registration.definition
    neighbor_types = definition.neighbor_types
    agent_type = length(neighbor_types) == 1 ? only(neighbor_types) : Nothing

    positions, ids, projections = _inst_phase(sim, :spatial_collect, agent_type) do
        _kd_collect_neighbors(sim, registration, agent_type)
    end

    space = definition.space
    tree = if !isempty(positions) && _kd_indexable_bounds(space) &&
              all(position -> _kd_indexable(space, position), positions)
        _inst_phase(sim, :spatial_kdtree, agent_type) do
            points = [_kd_point(space, position) for position in positions]
            kd = NearestNeighbors.KDTree(
                points, NearestNeighbors.Chebyshev();
                leafsize=registration.backend.leafsize,
                reorder=registration.backend.reorder,
            )
            if any(space.periodic)
                lower = map(periodic -> periodic ? 0.0 : -Inf, space.periodic)
                upper = map(space.bounds, space.periodic) do bounds, periodic
                    periodic ? _kd_period(space, bounds) : Inf
                end
                NearestNeighbors.PeriodicTree(kd, lower, upper)
            else
                kd
            end
        end
    else
        nothing
    end

    P(positions, ids, projections, tree)
end

_kd_indexable(position) = all(x -> abs(Float64(x)) <= 2.0^51, position)

_kd_period(::GridSpace, (lo, hi)) = Float64(Int128(hi) - Int128(lo) + 1)
_kd_period(::ContinuousSpace, (lo, hi)) = Float64(hi) - Float64(lo)

function _kd_indexable_bounds(space)
    all(eachindex(space.bounds)) do axis
        bounds = space.bounds[axis]
        !space.periodic[axis] ||
            (_kd_indexable(bounds) && 0.0 < _kd_period(space, bounds) <= 2.0^52)
    end
end

function _kd_indexable(space, position)
    _kd_indexable(position) && all(eachindex(space.bounds)) do axis
        lo, hi = space.bounds[axis]
        !space.periodic[axis] || lo <= position[axis] <= hi
    end
end

function _kd_point(space, position)
    SVector(map(space.bounds, space.periodic, Tuple(position)) do bounds, periodic, x
        if periodic
            mod(Float64(x) - Float64(first(bounds)), _kd_period(space, bounds))
        else
            Float64(x)
        end
    end)
end

function _kd_periodic_padding(space)
    # Account for rounding when shifting the query across periodic boundaries.
    scale = maximum(
        (maximum(x -> abs(Float64(x)), space.bounds[i])
         for i in eachindex(space.bounds) if space.periodic[i]);
        init=0.0,
    )
    8 * eps(scale)
end

function geom_hits(runtime::ReplicatedKDRuntime, id, position)::Vector{Int}
    definition = runtime.registration.definition
    prepared = runtime.prepared
    radius = definition.radius

    # Conservative box; the exact metric decides below.
    extent = 2 * max(radius, sqrt(floatmin(Float64)))
    space = definition.space
    if prepared.tree isa NearestNeighbors.PeriodicTree
        extent += _kd_periodic_padding(space)
    end
    candidates = if prepared.tree !== nothing && _kd_indexable(space, position) &&
                    isfinite(extent)
        point = _kd_point(space, position)
        NearestNeighbors.inrange(prepared.tree, point, extent)
    else
        collect(eachindex(prepared.ids))
    end

    filter!(candidates) do i
        prepared.ids[i] != id &&
            geom_distance(definition.space, position, prepared.positions[i]) <= radius
    end
end

geom_neighbor_id(runtime::ReplicatedKDRuntime, index) =
    runtime.prepared.ids[index]

geom_neighbor_projection(runtime::ReplicatedKDRuntime, index) =
    runtime.prepared.projections[index]

geom_neighbor_pair(runtime::ReplicatedKDRuntime, index) =
    (runtime.prepared.ids[index], runtime.prepared.projections[index])

geom_projection_type(::NeighborhoodRuntime{R,ReplicatedKDPrepared{V,T,K}}) where {R,V,T,K} =
    T

geom_release!(sim, runtime::ReplicatedKDRuntime) = nothing
