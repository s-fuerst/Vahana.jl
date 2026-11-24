Base.@kwdef struct ReplicatedKDTree <: GeometricBackend
    leafsize::Int = 25
    reorder::Bool = true
end

import NearestNeighbors

struct KDBatch{T}
    ids::Vector{AgentID}
    projections::Vector{T}
end

struct KDSegments{T}
    batches::Vector{KDBatch{T}}
    ends::Vector{Int}
end

struct ReplicatedKDPrepared{V,T,K,B}
    positions::Vector{V}
    neighbors::B
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
    T = registration.project_type
    B = length(registration.definition.neighbor_types) == 1 ? KDBatch{T} : KDSegments{T}
    ReplicatedKDPrepared{V,T,P,B}
end

function _kd_collect_batch(sim, registration, agent_type)
    failure = nothing
    local_data = try
        geom_collect_local_neighbors(sim, registration, agent_type)
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
    elseif projections === simfield(sim, agent_type).read.state &&
           has_hint(sim, agent_type, :Independent, :Agent)
        projections = copy(projections)
    end

    (positions, KDBatch(ids, projections))
end

function _kd_collect_neighbors(sim, registration)
    definition = registration.definition
    types = definition.neighbor_types
    if length(types) == 1
        _kd_collect_batch(sim, registration, only(types))
    else
        N = length(definition.space.bounds)
        positions = SVector{N,registration.coordinate_type}[]
        batches = KDBatch{registration.project_type}[]
        ends = Int[]
        for agent_type in types
            batch_positions, batch = _kd_collect_batch(sim, registration, agent_type)
            append!(positions, batch_positions)
            push!(batches, batch)
            push!(ends, length(positions))
        end
        (positions, KDSegments(batches, ends))
    end
end

function geom_prepare!(sim, runtime::NeighborhoodRuntime{R,P}) where {
    R<:GeometricRegistration{<:GeometricNeighborhood,ReplicatedKDTree},P
}
    registration = runtime.registration
    definition = registration.definition
    neighbor_types = definition.neighbor_types
    agent_type = length(neighbor_types) == 1 ? only(neighbor_types) : Nothing

    positions, neighbors = _inst_phase(sim, :spatial_collect, agent_type) do
        _kd_collect_neighbors(sim, registration)
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

    P(positions, neighbors, tree)
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

@inline _kd_locate(batch::KDBatch, i) = (batch, i)

@inline function _kd_locate(segments::KDSegments, i)
    b = searchsortedfirst(segments.ends, i)
    offset = b == 1 ? 0 : segments.ends[b - 1]
    (segments.batches[b], i - offset)
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
        collect(eachindex(prepared.positions))
    end

    filter!(candidates) do i
        geom_distance(space, position, prepared.positions[i]) <= radius &&
            geom_neighbor_id(runtime, i) != id
    end
end

@inline function geom_neighbor_id(runtime::ReplicatedKDRuntime, index)
    batch, i = _kd_locate(runtime.prepared.neighbors, index)
    batch.ids[i]
end

@inline function geom_neighbor_projection(runtime::ReplicatedKDRuntime, index)
    batch, i = _kd_locate(runtime.prepared.neighbors, index)
    batch.projections[i]
end

@inline function geom_neighbor_pair(runtime::ReplicatedKDRuntime, index)
    batch, i = _kd_locate(runtime.prepared.neighbors, index)
    (batch.ids[i], batch.projections[i])
end

geom_projection_type(::NeighborhoodRuntime{R,ReplicatedKDPrepared{V,T,K,B}}) where {R,V,T,K,B} =
    T

geom_release!(sim, runtime::ReplicatedKDRuntime) = nothing
