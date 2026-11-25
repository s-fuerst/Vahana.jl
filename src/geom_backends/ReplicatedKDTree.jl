Base.@kwdef struct ReplicatedKDTree <: GeometricBackend
    leafsize::Int = 25
    reorder::Bool = true
end

import NearestNeighbors

struct KDBatch{T,K}
    ids::Vector{AgentID}
    projections::Vector{T}
    tree::Union{Nothing,K}
end

struct ReplicatedKDPrepared{T,B}
    neighbors::B
end

const ReplicatedKDRuntime =
    NeighborhoodRuntime{<:GeometricRegistration{<:GeometricNeighborhood,ReplicatedKDTree}}

function geom_validate_backend(
    registration::GeometricRegistration{D,ReplicatedKDTree},
) where {D}
    if registration.backend.leafsize < 1
        throw(ArgumentError("ReplicatedKDTree requires a positive leafsize."))
    end
    if !(registration.definition.space.metric isa Euclidean)
        throw(ArgumentError("ReplicatedKDTree supports only the Euclidean metric."))
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
    W = SVector{N,Float64}
    K = NearestNeighbors.KDTree{W,NearestNeighbors.Euclidean,Float64,W}
    P = any(registration.definition.space.periodic) ?
        NearestNeighbors.PeriodicTree{W,NearestNeighbors.Euclidean,K,N,Float64} : K
    T = registration.project_type
    B = KDBatch{T,P}
    ReplicatedKDPrepared{T,length(registration.definition.neighbor_types) == 1 ? B : Vector{B}}
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

    (positions, ids, projections)
end

function _kd_prepare_batch(::Type{B}, sim, registration, agent_type) where {B<:KDBatch}
    positions, ids, projections = _inst_phase(sim, :spatial_collect, agent_type) do
        _kd_collect_batch(sim, registration, agent_type)
    end
    space = registration.definition.space
    tree = if isempty(positions)
        nothing
    else
        _inst_phase(sim, :spatial_kdtree, agent_type) do
            points = _kd_points(space, positions)
            kd = NearestNeighbors.KDTree(
                points, NearestNeighbors.Euclidean();
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
    end
    B(ids, projections, tree)
end

function geom_prepare!(sim, runtime::NeighborhoodRuntime{R,ReplicatedKDPrepared{T,B}}) where {
    R<:GeometricRegistration{<:GeometricNeighborhood,ReplicatedKDTree},T,B
}
    registration = runtime.registration
    types = registration.definition.neighbor_types
    neighbors = if B <: KDBatch
        _kd_prepare_batch(B, sim, registration, only(types))
    else
        Batch = eltype(B)
        Batch[_kd_prepare_batch(Batch, sim, registration, agent_type) for agent_type in types]
    end
    ReplicatedKDPrepared{T,B}(neighbors)
end

function _kd_points(space, positions::Vector{SVector{N,T}}) where {N,T}
    if T === Float64 && !any(space.periodic)
        positions
    else
        [_kd_point(space, position) for position in positions]
    end
end

_kd_period(::GridSpace, (lo, hi)) = Float64(Int128(hi) - Int128(lo) + 1)
_kd_period(::ContinuousSpace, (lo, hi)) = Float64(hi) - Float64(lo)

function _kd_point(space, position)
    SVector(map(space.bounds, space.periodic, Tuple(position)) do bounds, periodic, x
        if periodic
            mod(Float64(x) - Float64(first(bounds)), _kd_period(space, bounds))
        else
            Float64(x)
        end
    end)
end

function _kd_batch_hits(batch::KDBatch, id, point, radius)::Vector{Int}
    tree = batch.tree
    hits = if tree === nothing
        Int[]
    else
        NearestNeighbors.inrange(tree, point, radius)
    end
    if tree !== nothing && type_nr(first(batch.ids)) == type_nr(id)
        filter!(i -> batch.ids[i] != id, hits)
    end
    hits
end

_kd_hits(batch::KDBatch, id, point, radius) = _kd_batch_hits(batch, id, point, radius)

struct KDBatchHits
    indices::Vector{Vector{Int}}
end

Base.IteratorSize(::Type{KDBatchHits}) = Base.SizeUnknown()
Base.eltype(::Type{KDBatchHits}) = Tuple{Int,Int}

@inline function Base.iterate(hits::KDBatchHits, (batch, offset)=(1, 1))
    while batch <= length(hits.indices) && offset > length(hits.indices[batch])
        batch += 1
        offset = 1
    end
    if batch <= length(hits.indices)
        ((batch, hits.indices[batch][offset]), (batch, offset + 1))
    else
        nothing
    end
end

function _kd_hits(batches::Vector{<:KDBatch}, id, point, radius)
    KDBatchHits([_kd_batch_hits(batch, id, point, radius) for batch in batches])
end

function geom_hits(runtime::ReplicatedKDRuntime, id, position)
    definition = runtime.registration.definition
    point = _kd_point(definition.space, position)
    _kd_hits(runtime.prepared.neighbors, id, point, definition.radius)
end

@inline _kd_locate(batch::KDBatch, index) = (batch, index)
@inline _kd_locate(batches::Vector{<:KDBatch}, index) = (batches[index[1]], index[2])

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

geom_projection_type(::NeighborhoodRuntime{R,ReplicatedKDPrepared{T,B}}) where {R,T,B} = T

geom_release!(sim, runtime::ReplicatedKDRuntime) = nothing
