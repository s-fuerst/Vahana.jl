export geom_neighbors, geom_neighbors_iter
export geom_neighborstates, geom_neighborstates_iter
export geom_neighborids, geom_neighborids_iter

# Internal backend hooks. `geom_hits(runtime, id, position)` yields exact
# hit indices. The three accessors read results at those indices without
# searching again. Neither the search nor the accessors communicate via MPI.
# `geom_projection_type` must also work when there are no hits.
function geom_hits end
function geom_neighbor_id end
function geom_neighbor_projection end
function geom_neighbor_pair end
function geom_projection_type end

# Shared query machinery. The backend owns the hit source; this iterator
# reads only the requested result (ID, projection, or pair) for each hit.
struct GeomNeighborIterator{T,R,H,F}
    runtime::R
    hits::H
    read_hit::F
end

GeomNeighborIterator{T}(runtime::R, hits::H, read_hit::F) where {T,R,H,F} =
    GeomNeighborIterator{T,R,H,F}(runtime, hits, read_hit)

Base.IteratorSize(::Type{<:GeomNeighborIterator}) = Base.SizeUnknown()
Base.IteratorEltype(::Type{<:GeomNeighborIterator}) = Base.HasEltype()
Base.eltype(::Type{<:GeomNeighborIterator{T}}) where {T} = T

function Base.iterate(iter::GeomNeighborIterator, state...)
    next = iterate(iter.hits, state...)
    if next === nothing
        nothing
    else
        index, next_state = next
        (iter.read_hit(iter.runtime, index), next_state)
    end
end

@inline function geom_query_hits(sim, id, name::Symbol)
    runtime = getfield(sim.geom_neighborhoods, name)

    @mayassert runtime.prepared !== nothing """
    Geometric neighborhood $name was not prepared for this apply! call.
    """

    definition = runtime.registration.definition
    agent_type = type_of(sim, id)

    @mayassert agent_type in definition.query_types """
    Agent type $agent_type cannot query geometric neighborhood $name.
    """

    state = agentstate(sim, id, agent_type)
    position = definition.posfunc(state)
    hits = geom_hits(runtime, id, position)

    (runtime, hits)
end

# Public query API

@inline function geom_neighborids_iter(sim, id, name::Symbol)
    runtime, hits = geom_query_hits(sim, id, name)
    GeomNeighborIterator{AgentID}(runtime, hits, geom_neighbor_id)
end

@inline function geom_neighborstates_iter(sim, id, name::Symbol)
    runtime, hits = geom_query_hits(sim, id, name)
    T = geom_projection_type(runtime)
    GeomNeighborIterator{T}(runtime, hits, geom_neighbor_projection)
end

@inline function geom_neighbors_iter(sim, id, name::Symbol)
    runtime, hits = geom_query_hits(sim, id, name)
    T = geom_projection_type(runtime)
    GeomNeighborIterator{Tuple{AgentID,T}}(runtime, hits, geom_neighbor_pair)
end

@inline geom_neighborids(sim, id, name::Symbol) =
    collect(geom_neighborids_iter(sim, id, name))

@inline geom_neighborstates(sim, id, name::Symbol) =
    collect(geom_neighborstates_iter(sim, id, name))

@inline geom_neighbors(sim, id, name::Symbol) =
    collect(geom_neighbors_iter(sim, id, name))

# Optional helpers for backend preparation; not part of the backend contract.

# Collect `(positions, ids, projections)` for the living neighbors on the
# current rank that pass the registered neighbor filter. The three vectors have
# matching indices and use the registered coordinate and projection types.

# Call this before the transition callbacks to read their initial state. This
# helper does not communicate via MPI or build an index. Backends can use a
# different collection strategy if they do not need these intermediate vectors.
function geom_collect_local_neighbors(sim, registration)
    definition = registration.definition
    N = length(definition.space.bounds)
    positions = SVector{N,registration.coordinate_type}[]
    ids = AgentID[]
    projections = registration.project_type[]

    for agent_type in definition.neighbor_types
        _geom_collect_local_neighbors!(
            sim, agent_type, definition, positions, ids, projections,
        )
    end

    (positions, ids, projections)
end

# Append one agent type to the three aligned vectors.
function _geom_collect_local_neighbors!(
    sim, ::Type{T}, definition, positions, ids, projections,
) where {T}
    read = simfield(sim, T).read
    immortal = has_hint(sim, T, :Immortal, :Agent)
    filter = definition.neighbor_filter
    agent_type_id = typeid(sim, T)

    if filter === nothing
        capacity = length(ids) + length(read.state)
        sizehint!(positions, capacity)
        sizehint!(ids, capacity)
        sizehint!(projections, capacity)
    end

    for i in eachindex(read.state)
        if immortal || !read.died[i]
            state = read.state[i]
            if filter === nothing || filter(state)
                position = definition.posfunc(state)
                push!(positions, position)
                push!(ids, agent_id(agent_type_id, AgentNr(i)))
                push!(projections, definition.project(state))
            end
        end
    end
    nothing
end
