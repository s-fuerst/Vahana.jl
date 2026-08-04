export connect_spatial_neighbors!
export periodic_diff, periodic_clamp
export SpatialNeighbors
export find_neighbors, find_neighbors_iter

using StaticArrays
import NearestNeighbors: PeriodicTree, KDTree, knn, inrange, Euclidean

struct NeighborsInfo{T}
    kdtree::Union{PeriodicTree, KDTree}
    states::Vector{T}
    snhash::UInt64
    empty::Bool
end

NeighborsInfo(sn) = NeighborsInfo(KDTree(zeros(1, 0)), [], hash(sn), true)

# edge_cons can be also the identity function to get the states instead
function _agents_ids_states_and_edges(sim, ::Type{T}, pos_func, filter_pred,
                               edge_cons, must_join; ignore_ids = false) where T
    # Note: This function should not be called within a transition function
    
    states::Vector{T} = sim.initialized ?
        simfield(sim, T).read.state : 
        simfield(sim, T).write.state  

    if ! ignore_ids
        ids = AgentNr[]
        sizehint!(ids, length(states))
    end

    poss = Vector{Base.return_types(pos_func)[1]}()
    sizehint!(poss, length(states))

    immortal = has_hint(sim, T, :Immortal, :Agent)
    
    if edge_cons !== nothing
        edges = if immortal && edge_cons == identity
            states
        else
            e = Vector{Base.return_types(edge_cons)[1]}()
            sizehint!(e, length(states))
            e
        end
    end
    
    if filter_pred === nothing
        if immortal
            for i in 1:length(states)
                if ! ignore_ids
                    push!(ids, agent_id(typeid(sim, T), AgentNr(i)))
                end
                push!(poss, pos_func(states[i]))
                if edge_cons !== nothing && edge_cons !== identity
                    if typeof(edge_cons) == DataType
                        push!(edges, edge_cons())
                    else
                        push!(edges, edge_cons(states[i]))
                    end
                end
            end
        else
            died::Vector{Bool} = sim.initialized ?
                simfield(sim, T).read.died :
                simfield(sim, T).write.died
            
            for i in 1:length(died)
                if died[i] == false
                    if ! ignore_ids
                        push!(ids, agent_id(typeid(sim, T), AgentNr(i)))
                    end
                    push!(poss, pos_func(states[i]))
                    if edge_cons !== nothing
                        if typeof(edge_cons) == DataType
                            push!(edges, edge_cons())
                        else
                            push!(edges, edge_cons(states[i]))
                        end
                    end
                end
            end
        end
    else
        living = if has_hint(sim, T, :Immortal, :Agent)
            for i in 1:length(states)
                if filter_pred(states[i])
                    if ! ignore_ids
                        push!(ids, agent_id(typeid(sim, T), AgentNr(i)))
                    end
                    push!(poss, pos_func(states[i]))
                    if edge_cons !== nothing
                        if typeof(edge_cons) == DataType
                            push!(edges, edge_cons())
                        else
                            push!(edges, edge_cons(states[i]))
                        end
                    end
                end
            end
        else
            died = sim.initialized ?
                simfield(sim, T).read.died :
                simfield(sim, T).write.died  

            for i in 1:length(died)
                if died[i] == false && filter_pred(states[i])
                    if ! ignore_ids
                        push!(ids, agent_id(typeid(sim, T), AgentNr(i)))
                    end
                    push!(poss, pos_func(states[i]))
                    if edge_cons !== nothing
                        if typeof(edge_cons) == DataType
                            push!(edges, edge_cons())
                        else
                            push!(edges, edge_cons(states[i]))
                        end
                    end
                end
            end
        end
    end

    if must_join && mpi.active
        if ! ignore_ids
            ids = join(ids)
        end
        poss = join(poss)
        if edge_cons !== nothing
            edges = join(edges)
        end
    end

    e = edge_cons !== nothing ? edges : nothing
    if ignore_ids
        (nothing, poss, e)
    else
        (ids, poss, e)
    end
end


function _make_pos_func_val(::Type{T}, ::Val{fieldname}) where {T, fieldname}
    state::T -> getfield(state, fieldname)
end


# TODO update doc
# TODO: add tests for edge_constructor with state and
# from_filter. Write documentation

function _get_ids_poss_states(sim, types, pos_field, filter, edge_constructor;
                       ignore_ids = false)
    _log_info(sim, "<Begin> _get_ids_poss_states!")

    types = applicable(iterate, types) ? types : [ types ]

    pos_funcs = map(t -> _make_pos_func_val(t, Val(pos_field)), types)

    (ids, poss, edges) =
        _agents_ids_states_and_edges(sim, types[1],
                                     pos_funcs[1], filter,
                                     edge_constructor, true;
                                     ignore_ids)

    if length(types) > 1
        for n in 2:length(types)
            i, p, e =
                _agents_ids_states_and_edges(sim, types[n],
                                             pos_funcs[n], filter,
                                             edge_constructor, true)
            append!(ids, i)
            append!(poss, p)
            if edge_constructor !== nothing
                append!(edges, e)
            end
        end
    end

    _log_info(sim, "<End> _get_ids_poss_states!")

    (ids, poss, edges)
end


function _create_kdtree!(sim,
                  from_poss,
                  periodic_lower,
                  periodic_upper,
                  metric,
                  leafsize,
                  reorder)
    _log_info(sim, "<Begin> _create_kdtree!")

    if periodic_upper !== nothing
        if periodic_lower === nothing
            periodic_lower = fill(0.0, length(periodic_upper)) |>
                SVector{length(periodic_upper)}
        end
    end

    matrix = reduce(hcat, from_poss)
    if eltype(matrix) <: Int
        matrix = Float64.(matrix)
        if periodic_upper !== nothing
            periodic_lower = map(l -> Float64(l), periodic_lower)
            periodic_upper = map(u -> Float64(u + 1), periodic_upper)
        end
    end
    kdtree = KDTree(matrix, metric; leafsize=leafsize, reorder=reorder)

    if periodic_upper !== nothing
        kdtree = PeriodicTree(kdtree, periodic_lower, periodic_upper)
    end
    
    _log_info(sim, "<End> _create_kdtree!")

    kdtree
end

Base.@kwdef struct SpatialNeighbors
    agenttypes::Union{Vector{DataType}, DataType}
    state_func = identity
    pos_field = :pos
    filter::Union{Function, Nothing} = nothing
    periodic_lower::Union{Vector, Nothing} = nothing
    periodic_upper::Union{Vector, Nothing} = nothing
    metric::Any = Euclidean()
    leafsize = 25
    reorder = false
end

_spatial_agenttypes(sn::SpatialNeighbors) =
    sn.agenttypes isa DataType ? (sn.agenttypes,) : sn.agenttypes

SpatialNeighbors(agenttypes) = SpatialNeighbors(agenttypes = agenttypes)    
SpatialNeighbors(agenttypes, periodic_upper) =
    SpatialNeighbors(agenttypes = agenttypes, periodic_upper = periodic_upper)

"""
    connect_spatial_neighbors!(sim, from_type::DataType, to_type::DataType, edge_constructor; distance = 1.0, periodic = true, fieldname = :Creates)

pos edges between agents based on their spatial proximity.

Connects agents of type `from_type` to agents of type `to_type` when
they are within `distance` of each other in spatial coordinates. The
agent position is read from the field specified by `fieldname`
(defaults to `:pos`).

The connections are created with edges of type `edge_type`. The
`edge_type` must be stateless and registered like all
other edgetypes via `register_edgetype!`.


See also [`add_raster!`](@ref) and [`connect_raster_neighbors!`](@ref)
"""
function connect_spatial_neighbors!(sim,
                             from_types,
                             to_types,
                             edge_constructor;
                             add_existing = false,
                             from_pos_field::Symbol = :pos,
                             to_pos_field::Symbol = :pos,
                             from_filter = nothing,
                             to_filter = nothing,
                             distance = 1.0,
                             periodic_lower = nothing,
                             periodic_upper = nothing,
                             metric = Euclidean(),
                             leafsize = 25,
                             reorder = true)

    with_logger(sim) do
        @info "<Begin> connect_spatial_neighbors!" from_types to_types
    end

    (from_ids, from_poss, from_edges) = _get_ids_poss_states(sim,
                                                             from_types,
                                                             from_pos_field,
                                                             from_filter,
                                                             edge_constructor)


    function search_func(kdtree, pos, from_ids, from_states, to)
        for fidx in inrange(kdtree, pos, distance)
            if from_ids[fidx] != to
                add_edge!(sim, from_ids[fidx], to, from_edges[fidx])
            end
        end
    end

    # Prepare writing edges if simulation is not initialized
    edge_type = if typeof(edge_constructor) != DataType
        # edge_constructor is a function, get the type from the first edge
        typeof(from_edges[1])
    else
        # edge_constructor is a DataType, use it directly
        edge_constructor
    end
    
    if sim.initialized
        prepare_write!(sim, [], add_existing, edge_type)
    end
    sim.intransition = true

    if length(from_ids) > 0
        kdtree = _create_kdtree!(sim,
                                 from_poss,
                                 periodic_lower,
                                 periodic_upper,
                                 metric,
                                 leafsize,
                                 reorder)


        


        # collect the ids and pos vectors
        (to_ids, to_poss) = _get_ids_poss_states(sim,
                                                 to_types,
                                                 to_pos_field,
                                                 to_filter,
                                                 nothing)

        # iterate over the ids and search for neighbors
        if length(to_ids) > 0
            for (to_id, pos) in zip(to_ids, to_poss)
                # we construct the edges in the search_func
                search_func(kdtree, collect(pos), from_ids, from_edges, to_id)
            end
        end
    end
    sim.intransition = false
    # Finish writing edges if simulation is not initialized
    if sim.initialized
        finish_write!(sim, edge_type)
    end

    _log_info(sim, "<End> connect_spatial_neighbors!")
end

function periodic_diff(to::SVector{N, Float64}, from::SVector{N, Float64}, 
                periodic_upper::SVector{N, Float64}) where N
    map(from, to, periodic_upper) do f, t, upper
        dist = t - f
        abs(dist) > upper/2 ? dist - upper * sign(dist) : dist
    end
end

function periodic_diff(to::SVector{N, Float64}, from::SVector{N, Float64}, 
                periodic_lower::SVector{N, Float64},
                periodic_upper::SVector{N, Float64}) where N
    map(from, to, periodic_lower, periodic_upper) do f, t, low, upper
        range = upper - low
        dist = t - f
        abs(dist) > range/2 ? dist - range * sign(dist) : dist
    end
end

function periodic_diff(to::SVector{N, Float64}, from::SVector{N, Float64}, 
                periodic_boundaries::NTuple{2, SVector{N, Float64}}) where N
    periodic_diff(to, from, periodic_boundaries[1], periodic_boundaries[2])
end

function periodic_clamp(pos::SVector{N, T},
                 periodic_lower::SVector{N, T},
                 periodic_upper::SVector{N, T}) where {N, T}
    mod.(pos .- periodic_lower, periodic_upper .- periodic_lower) .+
        periodic_lower
end

function periodic_clamp(pos::SVector{N, T}, 
                 periodic_upper::SVector{N, T}) where {N, T}
    mod.(pos, periodic_upper) 
end

struct NeighborsIterator{T}
    nstates::Vector{T}
    fidx::Vector{Int}
    len::Int
end

function iterate(niter::NeighborsIterator{T})::Union{Tuple{T, Int64}, Nothing} where T
    iterate(niter, 1)
end

function iterate(niter::NeighborsIterator{T}, idx::Int)::Union{Tuple{T, Int64}, Nothing} where T
    if niter.len >= idx
        (niter.nstates[niter.fidx[idx]], idx + 1)
    else
        nothing
    end
end

Base.eltype(::Type{NeighborsIterator{T}}) where T = T
Base.length(niter::NeighborsIterator{T}) where T = niter.len
Base.IteratorSize(::Type{NeighborsIterator{T}}) where T = Base.HasLength()
Base.IteratorEltype(::Type{NeighborsIterator{T}}) where T = Base.HasEltype()

function find_neighbors_iter(sim, pos::SVector{N, T}, distance, ::Type{AT}, ::Type{ST})::NeighborsIterator{ST} where {N, T, AT, ST}
    @mayassert simfield(sim, AT).prepared_spatial_neighbors """
    $AT is not element of the `spatial_neighbors` keyword of apply(!)
    """

    if sim.neighbors_infos[AT].empty
        NeighborsIterator(ST[], Int[], 0)
    else
        ni::NeighborsInfo{ST} = sim.neighbors_infos[AT]
        fidx = inrange(ni.kdtree, pos, distance)
        NeighborsIterator(ni.states, fidx, length(fidx))
    end
end

function find_neighbors(sim, pos::SVector{N, T}, distance, ::Type{AT}) where {N, T, AT}
    @mayassert simfield(sim, AT).prepared_spatial_neighbors """
    $AT is not element of the `spatial_neighbors` keyword of apply(!)
    """
    ni = sim.neighbors_infos[AT]

    if ni.empty
        similar(ni.states, 0)
    else
        map(id -> ni.states[id], inrange(ni.kdtree, pos, distance))
    end
end

# the state_func can be given with an anonymous function like: state -> state.count
# but then it is not possible to inference the return type of this function.
# But we know the type of state, and add this information here
function _make_state_func_val(::Type{T}, state_func) where {T}
    state::T -> state_func(state)
end

function prepare_spatial_neighbors!(sim, sn)
    # TODO: test reuse of existing Infos (and removing when changed)
    
    if sn !== nothing
        for at in _spatial_agenttypes(sn)
            if ! haskey(sim.neighbors_infos, at) ||
                sim.neighbors_infos[at].snhash != hash(sn)

                state_func = _make_state_func_val(at, sn.state_func)
                
                (_, po, st) = _get_ids_poss_states(sim, at, sn.pos_field,
                                                   sn.filter, state_func;
                                                   ignore_ids = true)

                if length(st) > 0                
                    kdtree = _create_kdtree!(sim, po,
                                             sn.periodic_lower, sn.periodic_upper,
                                             sn.metric, sn.leafsize, sn.reorder)
                    sim.neighbors_infos[at] =
                        NeighborsInfo{typeof(first(st))}(kdtree, st, hash(sn), false)
                else
                    sim.neighbors_infos[at] = NeighborsInfo(sn)
                end
            end
            simfield(sim, at).prepared_spatial_neighbors = true
        end
    end
end

function finish_spatial_neighbors!(sim, sn, write)
    if sn !== nothing
        for at in _spatial_agenttypes(sn)
            simfield(sim, at).prepared_spatial_neighbors = false
        end
    end
    for w in write
        if haskey(sim.neighbors_infos, w)
            delete!(sim.neighbors_infos, w)
        end
    end
end        
