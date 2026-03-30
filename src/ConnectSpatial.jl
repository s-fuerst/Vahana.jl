export connect_spatial_neighbors!
export periodic_diff, periodic_clamp

using StaticArrays
import Combinatorics: combinations
import NearestNeighbors: PeriodicTree, KDTree, knn, inrange, Euclidean


const _empty_kdtree = KDTree(zeros(1, 0))

struct NeighborsInfo
    kdtree::KDTree
    ids::Vector
    poss::Vector
    edges::Union{Vector, Nothing}
end

NeighborsInfo() = NeighborsInfo(_empty_kdtree, [], [], nothing)

# edge_cons can be also the identity function to get the states instead
function _agents_ids_states_and_edges(sim, ::Type{T}, pos_func, filter_pred,
                               edge_cons, must_join) where T
    # Note: This function should not be called within a transition function
    
    states = sim.initialized ?
        getproperty(sim, Symbol(T)).read.state : 
        getproperty(sim, Symbol(T)).write.state  

    ids = AgentNr[]
    sizehint!(ids, length(states))

    poss = Vector{Base.return_types(pos_func)[1]}()
    sizehint!(poss, length(states))

    if edge_cons !== nothing
        edges = Vector{Base.return_types(edge_cons)[1]}()
        sizehint!(edges, length(states))
    end
    
    if filter_pred === nothing
        if has_hint(sim, T, :Immortal, :Agent)
            for i in 1:length(states)
                push!(ids, agent_id(typeid(sim, T), AgentNr(i)))
                push!(poss, pos_func(states[i]))
                if edge_cons !== nothing && edge_cons !== identity
                    if typeof(edge_cons) == DataType
                        push!(edges, edge_cons())
                    else
                        push!(edges, edge_cons(states[i]))
                    end
                elseif edge_cons == identity
                    edges = states
                end
            end
        else
            died = sim.initialized ?
                getproperty(sim, Symbol(T)).read.died :
                getproperty(sim, Symbol(T)).write.died
            
            for i in 1:length(died)
                if died[i] == false
                    push!(ids, agent_id(typeid(sim, T), AgentNr(i)))
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
                    push!(ids, agent_id(typeid(sim, T), AgentNr(i)))
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
                getproperty(sim, Symbol(T)).read.died :
                getproperty(sim, Symbol(T)).write.died  

            for i in 1:length(died)
                if died[i] == false && filter_pred(states[i])
                    push!(ids, agent_id(typeid(sim, T), AgentNr(i)))
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
        ids = join(ids)
        poss = join(poss)
        if edge_cons !== nothing
            edges = join(edges)
        end
    end

    if edge_cons !== nothing
        (ids, poss, edges)
    else
        (ids, poss, nothing)
    end
end


function _make_pos_func_val(::Type{T}, ::Val{fieldname}) where {T, fieldname}
    state::T -> getfield(state, fieldname)
end

# TODO update doc
# TODO: add tests for edge_constructor with state and
# from_filter. Write documentation

function _get_ids_poss_states(sim, types, pos_field, filter, edge_constructor)
    _log_info(sim, "<Begin> _get_ids_poss_states!")

    types = applicable(iterate, types) ? types : [ types ]

    pos_funcs = map(t -> _make_pos_func_val(t, Val(pos_field)), types)

    (ids, poss, edges) =
        _agents_ids_states_and_edges(sim, types[1],
                                     pos_funcs[1], filter,
                                     edge_constructor, true)

    if length(types) > 1
        for n in 2:length(types)
            i, p, e =
                _agents_ids_states_and_edges(sim, types[n],
                                             pos_funcs[n], filter,
                                             edge_constructor, true)
            append!(ids, i)
            append!(poss, p)
            if edge_cons !== nothing
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
                             periodic_lower::Union{SVector{N, T1}, Nothing} = nothing,
                             periodic_upper::Union{SVector{N, T2}, Nothing} = nothing,
                             metric = Euclidean(),
                             leafsize = 25,
                             reorder = true) where {N, T1, T2}

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

    
    if length(from_ids) > 0
        kdtree = _create_kdtree!(sim,
                                 from_poss,
                                 periodic_lower,
                                 periodic_upper,
                                 metric,
                                 leafsize,
                                 reorder)


        
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
        
        sim.intransition = false
        # Finish writing edges if simulation is not initialized
        if sim.initialized
            finish_write!(sim, edge_type)
        end
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
    vector_from_to(from, to, pb[1], pb[2])
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

