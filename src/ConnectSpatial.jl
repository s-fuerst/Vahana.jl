export connect_spatial_neighbors!
export pos_sarray, pos_tuple

using StaticArrays
import Combinatorics: combinations
import NearestNeighbors: KDTree, knn, inrange, Euclidean

### TODO DOC
pos_sarray(fieldname::Symbol) = state -> getproperty(state, fieldname)

pos_tuple(fieldname::Symbol, size) =
    state -> getproperty(state, fieldname) |> SVector{size}


"""
    connect_spatial_neighbors!(sim, from_type::DataType, to_type::DataType, edge_constructor; distance = 1.0, periodic = true, fieldname = :pos)

Creates edges between agents based on their spatial proximity.

Connects agents of type `from_type` to agents of type `to_type` when
they are within `distance` of each other in spatial coordinates. The
agent position is read from the field specified by `fieldname`
(defaults to `:pos`).

The connections are created with edges of type `edge_type`. The
`edge_type` must be stateless and registered like all
other edgetypes via `register_edgetype!`.


See also [`add_raster!`](@ref) and [`connect_raster_neighbors!`](@ref)
"""
# function connect_spatial_neighbors_direct!(sim,
#                              from_type::DataType,
#                              from_posfunc,
#                              to_type::DataType,
#                              to_posfunc,
#                              edge_type;
#                              distance = 1.0,
#                              periodic_boundaries = nothing,
#                              metric = Euclidean(),
#                              leafsize = 25,
#                              reorder = true)
#     # TODO update doc

#     # TODO: assertiongs for distance,periodic bounding,
#     # compare eltype of periodic bounding with eltype of positions
#     with_logger(sim) do
#         @info "<Begin> connect_spatial_neighbors!" from_type to_type distance
#     end

#     from_ids = all_agentids(sim, from_type, true)

#     if length(from_ids) > 0
#         # first we construct the KDTree with the information
#         # of the agents from all processes.
#         from_states = all_agents(sim, from_type, true;
#                                  statemapfunc = from_posfunc)
#         matrix = reduce(hcat, from_states)
#         if eltype(matrix) <: Int
#             matrix = Float64.(matrix)
#         end
#         kdtree = KDTree(matrix, metric; leafsize = 25, reorder = reorder)

#         # Prepare writing edges if simulation is not initialized
#         if sim.initialized
#             prepare_write!(sim, [], false, edge_type)
#         end
#         sim.intransition = true

#         to_ids = all_agentids(sim, to_type, false)
#         to_states = all_agents(sim, to_type, false)
#         to_pos = map(to_posfunc, to_states)

#         for (tidx, pos) in enumerate(to_pos)
#             found = inrange(kdtree, collect(pos), distance)
#             for fidx in found
#                 if from_ids[fidx] != to_ids[tidx]
#                     add_edge!(sim, from_ids[fidx], to_ids[tidx], edge_type())
#                 end
#             end
#         end

#         if periodic_boundaries !== nothing
#             # we start by determining for with dimensions boundaries
#             # are given and calculating from the boundaries tuple
#             # the offset that must be added to the position in
#             # form of a unit_vector.
#             num_dims = length(periodic_boundaries)
#             active = zeros(Bool, num_dims)
#             unit_vectors = fill(SVector{num_dims}(zeros(num_dims)), num_dims)
#             for i in 1:num_dims
#                 if typeof(periodic_boundaries[i]) != Tuple{}
#                     offset = periodic_boundaries[i][2] -
#                         periodic_boundaries[i][1]
#                     o2 = eltype(periodic_boundaries[i]) <: Int ? 1 : 0
#                     unit_vectors[i] = setindex(unit_vectors[i], offset + o2, i)
#                     active[i] = true
#                 end
#             end
#             # then we iterate over all positions
#             for (tidx, pos) in enumerate(to_pos)
#                 adjust_pos = SVector{num_dims}[]
#                 # and checking for which dimensions the agent pos in
#                 # in the distance of a boundary. For this dimensions we
#                 # calculating the unit vectors to the adjust_pos vector
#                 for i in 1:num_dims
#                     if active[i] > 0
#                         if pos[i] - distance < periodic_boundaries[i][1] 
#                             push!(adjust_pos, unit_vectors[i])
#                         elseif pos[i] + distance > periodic_boundaries[i][2]
#                             push!(adjust_pos, -unit_vectors[i])
#                         end
#                     end
#                 end
#                 # finally we create all combinations of the unit_vectors and
#                 # adjust the position for each of this combination, and
#                 # searching for the neighbors
#                 for c in combinations(adjust_pos)
#                     avec = reduce(+, c)
#                     found = inrange(kdtree, collect(pos + avec), distance)
#                     for fidx in found
#                         if from_ids[fidx] != to_ids[tidx]
#                             add_edge!(sim, from_ids[fidx], to_ids[tidx],
#                                       edge_type())
#                         end
#                     end
#                 end
#             end
#         end

#         # TODO: we need a function for this, that also increment
#         # the counter
#         sim.intransition = false
#         # Finish writing edges if simulation is not initialized
#         if sim.initialized
#             finish_write!(sim, edge_type)
#         end
#     end

#     _log_info(sim, "<End> connect_spatial_neighbors!")
# end

function _agents_ids_states_and_edges(sim, ::Type{T},
                               pos_func, filter_pred, edge_cons) where T
    # Note: This function should not be called within a transition function
    @assert fieldcount(T) > 0 """\n
        all_agents can be only called for agent types that have fields.
        To get the number of agents, you can call num_agents instead.
    """
    @assert length(Base.return_types(pos_func)) == 1
    @assert Base.return_types(pos_func)[1] != Vector{Any}
    @assert Base.return_types(pos_func)[1] != Any
    @info Base.return_types(pos_func)[1] 
    
    states = sim.initialized ?
        getproperty(sim, Symbol(T)).read.state : 
        getproperty(sim, Symbol(T)).write.state  

    ids = AgentNr[]
    sizehint!(ids, length(states))

    poss = Vector{Base.return_types(pos_func)[1]}()
    sizehint!(poss, length(states))

    if edge_cons !== nothing
        @assert length(Base.return_types(edge_cons)) == 1
        @assert Base.return_types(edge_cons)[1] != Vector{Any}
        edges = Vector{Base.return_types(edge_cons)[1]}()
        sizehint!(edges, length(states))
    end
    
    if filter_pred === nothing
        if has_hint(sim, T, :Immortal, :Agent)
            for i in 1:length(states)
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

    if mpi.active
        ids = join(ids)
        poss = join(poss)
        if edge_cons !== nothing
            edges = join(edges)
        end
    end

    if edge_cons !== nothing
        (ids, poss, edges)
    else
        (ids, poss)
    end
end


# function _spatial_neighbors!(sim,
#                       from_type::DataType,
#                       from_pos_func,
#                       to_type::DataType,
#                       to_pos_func,
#                       edge_constructor,
#                       search_func;
#                       from_filter = nothing, # inrange or knn for NearestNeighbors
#                       to_filter = nothing,
#                       distance = nothing, # only used for inrange
#                       periodic_boundaries = nothing, # only used for inrange
#                       metric = Euclidean(),
#                       leafsize = 25,
#                       reorder = true)
#     # TODO update doc

#     # TODO: assertiongs for distance,periodic bounding,
#     # compare eltype of periodic bounding with eltype of positions

# end

# TODO: add tests for edge_constructor with state and
# from_filter. Write documentation


function connect_spatial_neighbors!(sim,
                             from_type::DataType,
                             from_pos_func::Function,
                             to_type::DataType,
                             to_pos_func::Function,
                             edge_constructor;
                             from_filter = nothing,
                             to_filter = nothing,
                             distance = 1.0,
                             periodic_boundaries = nothing,
                             metric = Euclidean(),
                             leafsize = 25,
                             reorder = true)
    with_logger(sim) do
        @info "<Begin> connect_spatial_neighbors!" from_type to_type distance
    end

    function search_func(kdtree, pos, distance, from_ids, from_states, to)
        for fidx in inrange(kdtree, pos, distance)
            if from_ids[fidx] != to 
                add_edge!(sim, from_ids[fidx], to, from_edges[fidx])
            end
        end
    end
    
    (from_ids, from_poss, from_edges) =
        _agents_ids_states_and_edges(sim, from_type, from_pos_func, from_filter,
                                    edge_constructor)
    

    if length(from_ids) > 0
        # first we construct the KDTree with the information
        # of the agents from all processes.
        matrix = reduce(hcat, from_poss)
        if eltype(matrix) <: Int
            matrix = Float64.(matrix)
        end
        kdtree = KDTree(matrix, metric; leafsize=25, reorder=reorder)

        # Prepare writing edges if simulation is not initialized
        edge_type = if typeof(edge_constructor) != DataType
            # edge_constructor is a function, get the type from the first edge
            typeof(from_edges[1])
        else
            # edge_constructor is a DataType, use it directly
            edge_constructor
        end

        if sim.initialized
            prepare_write!(sim, [], false, edge_type)
        end
        sim.intransition = true

        (to_ids, to_pos) =
            _agents_ids_states_and_edges(sim, to_type, to_pos_func, to_filter,
                                         nothing)

        for (to_id, pos) in zip(to_ids, to_pos)
            search_func(kdtree, collect(pos), distance, from_ids, from_edges, to_id)
        end

        if periodic_boundaries !== nothing
            # we start by determining for with dimensions boundaries
            # are given and calculating from the boundaries tuple
            # the offset that must be added to the position in
            # form of a unit_vector.
            num_dims = length(periodic_boundaries)
            active = zeros(Bool, num_dims)
            unit_vectors = fill(SVector{num_dims}(zeros(num_dims)), num_dims)
            for i in 1:num_dims
                if typeof(periodic_boundaries[i]) != Tuple{}
                    offset = periodic_boundaries[i][2] -
                        periodic_boundaries[i][1]
                    o2 = eltype(periodic_boundaries[i]) <: Int ? 1 : 0
                    unit_vectors[i] = setindex(unit_vectors[i], offset + o2, i)
                    active[i] = true
                end
            end
            # then we iterate over all positions
            for (tidx, (to_id, pos)) in enumerate(zip(to_ids, to_pos))
                adjust_pos = SVector{num_dims}[]
                # and checking for which dimensions the agent pos in
                # in the distance of a boundary. For this dimensions we
                # calculating the unit vectors to the adjust_pos vector
                for i in 1:num_dims
                    if active[i] 
                        if pos[i] - distance < periodic_boundaries[i][1]
                            push!(adjust_pos, unit_vectors[i])
                        elseif pos[i] + distance > periodic_boundaries[i][2]
                            push!(adjust_pos, -unit_vectors[i])
                        end
                    end
                end
                # finally we create all combinations of the unit_vectors and
                # adjust the position for each of this combination, and
                # searching for the neighbors
                for c in combinations(adjust_pos)
                    if c != Any[]
                        avec = reduce(+, c)
                        search_func(kdtree, collect(pos + avec), distance, 
                                    from_ids, from_edges, to_id)
                    end
                end
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

function _make_pos_func_val(::Type{T}, ::Val{fieldname}) where {T, fieldname}
    return state::T -> SVector(getfield(state, fieldname))
end

# Neue Methode die Symbole akzeptiert
function connect_spatial_neighbors!(sim,
                             ::Type{FromType},
                             from_pos_field::Symbol,
                             ::Type{ToType},
                             to_pos_field::Symbol,
                             edge_constructor;
                             kwargs...) where {FromType, ToType}

    from_pos_func = _make_pos_func_val(FromType, Val(from_pos_field))
    to_pos_func = _make_pos_func_val(ToType, Val(to_pos_field))

    
    # Rufe die ursprüngliche Funktion auf
    connect_spatial_neighbors!(sim, FromType, from_pos_func, 
                              ToType, to_pos_func, edge_constructor;
                              kwargs...)
end
