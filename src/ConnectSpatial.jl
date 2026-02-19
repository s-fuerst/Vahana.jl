export connect_spatial_neighbors!
export pos_sarray, pos_tuple

using StaticArrays
import Combinatorics: combinations
import NearestNeighbors: KDTree, knn, inrange, Euclidean

### TODO DOC
pos_sarray(fieldname::Symbol) = state -> getproperty(state, fieldname)

pos_tuple(fieldname::Symbol, size) =
    state -> getproperty(state, fieldname) |> collect |> SVector{size}


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

function _agents_ids_and_states(sim, ::Type{T}, all_ranks = true;
                           statemapfunc = identity,
                           filterfunc = nothing) where T
    Vahana.@mayassert (! sim.intransition) || all_ranks == false """
    all_agents with all_ranks == true can not be called within a transition function
    """
    @assert fieldcount(T) > 0 """\n
        all_agents can be only called for agent types that have fields.
        To get the number of agents, you can call num_agents instead.
    """
    states = sim.initialized ?
        getproperty(sim, Symbol(T)).read.state : 
        getproperty(sim, Symbol(T)).write.state  

    if filterfunc === nothing
        living = if has_hint(sim, T, :Immortal, :Agent)
            zip([ agent_id(typeid(sim, T), AgentNr(i))
                  for i in 1:length(states) ],
                map(statemapfunc, states))
        else
            died = sim.initialized ?
                getproperty(sim, Symbol(T)).read.died :
                getproperty(sim, Symbol(T)).write.died  

            [ (agent_id(typeid(sim, T), AgentNr(i)), statemapfunc(states[i]))
              for i in 1:length(died) if died[i] == false ]
        end
    else
        living = if has_hint(sim, T, :Immortal, :Agent)
            zip([ agent_id(typeid(sim, T), AgentNr(i))
                  for i in 1:length(states) if filterfunc(states[i]) ],
                map(statemapfunc, states))
        else
            died = sim.initialized ?
                getproperty(sim, Symbol(T)).read.died :
                getproperty(sim, Symbol(T)).write.died  

            [ (agent_id(typeid(sim, T), AgentNr(i)), statemapfunc(states[i]))
              for i in 1:length(died) if died[i] == false && filterfunc(states[i])]
        end
    end

    if all_ranks && mpi.active
        join(living)
    else
        living
    end
end


function _spatial_neighbors!(sim,
                      from_type::DataType,
                      from_posfunc,
                      to_type::DataType,
                      to_posfunc,
                      edge_constructor,
                      search_func,
                      from_filter; # inrange or knn for NearestNeighbors
                      distance = nothing, # only used for inrange
                      periodic_boundaries = nothing, # only used for inrange
                      metric = Euclidean(),
                      leafsize = 25,
                      reorder = true)
    # TODO update doc

    # TODO: assertiongs for distance,periodic bounding,
    # compare eltype of periodic bounding with eltype of positions


    ids_states_iter = _agent_ids_and_states(sim, from_type, from_pos_func, from_filter)
    

    if length(ids_states_iter) > 0
        # first we construct the KDTree with the information
        # of the agents from all processes.
        (from_states, from_pos) = if typeof(edge_constructor) != DataType
            states = all_agents(sim, from_type, true)
            pos = map(from_posfunc, states)
            (states, pos)
        else
            (nothing, all_agents(sim, from_type, true; statemapfunc=from_posfunc))
        end

        matrix = reduce(hcat, from_pos)
        if eltype(matrix) <: Int
            matrix = Float64.(matrix)
        end
        kdtree = KDTree(matrix, metric; leafsize=25, reorder=reorder)

        # Prepare writing edges if simulation is not initialized
        edge_type = if typeof(edge_constructor) != DataType
            typeof(edge_constructor(from_states[1]))
        else
            typeof(edge_constructor())
        end

        if sim.initialized
            prepare_write!(sim, [], false, edge_type)
        end
        sim.intransition = true

        to_ids = all_agentids(sim, to_type, false)
        to_states = all_agents(sim, to_type, false)
        to_pos = map(to_posfunc, to_states)

        for (tidx, pos) in enumerate(to_pos)
            search_func(kdtree, collect(pos),
                        from_ids, from_states, from_filter, to_ids[tidx])
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
            for (tidx, pos) in enumerate(to_pos)
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
                # searching for the neighborsi
                for c in combinations(adjust_pos)
                    if c != Any[]
                        avec = reduce(+, c)
                        search_func(kdtree, collect(pos + avec),
                                    from_ids, from_states, from_filter, to_ids[tidx])
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
end

# TODO: add tests for edge_constructor with state and
# from_filter. Write documentation


function connect_spatial_neighbors!(sim,
                             from_type::DataType,
                             from_posfunc,
                             to_type::DataType,
                             to_posfunc,
                             edge_constructor;
                             from_filter = nothing,
                             distance = 1.0,
                             periodic_boundaries = nothing,
                             metric = Euclidean(),
                             leafsize = 25,
                             reorder = true)
    function search_with_state(kdtree, pos, from_ids, from_states, to)
        found = inrange(kdtree, pos, distance)
        for fidx in found
            from_state = from_states[fidx]
            if from_ids[fidx] != to 
                add_edge!(sim, from_ids[fidx], to, edge_constructor(from_state))
            end
        end
    end

    function search_wout_state(kdtree, pos, from_ids, _, to)
        found = inrange(kdtree, pos, distance)
        for fidx in found
            if from_ids[fidx] != to 
                add_edge!(sim, from_ids[fidx], to, edge_constructor())
            end
        end
    end

    with_logger(sim) do
        @info "<Begin> connect_spatial_neighbors!" from_type to_type distance
    end

    search_func = if typeof(edge_constructor) == DataType 
        search_wout_state
    else
        search_with_state
    end

    filt = if from_filter === nothing
        _ -> true
    else
        from_filter
    end
    
    _spatial_neighbors!(sim,
                        from_type,
                        from_posfunc,
                        to_type,
                        to_posfunc,
                        edge_constructor,
                        search_func,
                        filt;
                        distance,
                        periodic_boundaries,
                        metric,
                        leafsize,
                        reorder)

    _log_info(sim, "<End> connect_spatial_neighbors!")
end
