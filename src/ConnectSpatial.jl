export connect_spatial_neighbors!
export pos_sarray, pos_tuple

using StaticArrays
###
pos_sarray(fieldname::Symbol) = state -> getproperty(state, fieldname)

pos_tuple(fieldname::Symbol, size) =
    state -> getproperty(state, fieldname) |> collect |> SVector{size}

# struct Person2
#   foo::Tuple{Int64,Int64}
#   bar::Vector{Int64}
# end

# p = Person2((1, 1), [2, 2])

# pos_tuple(:foo)(p)

###

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

The keyword `periodic` determines whether the space has periodic boundaries,
however this feature is not yet implemented.

See also [`add_raster!`](@ref) and [`connect_raster_neighbors!`](@ref)
"""
function connect_spatial_neighbors!(sim,
                             from_type::DataType,
                             from_posfunc,
                             to_type::DataType,
                             to_posfunc,
                             edge_type;
                             distance = 1.0,
                             periodic = true)
    # leafsize,
    # reorder)

    with_logger(sim) do
        @info "<Begin> connect_spatial_neighbors!" from_type to_type distance
    end

    # TODO: periodic is not implemented

    from_ids = all_agentids(sim, from_type, true)

    if length(from_ids) > 0
        # first we construct the KDTree with the information
        # of the agents from all processes.
        from_states = all_agents(sim, from_type, true;
                                 statemapfunc = from_posfunc)
        matrix = reduce(hcat, from_states)
        if eltype(matrix) <: Int
            matrix = Float64.(matrix)
        end
        kdtree = KDTree(matrix)

        # Prepare writing edges if simulation is not initialized
        if sim.initialized
            prepare_write!(sim, [], false, edge_type)
        end
        sim.intransition = true

        to_ids = all_agentids(sim, to_type, false)
        to_states = all_agents(sim, to_type, false)
        to_pos = map(to_posfunc, to_states)

        for (tidx, pos) in enumerate(to_pos)
            found = inrange(kdtree, collect(pos), distance)
            for fidx in found
                if from_ids[fidx] != to_ids[tidx]
                    add_edge!(sim, from_ids[fidx], to_ids[tidx], edge_type())
                end
            end
        end

        # TODO: we need a function for this, that also increment
        # the counter
        sim.intransition = false
        # Finish writing edges if simulation is not initialized
        if sim.initialized
            finish_write!(sim, edge_type)
        end
    end

    _log_info(sim, "<End> connect_spatial_neighbors!")
end
