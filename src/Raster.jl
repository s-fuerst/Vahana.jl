import LinearAlgebra

export add_raster!, connect_raster_neighbors!
export calc_raster, calc_rasterstate, calc_rasterstate_flexible, move_to!

"""
    add_raster!(sim, name::Symbol, dims::NTuple{N, Int}, agent_constructor)

Adds a n-dimensional grid to `sim`, with the dimensions `dims`.

For each cell a new node/agent is added to the graph. To create the
agent, the `agent_constructor` function is called, with the cell position in
form of an `CartesianIndex` as argument.

The symbol `name` is an identifier for the created raster, as it is allowed
to add multiple rasters to `sim`. 

The types of the agents created by the `agent_constructor` must be
already registered via [`register_agenttype!`](@ref).

Returns a vector with the IDs of the created agents.

See also [`calc_raster`](@ref) [`connect_raster_neighbors!`](@ref) and
[`move_to!`](@ref).
"""
function add_raster!(sim,
              name::Symbol,
              dims::NTuple{N, Int},
              agent_constructor) where N

    positions = CartesianIndices(dims)
    nodeids = add_agents!(sim, [ agent_constructor(pos) for pos in positions ]) 

    grid = Array{AgentID, N}(undef, dims)
    for (id, pos) in zip(nodeids, positions)
        grid[pos] = id
    end

    sim.rasters[name] = grid
end

# add_raster!(agent_constructor, sim, name::Symbol, dims::NTuple) =
#     add_raster!(sim, name, dims, agent_constructor)



"""
    connect_raster_neighbors!(sim, name::Symbol, edge_constructor; distance::Int, metric:: Symbol, periodic::Bool)


All cells that are at most `distance` from each other (using the metric
`metric`) are connected with edges, where the edges are created with
the `edge_constructor`.

The `edge_constructor` must be a function with one argument with type
Tuple{CartesianIndex, CartesianIndex}. The first CartesianIndex is the
position of the source node, and the second CartesianIndex the
position of the target node.

Valid metrics are :chebyshev, :euclidean and :manhatten.

The keyword periodic determines whether all dimensions are cyclic
(e.g., in the two-dimensional case, the raster is a torus).

The default values of the optional keyword arguments are 1 for
`distance`, :chebyshev for `metric` and true for `periodic`. which is
equivalent to a Moore neighborhood. The :manhatten metric can be
used to connect cells in the von Neumann neighborhood.

The agent types of agents created by the `agent_constructor` must be
already registered via [`register_agenttype!`](@ref).

See also [`add_raster!`](@ref)
"""
function connect_raster_neighbors!(sim,
                            name::Symbol,
                            edge_constructor;
                            distance = 1,
                            metric::Symbol = :chebyshev,
                            periodic = true)
    # CartesianIndices are immutable, this function "replaces" setindex! 
    replace_idx(ci::CartesianIndex, pos, value) = 
        CartesianIndex([ (i == pos ? value : ci[i]) for i in 1:length(ci) ]...)

    @assert metric in [ :chebyshev, :euclidean, :manhatten ]

    grid = sim.rasters[name]
    dims = size(grid)
    n = ndims(grid)
    d = distance |> floor |> Int
    shift = Iterators.product([ (-d):d for _ in 1:n ]...) |> collect
    shift = reshape(shift, length(shift))
    # remove the zero point from the list, as we do not want to create loops
    filter!(s -> s !== zero(CartesianIndex{n}), shift)
    
    if metric == :euclidean
        filter!(s -> LinearAlgebra.norm(s) <= distance, shift) 
    elseif metric == :manhatten
        filter!(s -> mapreduce(abs, +, s) <= distance, shift)
    end

    for s in map(CartesianIndex, shift)
        for org in keys(grid)
            shifted = org + s
            ignore = false
            for i in 1:length(dims)
                if shifted[i] < 1 || shifted[i] > dims[i]
                    if periodic
                        modval = mod1(shifted[i], dims[i])
                        shifted = replace_idx(shifted, i, modval)
                    else
                        ignore = true
                    end
                end
            end
            if ! ignore
                add_edge!(sim, grid[org], grid[shifted],
                          edge_constructor(org, shifted))
            end
        end
    end
end

"""
    calc_raster(sim, raster::Symbol, f)

Calculate values for the raster `raster` by applying `f` to each
cell ID of the cells constructed by the `add_raster!` function.

If the results of calc_raster depend only on the state of the cells
(as in the following example) and all cells have the same type,
[`calc_rasterstate`](@ref) and [`calc_rasterstate_flexible`](@ref) can
be used as concise alternatives.

Returns a n-dimensional array (with the same dimensions as `raster`)
with those values.

Example

The following code from the "Game of Life" example generates a
Boolean matrix indicating which cells are alive (and therefore
maps the internal graph structure to the usual representation of a
cellular automaton):

```@example
    calc_raster(sim, :raster) do id
        agentstate(sim, id, Cell).active
    end
```

See also [`add_raster!`](@ref) and [`calc_rasterstate`](@ref)
"""
function calc_raster(sim, raster::Symbol, f) 
    map(f, sim.rasters[raster])
end

calc_raster(f, sim, raster::Symbol) = calc_raster(sim, raster, f)


"""
    calc_rasterstate(sim, raster::Symbol, f, t::Type{T})

Combined calc_raster with agentstate for the cells of the raster.

Calculate values for the raster `raster` by applying `f` to the state of each
cell.

Returns a n-dimensional array (with the same dimensions as `raster`)
with those values.

Example

Instead of
```@example
    calc_raster(sim, :raster) do id
        agentstate(sim, id, Cell).active
    end
```
it also possible to just write
```@example
    calc_rasterstate(sim, :raster, s -> s.active, Cell)
```
    
See also [`add_raster!`](@ref) and [`calc_rasterstate`](@ref)
"""
function calc_rasterstate(sim, raster::Symbol, f, t::Type{T}) where T
    map(id -> agentstate(sim, id, t) |> f, sim.rasters[raster])
end

# """
#     calc_rasterstateflexible(sim, raster::Symbol, f, t::Type{T})
# """
# function calc_rasterstate_flexible(sim, raster::Symbol, f)
#     map(id -> agentstate_flexible(sim, id) |> f, sim.rasters[raster])
# end


# this is an Vahana internal function
"""
    cellid(sim, name::Symbol, pos)

Returns the ID of the agent (node) from the raster `name` at the
position `pos`. `pos` must be of type CartesianIndex or a Dims{N}.

See also [`add_raster!`](@ref), [`move_to!`](@ref),
[`add_edge!`](@ref) and [`agentstate`](@ref)
"""
function cellid(sim, name::Symbol, pos)
    sim.rasters[name][CartesianIndex(pos)]
end


"""
    move_to!(sim, name::Symbol, id::AgentID, pos, edge_from_raster, edge_to_raster) 

Creates up to two edges of type between the agent with ID `id` and the cell from the raster `name` at the position `pos`.

`pos` must be of type CartesianIndex or a Dims{N}. 

`edge_from_raster` is the edge that will be added with the cell as
source node and the agent as target node. `edge_from_raster` can be
`nothing`, in this case no edge will be added with the agent as target
node.

`edge_to_raster` is the edge that will be added with the agent as
source node and the cell as target node. `edge_to_raster` can be
`nothing`, in this case no edge will be added with the agent as source
node.

See also [`add_raster!`](@ref), [`raster_nodeid`](@ref) and [`add_edge!`](@ref) 
"""
function move_to!(sim,
           name::Symbol,
           id::AgentID,
           pos,
           edge_from_raster,
           edge_to_raster) 
    posid = cellid(sim, name, pos)
    if ! isnothing(edge_from_raster)
        add_edge!(sim, posid, id, edge_from_raster)
    end
    if ! isnothing(edge_to_raster)
        add_edge!(sim, id, posid, edge_to_raster)
    end
end

# TODO
# move_to! mit Radius (name ist mir unklar)
