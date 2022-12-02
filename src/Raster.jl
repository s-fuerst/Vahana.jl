import LinearAlgebra

export add_raster!, connect_raster_neighbors!
export calc_raster, calc_rasterstate, move_to!

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

Can be only called before [`finish_init!`](@ref).

See also [`calc_raster`](@ref) [`connect_raster_neighbors!`](@ref) and
[`move_to!`](@ref).
"""
function add_raster!(sim,
              name::Symbol,
              dims::NTuple{N, Int},
              agent_constructor) where N
    @assert ! sim.initialized "add_raster! can be only called before finish_init!"

    grid = Array{AgentID, N}(undef, dims)

    positions = CartesianIndices(dims)
    nodeids = add_agents!(sim, [ agent_constructor(pos) for pos in positions ]) 
    
    for (id, pos) in zip(nodeids, positions)
        grid[pos] = id
    end


    sim.rasters[name] = grid
end

# while distributing the Agents to the different PEs, the AgentIDs are
# changed, so the ids of the grid most be updated
function broadcastids(sim, raster, idmapping::Dict)
    if mpi.isroot
        sim.rasters[raster] = map(id -> idmapping[id], sim.rasters[raster])
    end
    MPI.Bcast!(sim.rasters[raster], MPI.COMM_WORLD)
end

function _stencil_core(metric, n::Int64, distance, d::Int64)
    @assert metric in [ :chebyshev, :euclidean, :manhatten ]

    stencil = Iterators.product([ (-d):d for _ in 1:n ]...) |> collect
    stencil = reshape(stencil, length(stencil))
    # remove the zero point from the list, as we do not want to create loops
    filter!(s -> s !== Tuple(zeros(Int64, n)), stencil)

    if metric == :euclidean
        filter!(s -> LinearAlgebra.norm(s) <= distance, stencil) 
    elseif metric == :manhatten
        filter!(s -> mapreduce(abs, +, s) <= distance, stencil)
    end
    map(CartesianIndex, stencil)
end


const stencil_cache = Dict{Tuple{Symbol, Int64, Int64}, Array{CartesianIndex}}()

function stencil(metric, n::Int64, distance::Int64)
    get!(stencil_cache, (metric, n, distance)) do
        _stencil_core(metric, n, distance, distance)
    end
end


function stencil(metric, n::Int64, distance::Float64)
    d = distance |> floor |> Int
    _stencil_core(metric, n, distance, d)
end



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
    # before a simulation is initialized, the raster existing
    # only on the root 
    if mpi.isroot || sim.initialized
        raster = sim.rasters[name]
        dims = size(raster)
        st = stencil(metric, ndims(raster), distance)

        for org in keys(raster)
            for s in st
                shifted = _checkpos(org + s, dims, periodic)
                if ! isnothing(shifted)
                    add_edge!(sim, raster[org], raster[shifted],
                              edge_constructor(org, shifted))
                end
            end
        end
    end
end

"""
    calc_raster(sim, raster::Symbol, f, f_returns::DataType, accessible::Vector{DataType})

Calculate values for the raster `raster` by applying `f` to each
cell ID of the cells constructed by the `add_raster!` function.

`f_returns` must be the type returned by the function f. There must be an
implementation of the zero function for this type, and zero(returntype) | f(id)
must be equal to f(id).

`accessible` is a vector of Agent and/or Edge types. This vector must
list all types that are accessed directly (e.g. via
[`agentstate`](@ref) or indirectly (e.g. via [`neighborstates`](@ref)
in the transition function.

If the results of `calc_raster` depend only on the state of the cells
(as in the following example) and all cells have the same type,
[`calc_rasterstate`](@ref) can be used as concise alternatives.

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

Can be only called after [`finish_init!`](@ref).

See also [`add_raster!`](@ref) and [`calc_rasterstate`](@ref)
"""
function calc_raster(sim, raster::Symbol, f, f_returns::DataType, accessible::Vector{DataType})
    @assert sim.initialized "calc_raster can be only called after finish_init!"
    foreach(T -> prepare_read!(sim, T), accessible)
    z = zero(f_returns)
    rs = map(sim.rasters[raster]) do id
        if process_nr(id) == mpi.rank
            f(id)
        else
            z
        end
    end
    foreach(T -> finish_read!(sim, T), accessible)
    MPI.Allreduce(rs, |, MPI.COMM_WORLD)
end


calc_raster(f, sim, raster::Symbol, f_returns::DataType,
            accessible::Vector{DataType}) =
                calc_raster(sim, raster, f, f_returns, accessible)


"""
    calc_rasterstate(sim, raster::Symbol, f, f_returns::DataType, ::Type{T})

Combined calc_raster with agentstate for the cells of the raster.

Calculate values for the raster `raster` by applying `f` to the state of each
cell.

`f_returns` must be the type returned by the function f. There must be an
implementation of the zero function for this type, and zero(returntype) | f(state)
must be equal to f(state).

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

Can be only called after [`finish_init!`](@ref).
    
See also [`add_raster!`](@ref) and [`calc_rasterstate`](@ref)
"""
function calc_rasterstate(sim, raster::Symbol, f, f_returns::DataType, ::Type{T}) where T
    @assert sim.initialized """
    calc_rasterstate can be only called after finish_init!"""

    disable_transition_checks = true
    z = zero(f_returns)
    rs = map(sim.rasters[raster]) do id
        if process_nr(id) == mpi.rank
            f(agentstate(sim, id, T))
        else
            z
        end
    end
    disable_transition_checks = false
    MPI.Allreduce(rs, |, MPI.COMM_WORLD)
end

"""
    cellid(sim, name::Symbol, pos)

Returns the ID of the agent (node) from the raster `name` at the
position `pos`. `pos` must be of type CartesianIndex or a Dims{N}.

See also [`add_raster!`](@ref), [`move_to!`](@ref),
[`add_edge!`](@ref) and [`agentstate`](@ref)

Can be only called after [`finish_init!`](@ref).

"""
function cellid(sim, name::Symbol, pos)
    @assert sim.initialized "cellid can be only called after finish_init!"
    sim.rasters[name][CartesianIndex(pos)]
end


"""
    move_to!(sim, name::Symbol, id::AgentID, pos, edge_from_raster, edge_to_raster; distance = 0, metric = :chebyshev, periodic = true) 

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

Using the keyword arguments, it is possible to add additional edges to
the surroundings of the cell at position `pos` in the same raster,
i.e. to all cells at distance `distance` under metric `metric`, where
valid metrics are :chebyshev, :euclidean and :manhatten`. And the
keyword `periodic` determines whether all dimensions are cyclic.

See also [`add_raster!`](@ref) and [`connect_raster_neighbors!`](@ref) 
"""
function move_to!(sim,
                  name::Symbol,
                  id::AgentID,
                  pos,
                  edge_from_raster,
                  edge_to_raster;
                  distance = 0,
                  metric::Symbol = :chebyshev,
                  periodic = true)
    # before a simulation is initialized, the raster existing
    # only on the root 
    if mpi.isroot || sim.initialized
        raster = sim.rasters[name]
        dims = size(raster)
        st = stencil(metric, ndims(raster), distance)
        pos = CartesianIndex(pos)

        if ! isnothing(edge_from_raster)
            add_edge!(sim, raster[pos], id, edge_from_raster)
        end
        if ! isnothing(edge_to_raster)
            add_edge!(sim, id, raster[pos], edge_to_raster)
        end
        
        if distance >= 1 
            for s in st
                shifted = _checkpos(CartesianIndex(pos) + s, dims, periodic)
                if ! isnothing(shifted)
                    if ! isnothing(edge_from_raster)
                        add_edge!(sim, raster[shifted], id, edge_from_raster)
                    end
                    if ! isnothing(edge_to_raster)
                        add_edge!(sim, id, raster[shifted], edge_to_raster)
                    end
                end
            end
        end
    end
end

function _checkpos(pos::CartesianIndex, dims, periodic)
    cpos = Array{Int64}(undef, length(dims))
    outofbounds = false
    for i in 1:length(dims)
        if pos[i] < 1 || pos[i] > dims[i]
            outofbounds = true
            cpos[i] = mod1(pos[i], dims[i])
        else
            cpos[i] = pos[i]
        end
    end
    if ! outofbounds
        pos
    else
        if periodic
            CartesianIndex(cpos...)
        else
            nothing
        end
    end
end
