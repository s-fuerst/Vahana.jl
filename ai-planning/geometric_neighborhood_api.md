# Geometric Neighborhood: Consolidated API Design

The basis is `~/mdu2/texte/plan_geometric_neighborhood/geometric_neighborhood.tex` as well as the existing implementation in `src/ConnectSpatial.jl`, `src/ModelTypes.jl` and `src/Simulation.jl`.

**Status:** This text summarizes the discussed decisions. The space types and a part of the geometric helpers are implemented in `src/GeometricSpace.jl` and tested independently; integration into Vahana and further functions are still pending. Examples for neighborhood search continue to describe the planned API. Smaller internal details and future convenience functions will be clarified during implementation.

## 1. Basic Idea and Scope

Geometric neighborhoods are used within regular `apply!` transitions via the `geom_*` search functions. The transition can process projected neighbor states or create graph edges with the found agent IDs.

The following applies:

- Only the position of the searching agent is considered. The position is provided by the `posfunc` of the selected `GeometricNeighborhood`.
- The radius is defined in the neighborhood definition, not during the search call.
- Neighborhoods and their backends are registered in `ModelTypes` before `create_model`.
- `apply!` selects registered neighborhoods. Search calls within the transition do not communicate with each other.
- The first implementation introduces the new API using the existing **globally replicated KD-tree principle as backend**. Each rank maintains its own data copy and its own KD-tree. This backend is initially used for testing and validating the new API.
- The previous `spatial_neighbors` API path is abandoned, not continued in parallel as a legacy API. The KD-tree search principle is adopted, not the old interface.
- A node-local shared Cell-list backend will be added only after successful API validation. It replicates data per node instead of per rank.
- There is no cache between `apply!` calls initially. Temporary search data and backend-dependent resources are built up and released per call.
- Neighborhood search is only available after `finish_init!`.

The previous free position search `find_neighbors(sim, pos, distance, AgentType)` is not part of the new API. `connect_spatial_neighbors!` will follow later as a convenience function using the same mechanism, not as an independent search path.

## 2. Geometric Spaces

### Space Types and Constructors

Different space types are derived from a common abstract base type:

```julia
abstract type GeometricSpace end

# Concrete parametric subtypes:
# GridSpace{...} <: GeometricSpace
# ContinuousSpace{...} <: GeometricSpace
```

Both constructors receive a pair `(lower, upper)` per dimension. The space describes only the geometry: boundaries, periodicity, and metric. It contains neither a position coordinate type nor a position function; the latter belongs to the `GeometricNeighborhood`:

```julia
grid = GridSpace(
    [(0, 6), (2, 7), (1, 4)];
    periodic = true,
    metric = Euclidean(),
)

continuous = ContinuousSpace(
    [(0.0, 6.0), (2.0, 7.0), (1.0, 4.0)];
    periodic = (true, false, true),
    metric = Euclidean(),
)
```

The meaning of the boundaries is determined by the space type, not by the numeric type of the boundary values:

- **`GridSpace`:** inclusive integer raster boundaries, initially with step size `1`. In the example, the raster sizes are `(7, 6, 4)`. The period length is `upper - lower + 1`.
- **`ContinuousSpace`:** geometric extent `(6, 5, 3)` in the example. The period length is `upper - lower`. On periodic axes, the canonical range is `[lower, upper)`.

The public API does not require `SVector`s for boundary specification. Internally, boundaries and positions are statically represented; the dimension is determined by the number of boundary pairs.

### Periodicity and Invalid Positions

`periodic` accepts a `Bool` for all dimensions or a tuple with one `Bool` per dimension:

```julia
periodic = true
periodic = false
periodic = (true, false, true)
```

Internally, this is normalized to `NTuple{N,Bool}`. Wrapping and periodic distance calculation only affect periodic axes.

**Positions outside non-periodic boundaries are rejected**, not clipped, reflected, or silently shifted. Periodic axes use their periodic normalization. Dimension, valid values, and representability must be validated; values such as `NaN` or infinite coordinates are invalid.

### Position Mapping in Neighborhood

Each `GeometricNeighborhood` receives a `posfunc` that maps agent states to positions in the referenced space. The same function can have multiple type-specific methods:

```julia
struct Predator
    location::SVector{2, Float32}
    energy::Float64
end

struct Prey
    coordinates::SVector{2, Float32}
    age::Int
end

physical_position(a::Predator) = a.location
physical_position(a::Prey) = a.coordinates

physical = ContinuousSpace(
    [(0, 100), (0, 100)];
    periodic = true,
)
```

Here, different fields are read without type checks in a common function implementation. Vahana's separate state arrays per concrete agent type allow specialized loops and inlining of the appropriate method.

`physical_position` is passed to the neighborhood as `posfunc` (see Section 3). Multiple neighborhoods can use the same space with different position functions. For example, a person has both a housing and a workplace position in the same city space:

```julia
city = ContinuousSpace([(0.0, 100.0), (0.0, 100.0)]; periodic = false)

near_home = GeometricNeighborhood(
    city, p -> p.home_position, Person, Person;
    radius = 2.0,
    project = p -> p.home_position,
)

near_work = GeometricNeighborhood(
    city, p -> p.work_position, Person, Person;
    radius = 2.0,
    project = p -> p.work_position,
)
```

Here, `Person` refers to an agent type with both position fields. The position mapping is neighborhood-specific, while the space geometry is shared. Different spaces are also possible, for example, for physical position and opinion.

### Coordinate Type and Type Derivation

**The default coordinate type of a registered neighborhood is derived from the return type of its `posfunc`, not from the boundary values of the space.** For the `physical_position` function shown above with `Predator` and `Prey`, the internal representation is therefore `SVector{2,Float32}`, even though the boundaries of `physical` are specified as integers.

In `register_geom_neighborhood!`, the involved `query_types` and `neighbor_types` are known. For all these types, the neighborhood's `posfunc` is checked and its internal coordinate type is determined. The registration does not require example agents; empty populations must work. The derived type belongs to the validated metadata of the registration; the shared space object is not modified. Different neighborhoods of the same space can use different valid coordinate types.

Valid coordinate categories are:

- **`GridSpace`:** signed integer types of fixed width, in particular `Int8`, `Int16`, `Int32`, `Int64`.
- **`ContinuousSpace`:** floating-point types of fixed width, in particular `Float16`, `Float32`, `Float64`.

Complex numbers as well as types with dynamic memory like `BigInt` and `BigFloat` are not supported. Narrow coordinate types shall be preserved, especially to keep replicated position arrays compact.

An explicit specification of the position coordinate type at the space is not intended. Internally, each registered neighborhood has a common coordinate type `T` and positions of type `SVector{N,T}`. Position methods may return tuples or `SVector`s. All coordinates within a position must have the same type. Only between position methods for different agent types will the coordinate types be promoted to a common type. The positions are normalized to this internal representation. Non-uniquely derivable or incompatible position types result in a comprehensible error message at registration instead of `Any` in the search data.

Raster coordinates must not be silently rounded. Actual values and the length of a dynamic result vector may only be checked during preparation.

**Storage and computation type are to be distinguished:** Period lengths, differences, squared distances, and cell indices must not overflow due to overly narrow intermediate computations. Wider computation types may be used without broadly expanding the stored positions.

### Metric

The metric belongs to the space and is stored concretely typed. The default is `Euclidean()`; additionally, `Cityblock()` (Manhattan) and `Chebyshev()` are supported. Weighted metrics do not belong to the first implementation.

Only metrics are supported for which **component-wise minimum-image wrapping is correct**: On each periodic axis, the difference is reduced to a shortest periodic path. Metrics with axis coupling that do not yield the shortest total distance are excluded.

The contract with the backend also includes a **conservative search extent matching the metric**: The selection of cells to be examined must not omit possible hits. Final verification occurs using the chosen metric. Arbitrary distance functions without this contract are not automatically supported.

Space type and index remain independent. Even a continuous space can be indexed by a cell list; a raster space does not enforce Manhattan distance.

## 3. Neighborhood Definition

`GeometricNeighborhood` describes a directed search relationship, the required neighbor data, and their relevance for partitioning:

```julia
nearby_prey = GeometricNeighborhood(
    physical, physical_position, Predator, Prey;
    neighbor_filter = prey -> prey.age > 0,
    radius = 2.0,
    project = prey -> (; prey.coordinates, prey.age),
    weight = 1.0,
)
```

- `space`: referenced spatial geometry, independent of position assignment.
- `posfunc`: pure position function on the readable snapshot for queriers and queried; may differ between neighborhoods of the same space.
- `query_types`: querying agent types.
- `neighbor_types`: queried agent types.
- `radius`: fixed search radius for the first version; not part of later search calls.
- `project`: projection exclusively applied to states of the queried agents.
- `weight`: weight for partitioning, without influence on search results.

An agent-dependent radius is a possible future extension; this would also need to be determined before transition from the readable state. A freely selectable search position is not intended.

### Neighbor Filter

`neighbor_filter` determines which agents qualify as hits; `nothing` means no restriction. The filter is a pure function, supports all `neighbor_types`, and is evaluated during preparation on the full readable state before projection.

Whether a querying agent performs a query is decided by the model transition itself.

### Projection and Result Type

The position function must support both queriers and queried agents. The projection, however, only needs to support `neighbor_types`, possibly through multiple methods. Coordinates need not be part of the projection result.

The projection contract is checked at `register_geom_neighborhood!`:

1. If `project_type` is specified, it is used as the declared result type.
2. Otherwise, Vahana attempts to derive the return type from `project` and each concrete `neighbor_type`, without executing the function on example states.
3. Without explicit specification, the derivation must yield a common concrete result type for all queried types. Otherwise, registration fails with a comprehensible error message and a hint toward `project_type`.
4. The result type must meet backend requirements; for `NodeSharedCellList`, it must particularly be a bitstype.

An explicit specification could, for example, be `project_type = PreyInfo`. It allows registration even when compiler inference cannot determine the concrete type. The actually generated values must conform to the agreed-upon result type.

NamedTuple projections like `prey -> (; prey.coordinates, prey.age)` are regular use cases. Inference is centrally encapsulated and tested. Position and projection methods must already be defined at registration time and are considered fixed for the generated model.

## 4. Registration in `ModelTypes` and Backend Choice

Registration follows Vahana's existing model structure: The first argument is **`ModelTypes`, not a simulation**.

```julia
types = ModelTypes()
# Register agent and edge types ...

register_geom_neighborhood!(
    types,
    :food,
    nearby_prey;
    backend = ReplicatedKDTree(),
)

model = create_model(types, "PredatorPrey")
sim = create_simulation(model)
# Add agents ...
finish_init!(sim)
```

`ModelTypes` receives registration information for spaces and named neighborhoods. The concrete public form of an optionally separate space registration is not yet defined; the neighborhood already references its space via `space`.

**The structural registration boundary is `create_model`, not `finish_init!`.** At this point, names, space and backend types, as well as internal coordinate and projection types must be fixed. Subsequent changes to registered definitions are not intended for the generated model.

The backend is fixed per registered neighborhood. It is neither integrated into the domain-specific `GeometricNeighborhood` nor switched per `apply!`. `ReplicatedKDTree` is the provisional name of the first backend; KD-tree parameters such as `leafsize` and `reorder` belong to its configuration. The later-added `NodeSharedCellList`, for example, contains the cell size. Different model configurations are generated for backend comparisons.

The KD-tree backend must fulfill the same spatial, metric, filter, and snapshot semantics. Supported combinations of the used search library are explicitly to be checked, especially mixed periodicity and narrow coordinate types. An unsupported combination requires correct internal backend adaptation or a clear error, not a silent change of search semantics.

Registration does not build a search index nor transfers neighbor states. It provides definitions and validated type information that is already available for initial partitioning.

## 5. Type-Stable Storage in Generated Simulation Type

Analogous to existing generated structures, `create_model` generates a structure with a field of concrete type per registered neighborhood. The corresponding field in the generated simulation type is also concretely typed:

```julia
sim.geom_neighborhoods.food
```

Each neighborhood field contains its own runtime container:

```julia
mutable struct NeighborhoodRuntime{R<:GeometricRegistration,P}
    registration::R
    prepared::Union{Nothing,P}
end
```

- `registration` contains the name, the `GeometricNeighborhood` definition, the concrete backend configuration, and the resolved coordinate and projection types.
- `prepared` bundles backend-dependent temporary search data: initially private replicated positions, IDs, projected states, and KD-tree; for the later shared-memory backend additionally shared arrays, Cell List, and MPI window management.
- `P` is concretely determined by the registered definitions and the specified backend.
- All runtime data is simulation-specific; different simulations of the same model do not share temporary search data.

The concrete backend type is stored in `R = GeometricRegistration{D,B}`; it does not need to be additionally stored in `prepared`. Functions, spaces, and metrics are stored parametrically, not in abstractly typed fields like `Function` or `Any`.

Access with a literal name, such as `geom_neighborstates_iter(sim, id, :food)`, should be specialized for direct access to the corresponding generated field. An additional neighborhood context as a transition argument is not intended. A runtime-chosen arbitrary name does not automatically have the same inference guarantee.

The design is to be checked by inference checks and a small benchmark. The additional result type of the previous function `find_neighbors_iter` is not a required argument of the new search API.

## 6. Selection in `apply!` and Search Functions

`apply!` selects registered neighborhoods via a symbol or vector of symbols:

```julia
apply!(
    sim, hunt!, Predator, [Predator, Prey], Predator;
    neighborhoods = :food,
)

# Multiple neighborhoods:
# apply!(...; neighborhoods = [:food, :competitors])
```

The default is `neighborhoods = nothing`. A single symbol is internally treated like the vector form. Unknown names are validated before preparation. Tuples and new definitions are not the intended input format here; a backend parameter is omitted.

Only selected neighborhoods are prepared for the call. Registering all neighborhoods does not mean they are prepared on every call.

### Three Access Forms

| Vector | Iterator | One Hit |
|---|---|---|
| `geom_neighbors` | `geom_neighbors_iter` | `(AgentID, projected state)` |
| `geom_neighborstates` | `geom_neighborstates_iter` | projected state |
| `geom_neighborids` | `geom_neighborids_iter` | AgentID |

All six functions take `sim`, the ID of the searching agent, and the registered name:

```julia
geom_neighbors(sim, id, :food)
geom_neighborstates(sim, id, :food)
geom_neighborids(sim, id, :food)

geom_neighbors_iter(sim, id, :food)
geom_neighborstates_iter(sim, id, :food)
geom_neighborids_iter(sim, id, :food)
```

The transition retains its familiar signature:

```julia
function hunt!(self, id, sim)
    for prey in geom_neighborstates_iter(sim, id, :food)
        # prey is the projection result
    end
    # ...
end
```

The variants without `_iter` create a new result vector; the iterator variants avoid this result vector. This does not guarantee an entirely allocation-free search. Internally, the vector variants should be built on the iterator variants with shared spatial search logic.

### Shared Search Semantics

- All access forms refer to the same hits in the readable snapshot at the beginning of `apply!`.
- An agent is a geometric neighbor within distance `<= radius` according to the spatial metric if it satisfies type and filter conditions.
- Self-exclusion is performed based on AgentID. Different agents at the same position remain possible neighbors.
- One hit is generated per found agent. Identical projections from different agents are not deduplicated.
- The result order is not guaranteed.
- Searches with no hits return appropriately typed empty vectors or iterators.
- Neighbor IDs are provided by the infrastructure; they do not need to be part of the projection.
- Iterators are only valid during the respective `apply!`. Changes through transitions do not alter the prepared search data.

## 7. Lifecycle Without Cache

Definitions remain, but the prepared search data exists only for one `apply!`:

1. Validate selected neighborhoods and their readable inputs.
2. Prepare search data based on the readable snapshot: first globally replicate to each rank and build a KD-tree there; for the later shared-memory backend build shared node data and Cell List.
3. Execute transitions. The `geom_*` calls do not perform communication.
4. Ensure no further accesses occur on all involved ranks.
5. Release backend resources and set `prepared` to `nothing`. The KD-tree backend releases its references to private arrays and tree data; the later shared-memory backend must explicitly release its windows and other collective resources collectively.

Between calls, neither valid search data nor shared-memory windows are retained. This initially eliminates a cache invalidation logic for state changes, births, deaths, and migration.

The replication collectives must already be executed consistently on all ranks in the KD-tree backend, even with empty local populations. Local exceptions and validation errors must be handled in an MPI-safe manner.

When using the later Shared-memory backend, this also applies to window allocation, synchronization and deallocation on the involved communicators. A rank must not free memory that others are still accessing. A local `finally` alone does not guarantee safe collective cleanup.

Later reuse of memory or caching is a standalone optimization, not a prerequisite for the first version.

### Internal Backend Contract

This interface is Vahana-internal, not a public plugin API. The model continues to use only the registered neighborhood and the six public search functions. The shared code resolves the name once to a `NeighborhoodRuntime`. Backend methods receive this container, not the name. If a method needs `sim`, `sim` is its first argument.

#### Adding a Backend

1. Define and include a concrete subtype of `GeometricBackend` for the configuration under `src/geom_backends/`. The registration must accept this configuration and call backend validation after common type derivation.
2. Define a concrete type for prepared data. It holds neighbor data or their views, the spatial index, and optionally MPI resources. `geom_prepared_type` must be able to determine this type during model building without collecting data or calling MPI. Even a globally empty neighborhood has a prepared state of this type during transition; `nothing` means "not prepared".
3. Implement preparation, local hit set, result access, and deallocation according to the table below. Methods dispatch via registration or via the backend type in the concrete `NeighborhoodRuntime` type. The shared search code contains no query `if backend isa …`.

| Function | Contract |
|---|---|
| `geom_validate_backend(registration)` | Check backend-specific prerequisites at registration, such as supported space/metric combinations or restrictions on projection types. Clearly reject unsupported combinations; do not silently change search semantics. |
| `geom_prepared_type(registration)` | Return the concrete type `P` for `NeighborhoodRuntime{R,P}`. `P` does not depend on population, rank, or hit count. |
| `geom_prepare!(sim, runtime)` | Call the shared local collector, distribute neighbor data backend-dependently, build the index, and return a `P`. The shared lifecycle stores it in `runtime.prepared`. |
| `geom_hits(runtime, id, position)` | Return an iterable source of **exact hit indices**, e.g., an index vector or a backend iterator. `id` is the ID of the searcher, `position` its search position. Radius, space, and metric come from the registration. No MPI communication, even during iteration. |
| `geom_neighbor_id(runtime, index)` | Read the `AgentID` for the hit index without retrieving the projection. |
| `geom_neighbor_projection(runtime, index)` | Read the projection for the hit index in the registered type `T`. |
| `geom_neighbor_pair(runtime, index)` | Read `(AgentID, Projection)` for **the same** hit index as `Tuple{AgentID,T}`. The backend can load a shared data record once or read two arrays at the same index. No further spatial search occurs. |
| `geom_release!(sim, runtime)` | Free backend resources while `runtime.prepared` is still available. Then, the shared lifecycle sets the field to `nothing`. |

#### Preparation and MPI

`geom_collect_local_neighbors(sim, registration)` is a **shared helper**, not a backend hook. It collects living local neighbors from read buffers per rank, applies `neighbor_filter`, and gathers typed positions, existing agent IDs, and projections. The backend calls the collector during `geom_prepare!`; local data is not an additional argument to `geom_prepare!`. The collector itself does not perform collectives and knows neither the KD-tree nor the Cell List.

`ReplicatedKDTree` replicates the collected neighbor data privately to each rank and builds a tree per rank. `NodeSharedCellList` should hold data and cell index per node, without first creating a global private copy on each rank. Both backends initially replicate the same filtered neighbor set. A later selective transfer changes this preparation step, not the public search API.

The shared `apply!` lifecycle calls preparation and deallocation on all involved ranks in the same neighborhood order. Empty local data is no reason to skip a collective step. The backend is responsible for the order and error paths of its own collectives; a local failure must not block other ranks in the next collective. Release of shared resources occurs only when no involved ranks are still accessing them.

#### Search Path and Iterators

The shared code determines the agent type with `type_of(sim, id)`, checks it against `query_types` using `@mayassert`, reads its read-state with `agentstate(sim, id, agent_type)`, and determines position with `posfunc`. The query uses the ID of the currently executing agent. Whether it is called at all is decided by the model transition.

`geom_hits` may **internally** work with a superset, e.g., agents in clipped cells or candidates from periodic search images. Only hits with distance `<= radius` according to the exact space metric pass the backend boundary, excluding the searcher itself and avoiding duplicate output of the same agent. Backends use the shared spatial distance for necessary exact checks. Hit indices are backend-internal keys; the three result accessors must use the same indexing as `geom_hits`. There is no position access through the backend interface.

`GeomNeighborIterator` lives in shared code. It holds the runtime, hit source, and typed access function. It consumes the hit source and calls **exactly the backend method chosen for the result format** per index. The element type is fixed in the iterator type. The helper `geom_projection_type(runtime)` retrieves the projection type from the concrete types of the runtime, e.g., from the element type of the typed projection storage. It reads no hits and therefore requires no non-empty population. Thus, `collect` also returns a vector of the correct type even without hits. A backend may internally materialize an index vector; `_iter` avoids the public result vector, not necessarily all allocations.

Specifically: `geom_neighborstates(sim, id, :food)` collects `geom_neighborstates_iter(sim, id, :food)` with `collect`. The iterator uses `geom_hits(runtime, id, position)` once and calls `geom_neighbor_projection` only per hit index. For `geom_neighborids_iter`, the access is only `geom_neighbor_id`; for `geom_neighbors_iter`, only `geom_neighbor_pair`. All three iterators use the same common iterator type; their element types are `T`, `AgentID`, or `Tuple{AgentID,T}` respectively.

#### Acceptance of a Backend

Test all six search forms against the same complete reference, including empty results, self-exclusion, periodic multiple candidates, and agents at identical positions. For literal neighborhood names, test the inference of public iterators and vectors with and without hits. For MPI, additionally test empty local populations, unchanged results compared to the serial reference, and full collective allocation and deallocation.

## 8. Partitioning

Through registration in `ModelTypes`, Vahana knows all declared neighborhoods, independent of the selection during the current `apply!`:

- Spaces and neighborhood-specific position mappings (`posfunc`);
- directed type relationships;
- radii and metrics;
- neighbor filters;
- relative weights.

`weight` is part of `GeometricNeighborhood`. The default is `1.0`; valid are finite, non-negative values. `weight = 0` means that the neighborhood is ignored during partitioning only. The search remains unchanged and available.

Weighted contributions from multiple neighborhoods serve as input for interaction weights in the plan. Search frequencies can be considered when choosing `weight`; an automatic determination is not specified. The global ratio between graph and geometry parts, i.e., Alpha, remains a parameter of the partitioner.

The neighbor filter also affects geometric interactions: Excluded search targets are not hits. The concrete approximation by a partitioner remains an implementation question. Data projection, search index, and cell statistics are separate tasks, even though they share geometric infrastructure.

Both the initially rank-replicated KD-tree backend and the later node-local shared backend reduce the geometrically partitioned data volume, but not the fundamentally replicated data volume. Both can serve as baselines for future selective communication.

## 9. Edge Generation and Later Comfort Function

First, spatial edges are generated in regular model transitions:

```julia
for neighbor_id in geom_neighborids_iter(sim, id, :food)
    add_edge!(sim, neighbor_id, id, Neighbor())
end
```

This is merely an example of edge direction. A transition may also use the endpoints in reverse order. If projection data is needed, `geom_neighbors_iter` is available.

`connect_spatial_neighbors!(...)` follows as a comfort function using the same registration, search, and `apply!` mechanism. **The edge direction must be configurable there:** from the searcher to the found neighbor or vice versa. The search direction does not automatically determine the edge direction.

The exact signature and manner of passing registered neighborhoods remain to be specified for the later comfort function. A separate search or communication path is not implemented initially. Like the search itself, the comfort function is only available after `finish_init!`.

## 10. Extension of Graph Neighbor API

Existing functions distinguish agent and edge data:

- `neighborids` / `neighborids_iter`: Neighbor agent IDs.
- `neighborstates` / `neighborstates_iter`: Neighbor agent states.
- `neighborstates_flexible` / `neighborstates_flexible_iter`: Agent states for different neighbor types.
- `edges`: Source ID and **edge state**, not neighbor agent state.
- `edgestates` / `edgestates_iter`: Edge states.

As a separate addition, the following should be included:

```julia
neighbors(sim, id, EdgeType, AgentType)       # Vektor von (AgentID, Agentenzustand)
neighbors_iter(sim, id, EdgeType, AgentType)  # entsprechender Iterator
```

Thus, graph and geometric neighbors offer the same tripartite division: ID plus state, only state, and only ID, each with an iterator variant. For geometric neighbors, "state" means the projection result. This extension is not a prerequisite for the shared index.

## 11. Geometric Helper Functions

The helper functions work with the space object and positions, not full agent states. Bounds, periodicity per dimension, and metric are taken from the space. They require neither a position function nor a prepared neighborhood index and perform no MPI communication.

### Type Rules of Helper Functions

Space type (`GridSpace` or `ContinuousSpace`) and scalar coordinate type (`Int32`, `Float32`, etc.) must be distinguished. Without explicit coordinate type specification at the space, the passed positions determine the types of the helper functions; a neighborhood or its `posfunc` is not needed for this.

- `geom_wrap(space, pos, clamp_nonperiodic=false)` returns an `SVector` with the coordinate type of `pos`.
- `geom_move(space, pos, move_vector)` returns an `SVector` with the coordinate type of the input position `pos`. The type of `move_vector` may widen intermediate calculations but does not change the target coordinate type. For `GridSpace`, the rounding described below applies.
- `geom_displacement(space, from, to)` uses a common calculation type for both positions, which is widened as needed for overflow-safe differences.
- `geom_distance(space, a, b)` relies on safe difference arithmetic. For `Euclidean` and all metrics in `ContinuousSpace`, the function returns `Float64`. In `GridSpace`, `Cityblock` and `Chebyshev` return the integer calculation type of `geom_displacement`; no additional widening occurs.
- `geom_isvalidposition` returns a `Bool` for well-formed positions; dimension and coordinate category are preconditions checked with `@mayassert`.

All inputs must match the coordinate category of the space; these type rules specifically prevent silent rounding of grid positions outside the explicitly provided rounding in `geom_move`. Results must be representable in the target coordinate type; space boundaries need not be. Intermediate calculations may proceed independently of this. The rules are independent of previously registered neighborhoods and do not modify the space object.

### `geom_wrap`: normalize periodically and optionally clamp

```julia
new_pos = geom_wrap(space, pos, clamp_nonperiodic=false)
```

Periodic axes are wrapped into the canonical range even across multiple periods. Nonperiodic overflows are rejected by default; with `clamp_nonperiodic=true`, they are clamped coordinate-wise to the boundaries. No separate function `geom_clamp` is provided.

```julia
space = ContinuousSpace([(0, 10), (0, 5)]; periodic = (true, false))
geom_wrap(space, SVector(12.0, 7.0), true)
# SVector(2.0, 5.0)
```

Invalid inputs such as incorrect dimensions or `NaN` are not repaired by clamping. Explicit correction in model code does not change the search contract: neighborhood searches reject invalid nonperiodic positions.

### `geom_move`: move until first boundary contact

```julia
new_pos = geom_move(space, pos, move_vector)
```

The third argument is a displacement, not an absolute target position. The path considered is

```julia
pos(t) = pos + t * move_vector   # 0 <= t <= 1
```

Movement ends at the first contact that must prevent an overflow of a **nonperiodic** boundary. Without such a constraint, the entire movement is executed. Thus, a common factor `t` is determined for the entire displacement vector, rather than clamping individual target coordinates independently.

```julia
space = ContinuousSpace([(0, 10), (0, 10)]; periodic = false)

geom_move(space, SVector(7.0, 9.0), SVector(1.0, 2.0))
# SVector(7.5, 10.0)
```

- After a limiting boundary contact, no residual movement along the boundary is performed or reflected.
- Periodic axes do not constrain movement; resulting coordinates are wrapped there.
- In mixed periodicity, a nonperiodic boundary contact shortens the entire displacement vector, including its periodic components.
- A valid starting position is assumed. Intermediate calculations must be sufficiently wide; in particular, a fractional intersection must not be lost prematurely due to integer arithmetic.
- Return is exclusively the new position as an `SVector` with the coordinate type of the input position `pos`, not a new agent state.

**For `GridSpace`, the computed target position is rounded using `RoundNearest`**, with ties to the nearest even integer. A warning with `@warn` and `maxlog=1` is issued only upon actual rounding; clamping applies per MPI process/logger, without additional collectives. The result must still be a valid grid position after rounding and periodic normalization.

The model code itself updates the agent state, for example using accessors:

```julia
new_state = @set state.pos = geom_move(space, state.pos, movement)
```

Thus, the space does not require a write-back function and the helper function itself does not depend on accessors.

### Difference vector and distance

Additional helper functions are provided:

```julia
delta = geom_displacement(space, from, to)
distance = geom_distance(space, a, b)
```

`geom_displacement` returns the directed vector **from `from` to `to`**. On periodic axes, the shortest periodic path is computed using the minimum image convention; on nonperiodic axes, it is the standard difference. The calculation should also work correctly with multiple periods of offset.

```julia
# Periodic continuous axis [0, 10):
line = ContinuousSpace([(0, 10)]; periodic = true)
geom_displacement(line, SVector(1.0), SVector(9.0))
# SVector(-2.0)
```

When exactly half a period length is involved, there are two equally short directions. In case of a tie, the sign of the original difference is preserved so that swapping endpoints reverses the sign. This convention must be explicitly covered in tests.

The difference vector may have a wider calculation type than the stored positions: Two valid narrow integer coordinates can yield a difference that does not fit into their storage type.

`geom_distance` returns the distance according to the space metric, including periodic handling. Search backends and model code should use the same distance semantics.

For Euclidean distance, difference components are converted to `Float64` before squaring, and the square root of their sum of squares is computed. Extremely large or small floating-point values may cause overflow or underflow; a scaled, robust norm computation is not provided.

The integer Manhattan distance is summed without additional type widening. Before each addition, `@mayassert` checks whether the sum remains representable in the result type. With assertions enabled, an overflow causes `AssertionError`; with assertions disabled, the sum must not exceed the result type.

### `geom_isvalidposition`: check position

`geom_isvalidposition(space, pos)` is part of the public API. For a position with correct dimension and supported coordinate category, the function returns `true` if all values are finite and within the valid range, otherwise `false`. Periodic axes must lie in the canonical range; nonperiodic continuous axes include both boundaries.

Correct dimension and coordinate category are preconditions checked with `@mayassert`. With assertions enabled, a violation causes `AssertionError`; with assertions disabled, these preconditions must not be violated.

Cell assignment remains a backend operation and is initially not a public geometry helper. A general public function for squared distances is not necessary; corresponding optimizations can be performed internally on a metric-specific basis.

## 12. Implementation and Remaining Concretization

The basic architecture is defined. The next step suggests a vertical prototype:

1. Space and neighborhood registration including type derivation and validation.
2. Generated concrete typed neighborhood fields in the simulation type.
3. Bind the existing KD-tree search principle as `ReplicatedKDTree` to the new registered API; remove the old `spatial_neighbors` path. Implement all six search forms, snapshot preparation and release per `apply!` with this backend.
4. Test the new API with the KD-tree backend against a full reference search. Cover geometric helper functions, inference, empty populations, filters, periodicity per axis, metrics, and narrow coordinate types. This is a standalone acceptance milestone, before further backends are started.
5. Only after that, add `NodeSharedCellList` as an additional backend with shared node replication and collective window lifecycle.
6. Offer convenience functions and enhancements to the graph neighbor API as separate tasks; they do not require their own search infrastructure.

In the implementation, the following still need to be concretized in particular: the shared type resolution with different position methods within a neighborhood, the concrete storage layout, and MPI-safe error paths. These points do not change the described public search mechanism.
