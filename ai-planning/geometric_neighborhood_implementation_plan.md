# Geometric Neighborhoods: Concrete Implementation Plan

## 1. Objective and Commitment

This plan translates the design from [`geometric_neighborhood_api.md`](geometric_neighborhood_api.md) into work packages. The basis is an investigation of the current Vahana code, especially the registration in `ModelTypes`, the code generation in `create_model`, the generated agent/edge methods, the execution flow of `apply!`, the existing MPI shared-memory management, and the tests.

**This step only creates the plan, not the implementation.** Where internal details have not yet been decided, they are labeled as implementation suggestions or checkpoints. The plan must not silently introduce new public API contracts.

Objective of the first usable version:

- Registration of named geometric neighborhoods in `ModelTypes`, before `create_model`.
- Generated, concretely typed storage under `sim.geom_neighborhoods.<name>`.
- Fixed backend per registration, initially the existing globally replicated KD-tree search principle under the provisional backend name `ReplicatedKDTree`.
- New API to be tested first fully with this backend; `NodeSharedCellList` to be added later.
- Preparation, global replication and backend-dependent resource release per `apply!`, without cache.
- Replacement of the old `spatial_neighbors` API path, not continued in parallel.
- Six `geom_*` search functions, filters, projections, metrics and dimension-wise periodicity.
- Spatial edge creation through regular transitions.
- Spatial geometry helper functions.
- Separate spatial geometry and position assignment: `posfunc` belongs to `GeometricNeighborhood`; multiple neighborhoods can use the same space with different position functions.

**Not part of the first backend milestone:** node-local shared Cell List, selective communication, new geometric partitioners, cache or memory pool, standalone edge search path. The convenience function `connect_spatial_neighbors!` and the Graph neighbor API extension will follow separately.

## 2. Existing Patterns to Follow in Implementation

### 2.1 Registration and Code Generation

| Location | Existing Pattern | Consequence for New Code |
|---|---|---|
| `src/ModelTypes.jl`: `Param`, `Global`, `ModelTypes` | Small registration objects and vectors with model information | New geometric registrations also stored as model metadata |
| `register_agenttype!`, `register_edgetype!`, `register_param!`, `register_global!` | First argument `ModelTypes`; validation; return `types`; additional pipeable method | `register_geom_neighborhood!` follows this call pattern exactly |
| `src/Simulation.jl`: `create_model` | `Expr`, `Base.@kwdef` and `eval` generate simulation type, parameter and global structure | Generate a model-named geometric field structure in the same way |
| `src/Helpers.jl`: `simfield`, access macros | Direct access to generated fields | Resolve geometry names via field access, not search dictionaries in agent loop |
| `create_simulation` | Instantiation of generated type and initialization of its fields | Create new geometric runtime fields per simulation with `prepared = nothing` |

Dynamic vectors and optionally dictionaries are allowed during registration and outer preparation. They must not replace the concretely typed storage used within a transition for searching.

### 2.2 State, MPI, and Lifecycle Patterns

- `AgentFields{T}` contains separate Read-/Write data as well as MPI management; `AgentMethods.jl` generates specialized accesses including mortality handling.
- `prepare_write!` can replace Read and Write buffers when `add_existing = false`. Therefore, the geometric snapshot preparation must not silently remain at the previous spatial position after `prepare_write!`.
- `:Independent` allows in-place updates. Neighbor data is collected from Read buffers before transitions; the searcher reads its state in its own callback before update (see D1).
- `MPIinit.jl` provides `mpi.comm`, `mpi.shmcomm`, shared-memory rank and size. Existing topology assumptions are not changed incidentally.
- `AgentMethods.jl` uses `MPI.Win_allocate_shared`, `MPI.Win_shared_query`, `MPI.Win_fence` and explicitly `MPI.free`.
- Even in single-process case, existing agent code uses MPI shared-memory operations. The first geometric backend itself does not require additional shared-memory windows: it holds private KD-tree and neighbor data. Existing agent windows remain unchanged.
- `create_simulation` explicitly avoids parallel simulation finalizers. New collective releases must also not be left to the garbage collector.
- `copy_simulation` copies state and explicitly repairs existing agent windows. Add a check of `sim.intransition` at the beginning of the function: copying during an `apply!` is generally rejected with a comprehensible error, regardless of whether geometric neighborhoods are selected. Geometric runtime data lives exclusively within `apply!`; thus, when copying outside a transition is allowed, `prepared = nothing`. Temporary geometry arrays, indices or future geometry windows must not be copied.

### 2.3 What Should Not Be Used as Template

The current code in `ConnectSpatial.jl` is the functional starting point, but not the desired memory architecture:

- `NeighborsInfo` and `sim.neighbors_infos` hold data per agent type instead of per named neighborhood.
- `SpatialNeighbors` mixes search definition, KD-tree tuning and cache identity.
- `_get_ids_poss_states` and `join` replicate complete data in private vectors on each rank.
- Type derivation via `Base.return_types(f)[1]` is insufficient for a validated, argument-type-resolved registration.
- Integral periodic boundaries are interpreted only at KD-tree construction time.
- The KD-tree construction is reused as a backend mechanism. Old cache hits and the old type-based storage are discarded; instrumentation and tests will be migrated to the new registered API.

These mechanisms should not merely be renamed. However, the existing graph, MPI, and model architecture remains intact.

## 3. Proposed File and Module Splitting

Vahana uses a module with source files separated by concern. The proposal sticks to this pattern and does not introduce a new sub-module or plugin framework.

| File | Planned Responsibility |
|---|---|
| `src/GeometricSpace.jl` (new) | Space types, coordinate normalization, metric contract, pure helper functions |
| `src/GeometricNeighborhood.jl` | Domain-specific definition, `GeometricBackend`, registration descriptor and `NeighborhoodRuntime` |
| `src/geom_backends/ReplicatedKDTree.jl` | Configuration present; private global replication, tree, exact hit indices and result accessors to be added |
| `src/geom_backends/NodeSharedCellList.jl` (later) | Configuration, shared cell index and same backend hooks |
| `src/ModelTypes.jl` | Registration vectors, `register_geom_neighborhood!` and call to backend validation |
| `src/GeometricMethods.jl` (new) | Local collector, shared search path, `GeomNeighborIterator` and six public search functions |
| `src/GeometricMPI.jl` (later) | Shared-memory construction, hierarchical replication, synchronization and release for the additional cell-list backend |
| `src/Simulation.jl` | Generated geometric structure, simulation initialization, selection and lifecycle in `apply!` |
| `src/Instrumentation.jl` | New phase/mechanism labels, registration and output of events |
| `src/REPL.jl` | Display registered geometric neighborhoods at model and simulation level and concrete geometric neighbors in `show_agent` |
| `src/ConnectSpatial.jl` | Transfer KD-tree search principle into new backend; remove old API. Optionally, location for new convenience functions later on |
| `src/Edge.jl`, `src/EdgeMethods.jl` | Separate step later for `neighbors` and `neighbors_iter` |
| `test/geometric_*.jl`, `test/mpi/test_geometric_*.jl` (new) | Concern-separated tests plus MPI wrapper in existing style |

File names are implementation suggestions. Merging small parts is sensible if a separate file does not gain real responsibility.

### Include Order

Currently, `ConnectSpatial.jl` is included only after `Simulation.jl`. New types in `ModelTypes` or simulation fields must be known earlier:

1. Include `GeometricSpace.jl` and basic types from `GeometricNeighborhood.jl` before `ModelTypes.jl`.
2. Backend methods dispatching on `GeometricRegistration` or `NeighborhoodRuntime` should be included only after these type definitions. The current early inclusion of `ReplicatedKDTree.jl` may need to be moved accordingly. Generation and backend functions must be available before first use of registration or `create_model`.
3. Avoid type annotations for yet undefined concrete types; do not create circular include dependencies.
4. Do not retain the former `ConnectSpatial.jl` API path as a legacy alternative. If the file initially still contains helper functions, explicitly justify remaining includes; integrate taken KD-tree code into the new backend.

Keep new imports local and traceable. `StaticArrays`, `MPI`, and `NearestNeighbors` already exist as direct dependencies. If `Distances` is imported directly, it must be explicitly listed in `Project.toml`; do not silently assume a transitive dependency. Accessors are not needed for the geometry implementation.

## 4. Work Package A: Spaces, Metrics and Helper Functions

### A1. Parametric Space Types

- Implement `GeometricSpace` as an abstract base type along with `GridSpace` and `ContinuousSpace`.
- Store metric, number of dimensions, bounds and periodicity using concrete types. Do not store a position function or scalar positional coordinate type in the space; also do not provide a phantom type parameter for that.
- Offer constructors with bound pairs per dimension and `Bool` or `NTuple{N,Bool}` for periodicity.
- Determine dimension solely from the number of bound pairs. Do not offer explicit coordinate type specification like `GridSpace{Int32}(...)`. The storage type of bounds does not determine the positional coordinate type. For helper functions, apply input-based typing rules from A4.
- Check supported fixed integer/float types for position arguments in helper functions; do not automatically widen to arbitrary `Number` types.

Work package A requires neither agent types nor position functions or neighborhood registration. Deriving coordinate type from a neighborhood's `posfunc` belongs exclusively to work package B.

### A2. Numerics and Bounds

- Grid periods including upper bound; continuous periods without additional `+1`.
- Non-periodic overflows default to rejection; allow explicit coordinate-wise bounding via an additional bool argument in `geom_wrap`. Centralize periodic normalization.
- Allow both geometric boundary surfaces for continuous non-periodic axes in `geom_wrap` and `geom_move`.
- Keep coordinate storage narrow, but widen intermediate calculations and cell indices safely.
- Check bound conversion, overflow, `NaN`, infinite values, dimension and invalid space intervals.
- Test full value ranges of narrower integers: Even the period length may no longer fit into the coordinate type.

### A3. Metric Contract

Implement exact distance using periodic difference computation for Euclidean, Cityblock/Manhattan, and Chebyshev distances. Weighted metrics are not part of work package A. Only allow metrics where component-wise minimum image is correct.

Reject unknown or unsupported combinations early with a clear message. Conservative axis-wise search extent for a radius will be implemented later in work package E2 and does not belong to the acceptance criteria of A.

### A4. Public Helper Functions

- `geom_move(space, pos, move_vector)`: common path parameter to the first limiting nonperiodic boundary contact; no sliding or reflection.
- `geom_displacement(space, from, to)`: directed difference including minimum image; result type may be wider as needed. For exactly one half-period length, preserve the sign of the original difference; swapping endpoints flips the sign.
- `geom_distance(space, a, b)`: same metric and periodicity semantics as in the backend. For `Euclidean` and all metrics in `ContinuousSpace`, the return type is `Float64`; `Cityblock` and `Chebyshev` in `GridSpace` return the integer computation type of `geom_displacement`, without additional widening. For `Euclidean`, convert to `Float64` before squaring and use square root of sum of squares; over/underflow of extreme floating-point values is not avoided by a robust norm calculation. Compute the integer Manhattan sum in a loop and check its representability exclusively with `@mayassert` before each addition. With disabled assertions, the sum must not exceed the result type.
- `geom_wrap(space, pos, clamp_nonperiodic::Bool=false)` as mandatory public API to offer. Normalize periodic axes always. When `clamp_nonperiodic=false`, reject nonperiodic overflows; when `true`, clamp nonperiodic coordinates to the limits. Do not provide a separate function `geom_clamp`. Test wrapping across multiple periods, mixed periodicity, invalid inputs, and both behaviors for nonperiodic overflows.
- `geom_isvalidposition(space, pos)` as mandatory public API to offer: correct dimension and supported coordinate category are preconditions checked with `@mayassert` (`AssertionError` when assertions are enabled). For well-formed positions, the function returns a `Bool`: `false` for non-finite values or outside the valid position range, otherwise `true`.

**Mandatory Type Rules:** `geom_wrap` and `geom_move` return an `SVector` with the coordinate type of `pos`. The movement vector's type may widen intermediate computations but must not change the target coordinate type. `geom_displacement` uses a common, potentially widened computation type for both positions to ensure overflow safety. `geom_distance` returns `Float64` for `Euclidean` and in `ContinuousSpace`, and the integer computation type of `geom_displacement` for `Cityblock` and `Chebyshev` in `GridSpace`. `geom_isvalidposition` returns a `Bool` for well-formed positions and checks dimension and coordinate category with `@mayassert`. There is no explicit position coordinate type specification on the space. These rules do not require `posfunc`, do not depend on registered neighborhoods, and do not modify the space object. Continue to check valid coordinate categories and representability; the explicitly intended grid rounding in `geom_move` remains unchanged.

Tests must especially cover different types of input positions and movement vectors, independence of position coordinate type from boundary type, safe differences with narrow coordinates, space- and metric-dependent return types of `geom_distance`, including `@mayassert` overflow checks for integer Manhattan sum, public position validation, and independence from neighborhood registrations.

For `geom_move` in GridSpace, compute the geometric intersection first, then round, normalize, and ensure a valid grid position. Round using Julia's `RoundNearest` to the nearest even integer; explicitly test this convention. Emit warning only on actual rounding with `maxlog=1`, without collective operations in movement calls. This limitation applies per process/logger under MPI, not globally.

**Acceptance A:** independent tests of geometry without simulation, neighborhood indexing, or new communication operations.

## 5. Work Package B: Registration and Type Derivation (completed)

### B1. Definitions and Metadata

- Create `GeometricNeighborhood` parametrically for space, `posfunc`, `neighbor_filter`, and projection. Agent types are stored in type vectors; radius and weight are stored as `Float64`. The pure position function on the readable snapshot must support all involved Query and Neighbor types.
- Support `weight`, `project_type`, and defaults according to the API design.
- Define `GeometricRegistration` as a registration descriptor with `name`, `definition`, `backend`, `coordinate_type`, and `project_type`. Store backend configuration and derived type parameters alongside the functional definition.
- Add a vector `geom_neighborhoods::Vector{GeometricRegistration}` for named neighborhoods in `ModelTypes`, similar to `params` and `globals`.
- Spaces are not separately registered or managed in their own model list. The space is directly accessible via `definition.space`; a `space_index` is omitted.
- The same space may be referenced by multiple neighborhoods with different `posfunc`s, for example, housing and workplace positions of the same person. Position mapping and derived coordinate type remain neighborhood-specific; different valid coordinate types are allowed.

### B2. Registration Functions

```julia
register_geom_neighborhood!(types, :food, definition; backend = ReplicatedKDTree())
register_geom_neighborhood!(:food, definition; backend = ReplicatedKDTree())
```

The second form returns a function for the pipeline like other `register_*` functions. It calls the same registration that returns `types`.

Registration checks unique names and ensures all specified agent types are already registered in `ModelTypes`. Mortality and agent hints are stored there, not re-registered in the neighborhood.

**Position and Coordinate Type Derivation:** For each involved Query and Neighbor type, determine the concrete return type of `posfunc` via compiler inference. Use the element type and check with `@mayassert` that all axes have the same type for tuples. Check the coordinate type per agent type against the allowed space category and only promote between agent types to a common concrete type. This is stored in registration metadata; the shared space object remains unchanged. No example agents are needed for this.

**Conscious lightweight input validation:** Matching position form and dimension, a boolean neighbor filter, finite non-negative radius, and equally valid weight as well as sensible backend parameters are prerequisites for usage. The registration does not check these specifically. Separate negative tests for these cases are not a requirement for B. The rules for the spatial helper functions from package A remain unchanged.

### B3. Adopt or derive projection type

- Accept an explicit `project_type` directly. Specifying a concrete type that matches the actual projection results is the task of the model definition.
- Without specification via argument-type-specific compiler inference, require the same concrete return type for all neighbor types. Otherwise, abort with a note to `project_type`.
- Do not perform projection on example states; empty populations are irrelevant for type derivation.

**Acceptance B:** The smoke tests in `test/geometric_neighborhood.jl` cover direct and pipeable registration, duplicate names, missing agent types, coordinate type derivation, implicit and explicit projection types, as well as `ReplicatedKDTree` as the first backend configuration. Backend and MPI requirements for data transfer belong to D and E.

## 6. Work Package C: Code generation and typed field access

### C1. Model-named field structure

Create a structure named `GeomNeighborhoods_<Modelname>` in `create_model` before the simulation type. Each registered symbol gets a field with a concrete `NeighborhoodRuntime{R,P}` type.

Generate the structure following the existing `Expr`-/`@kwdef`-/`eval` pattern. The field definition in the simulation type must carry the generated concrete type, not an abstract geometry container.

`NeighborhoodRuntime` contains exactly `registration::R` and `prepared::Union{Nothing,P}`. `R` already includes definition, backend configuration, and resolved types. `P` comes from `geom_prepared_type(registration)`.

`create_simulation` generates new runtime containers for each simulation. Do not embed a mutable runtime instance as a shared model default.

### C2. Inferable name access

Name resolution uses the generic field access `getfield(sim.geom_neighborhoods, name)`. A test run under Julia 1.12.6 showed that this access is inferable in an `apply!` transition for two literal names with different field types. For this, neither a `Val` dispatch nor a custom method generator is needed. The selection of neighborhoods before the agent loop may remain dynamic.

The public search API remains `(sim, id, :name)`, without additional transition arguments. C2 only checks the type inference of name access. Snapshot capture, search core, and the six search variants belong to D; preparation and release are included in F within `apply!`.

### C3. Model lifetime and copies

- The geometric structure is fixed at model creation; later mutation of underlying metadata must not silently alter the already generated schema.
- Test multiple models with different names and multiple simulations of the same model.
- Immediately at the start of `copy_simulation`, before copy or resource operations, check `sim.intransition` and throw a comprehensible error if `true`. Copying during a transition is generally prohibited, not just when active geometry is involved.
- Outside of a transition, `copy_simulation` must find `prepared = nothing` and create independent geometric containers. Since all temporary geometric data lives only within `apply!`, no copying strategy for active geometric resources or special checks for active geometric windows are required.
- No temporary indices or windows persist in HDF5 snapshots; rebuild them upon restart based on the model.

**Acceptance C:** concrete generated field types, inferable access with literal names from a transition, and no unintended shared simulation resources. The inference of search results is part of D.

## 7. Work Package D: Local snapshots and first KD-tree backend

Signatures and responsibilities are defined in the [internal backend contract of the API design](geometric_neighborhood_api.md#internal-backend-contract). D implements it initially for `ReplicatedKDTree`. The shared code passes the `NeighborhoodRuntime`; a method with `sim` always takes it as the first argument.

### D1. Collect local neighbor data

Before transitions, for each selected neighborhood, read live local neighbors from the read buffers (`read.state` and `read.died`). Filter them via `neighbor_filter` and collect per neighbor position (`posfunc`), existing agent ID, and projection. For this, reuse the local collection kernel from `ConnectSpatial.jl` as a shared helper `geom_collect_local_neighbors(sim, registration)`. `geom_prepare!(sim, runtime)` calls it and handles the MPI replication from D2; the helper itself does not communicate.

The search uses the ID of the currently executing agent and determines the search position with `posfunc` from its read state. The query type is checked with `@mayassert`. Whether to search is decided by the model transition; a separate query buffer is not needed.

For `:Independent`, the own new state is written only after the callback. A neighborhood where the same `:Independent` type both searches and appears as neighbor is already rejected at registration.

### D2. Global replication per rank

Transfer the existing principle from `_agents_ids_states_and_edges`, `_get_ids_poss_states`, and `join` to the registered neighborhood:

- Gather filtered local neighbor positions, IDs and projections from all ranks in identical order.
- All ranks maintain private global neighbor data. This is explicitly intended for the first backend, not yet a later memory optimization.
- Reuse existing `join` mechanism where appropriate; carefully check its `Int32` size limits. Size, position, ID and projection collectives must not be skipped due to local emptiness.
- Handle null-byte projections without invalid MPI data type; empty global neighbor sets receive a concrete typed empty preparation state.
- No cache and no `snhash`; each selected `apply!` call records and replicates anew.

### D3. KD-tree construction and search adapter

- Adopt the search principle `KDTree`/`inrange` from `ConnectSpatial.jl`, but without old API dependency, type-based storage or old boundary heuristic.
- Encapsulate backend parameters `leafsize` and `reorder` in `ReplicatedKDTree`. Space, metric and periodicity come from the new space definition.
- Keep search matrix and order of IDs/projections consistent. Test original indices used by the library, especially with `reorder = true`.
- Implement `geom_prepared_type(registration)` for the concrete KD-tree state, also for empty populations. Specialize tree and array types via fixed registration, not store in `Any`. `geom_prepare!(sim, runtime)` returns this state.
- Internally check candidates against exact spatial distance in `geom_hits(runtime, id, position)`, exclude self-ID and avoid periodic duplicates. Only output exact hit indices. The library may internally materialize an index vector for this.
- Implement `geom_neighbor_id`, `geom_neighbor_projection` and `geom_neighbor_pair` in the backend. All read at given hit index without re-search. The pair variant accesses the backend layout directly.
- Generate three specialized variants of `GeomNeighborIterator` from the same hit source in common code. Vector variants collect with `collect`. Check literal names with and without hits using `@inferred`, also in integrated `apply!` transitions.

**Compatibility check before implementation:** Current `_create_kdtree!` assumptions are insufficient for new API. In installed `NearestNeighbors`, especially check integer/narrow tree coordinates, multiple periodic axes, mixed periodicity and agreed metrics. Design a correct KD-tree adapter for unsupported periodic cases, e.g., normalized base positions with periodic query images and candidate deduplication. Separate broader working coordinates needed by the library from declared space/position storage types and expose their memory requirements.

Clearly report unsupported combinations in `geom_validate_backend(registration)`, do not silently change periodicity or metric. The promised space/metric cases must either be correctly connected or explicitly documented as unsupported backend combinations at API acceptance milestone.

### D4. Resources and acceptance

`prepared` contains private global neighbor arrays, the tree and necessary index mappings for this backend. The KD-tree backend does not need to release its own MPI resources in hook `geom_release!(sim, runtime)`. The common lifecycle then sets `runtime.prepared = nothing`.

**Acceptance D together with F/G:** test new API with KD-tree backend in real `apply!` transitions, serially and with MPI compared against a full reference search. Check all six access forms, registration, filtering, projections, snapshot semantics, helper functions and resource lifecycle. Begin only after this independent milestone with further backends. The full reference remains test code, not a second production backend.

## 8. Later work package E: node-local shared Cell List

**This package starts only after successful testing of the new API with `ReplicatedKDTree`.** It adds another registrable backend with its own `prepared` type and same hooks. `GeomNeighborIterator` and the six public functions remain unchanged. Work packages F/G are already required for the first KD-tree backend.

### E1. Concrete index layout

Proposal for future Cell List implementation: **sparse, sorted occupied cells with CSR-like ranges**, instead of an array for all conceivable cells or a private dictionary per rank.

Shared arrays:

- positions grouped by cells;
- associated agent IDs;
- projection results;
- sorted keys of occupied cells;
- offsets of hit ranges per occupied cell.

Cell keys and offsets have concrete fixed types, derived from dimension or safe index type. Search for a cell key e.g. via binary search. The layout requires memory depending on agents and occupied cells, not the entire volume of a possibly very finely discretized space.

First build on one rank of the node and then publish as immutable index. Parallel index construction is a later optimization.

### E2. Cell geometry and candidates

- Configure configurable positive cell size or set deterministic default.
- Periodic axes consistently tile into their period; use effective cell widths for non-exactly divisible extent.
- Assign the upper non-periodic boundary face of the last cell.
- Implement an internal metric operation for conservative axis-wise search extent for a radius. For Euclidean, Cityblock and Chebyshev, the radius per axis suffices. The extent may include additional candidates but must not exclude actual hits; do not silently use Euclidean search extents for other metrics.
- Determine candidates based on metric-specific search extent.
- Normalize periodic cell numbers and visit each candidate cell at most once, even if the radius covers the period.
- Check exact distance and exclude self-ID before `geom_hits` outputs a hit index. A Cell-list hit iterator can perform this check while iterating through cells.
- Test for zero radius, degenerate raster axes with only one raster point, and global empty population separately.
- Do not allow integer overflow in cell keys or total counts.

Compare the Cell List against the same full reference and additionally against the already tested KD-tree backend.

### E3. Communication Architecture

The **later shared-memory backend** must not merely execute the private KD-tree replication path and then transfer each rank copy into shared memory. That would miss its memory goal. Private global replication remains, however, a conscious feature of `ReplicatedKDTree`.

A proposed implementation is a two-stage replication:

1. All ranks collect their local filtered neighbor data.
2. Local data and sizes are gathered on the node; a shared-memory leader is the respective rank `mpi.shmrank == 0`.
3. Only the node leaders exchange the gathered data between nodes.
4. Create global receive arrays per node in shared memory; the leader gets the full segment, other node-ranks get a null segment.
5. The leader performs the data transfer and index construction into the shared arrays.
6. All node-ranks obtain their local array views via `Win_shared_query`. After correct window synchronization, only read access is used.

A leader communicator can be created collectively per `apply!`, used for its selected neighborhood, and released at the end. No new global, permanently cached runtime state is needed for this. The concrete implementation of size transfer, gather, and allgatherv follows MPI.jl 0.20 and will be checked against installed APIs.

Ranks may only participate in collectives of their respective communicator. All ranks must participate in the outer preparation and release protocol. A node without local hits still receives global data.

### E4. Memory and MPI Details

- One-time global data and index copy per node; local array headers per rank are acceptable.
- Leader staging data may temporarily cause an additional node copy; free staging afterwards and measure peak demand.
- No global, private O(N)-position/projection arrays on any non-leader.
- For projections in the shared-memory backend require `isbitstype`. Explicitly test MPI buffers and data types for `SVector`, tuples/cell keys, and custom bitstype projections. If needed, internally use a well-defined flat buffer without changing the public result format.
- Do not query null-byte projections via windows with `sizeof(T) == 0`; reconstruct values from known type or use a separate memoryless schema.
- Empty segments, a globally empty neighborhood, and single-process runs must follow the same collective lifecycle.
- Check size/displacement limits of used MPI calls. The existing `join` uses `Int32` sizes; do not blindly carry this restriction into new large transfers. Report unsupported sizes controlled or explicitly chunk them.
- Make data visible: do not equate barrier with window memory synchronization. Check and document the chosen `Win_fence`/synchronization sequence according to the MPI memory model.
- Hold all window references until explicit release; MPI.jl finalizers are not a resource protocol.

**Acceptance E:** one node with multiple ranks and at least one real multi-node test; complete results on all searching ranks and no private global data store per rank.

## 9. Work Package F: Integration into `apply!`

### F1. Selection and Read Contract

- Accept `neighborhoods = nothing`, a symbol, or `Vector{Symbol}`.
- Validate names before preparation and normalize duplicate occurrences.
- Perform preparation/release in stable registration order, not in a random dictionary order.
- Detect different selections on various ranks before beginning neighborhood-specific collectives; do not allow divergence due to differing name orders.
- Maintain the existing read contract: required states must be allowed in the `read` argument. Explicitly check and document query and neighbor type requirements; do not silently extend the graph read set.
- `with_edge` restricts the transition call, not the registered search definition.

### F2. Placement in Existing Flow

Intended order:

1. Normalize `apply!` arguments and validate geometric selection.
2. Perform existing `prepare_read!`.
3. For each selected runtime, call `geom_prepare!(sim, runtime)` and store the return value in `runtime.prepared`, **before `prepare_write!` can replace the read buffers**. Ensure that the own read state for direct position queries remains available in the callback.
4. Perform existing `prepare_write!` and the existing pre-apply barrier.
5. Use unchanged transition signatures and existing agent loops.
6. Wait for the existing post-apply barrier.
7. Call `geom_release!(sim, runtime)` in consistent neighbor order and then set `runtime.prepared = nothing`. For the KD-tree backend, this suffices to release tree/array references; the shared-memory backend collectively frees its windows and temporary communicators.
8. Retain existing edge transfers as well as `finish_read!`/`finish_write!` in their functionally required order.
9. Close instrumentation and increment transition counters as before.

Releasing immediately after the post-apply barrier already limits the lifetime of large private data in the KD-tree backend; for the later shared-memory backend, it additionally prevents unnecessary overlap with new agent windows in `finish_write!`. If other dependencies become visible during implementation, explicitly verify the exact position—not just replace the old `finish_spatial_neighbors!` call.

**Acceptance F first with KD-tree:** two consecutive calls build upon each other; write accesses, mortality, migration between calls and `:Independent` do not alter the snapshot of the current call. After normal completion, `prepared = nothing`. For the later shared-memory backend, all geometry windows and temporary communicators are additionally freed.

## 10. Work-package G: Instrumentation and existing side paths

### Instrumentation

- Use existing `_inst_phase`, `_inst_begin!`, and `_inst_end!`.
- First map phases for selection/validation, local capture, global replication, KD-tree construction, and release. Reuse existing suitable KD-tree-/join labels if their meaning remains unchanged.
- Add Node-Gather, leader exchange, shared allocation, and cell-list construction only for the later shared-memory backend.
- Do not generate symbolic event labels per agent; do not introduce an unlimited new label set per neighborhood.
- Existing events have a type dimension but no general neighborhood name yet. For the first step, document order/type mapping; extend schema only consciously including persistence and tools.
- Do not shift existing enum values by inserting new labels if persisted recordings use them numerically. Append new values or explicitly migrate format version.
- Disabled instrumentation must not cause additional O(N) measurement buffers or diagnostic collectives.
- Change `test/mpi/test_instrumentation.jl` to registered neighborhoods and the KD-tree backend. Expect KD-tree construction and replication at every selected call; remove old cache hit expectations.

### Copies, Completion, and HDF5

- Test `copy_simulation` and `apply` outside a transition with `prepared = nothing`. Additionally, verify via regression test that `copy_simulation` aborts with a comprehensible error before copying or resource operations when `sim.intransition == true`; this applies both with and without selected geometric neighborhoods.
- `finish_simulation!` should not find open geometry windows after normal `apply!`; no second blind release.
- Test HDF5 snapshot roundtrip with the new simulation type; temporary data remains non-persistent.

### REPL Display

- In `src/REPL.jl`, both `show(io, MIME"text/plain", ...)` outputs per neighborhood show name, space type, dimension, metric, radius as well as query and neighbor types. For `Model`, these values come from model registrations; for simulation, from their generated `geom_neighborhoods` structure. The display is independent of which neighborhoods were selected or prepared for a transition.
- Sections are omitted for models and simulations without neighborhoods. Backend and temporary search data (`prepared`) are not part of the display.
- Extend `show_agent` with actual geometric hits of the agent, grouped by named neighborhood and limited analogously to existing output via `max`. Use the same search semantics as in the `geom_*` API; show geometric hits separately from edge neighbors.
- Explicitly define how a search snapshot is made available for `show_agent` outside a transition. Normal display must not initiate implicit MPI collectives, since it can also be called on only one rank.

## 11. Replace old API, adopt KD-tree principle

**No parallel legacy path is provided.** The new registered API replaces the previous `spatial_neighbors` mechanism. The former search principle remains available as `ReplicatedKDTree`, not as an old public interface.

For the first API/KD-tree milestone:

1. Remove `SpatialNeighbors`, the `spatial_neighbors` parameter, the free `find_neighbors` API, and the old prepare/finish functions from the new call path.
2. Replace `sim.neighbors_infos`, `NeighborsInfo`, `snhash`, and `AgentFields.prepared_spatial_neighbors` with named generated fields; adjust affected constructors and comments.
3. Adapt reusable KD-tree and replication logic from `ConnectSpatial.jl` to space definitions, registration, and `prepared`. Do not retain a second search infrastructure alongside the new API.
4. Transfer content-relevant existing spatial test cases to the new registration and `geom_*` functions; consciously consider new semantics like inclusive grid boundaries.
5. Switch instrumentation tests, documentation, and examples to the new API. Remove cache hit expectations.
6. Migrate old `periodic_diff`-/`periodic_clamp` calls to new helper functions or document their removal separately in the public API.

The existing standalone `connect_spatial_neighbors!` implementation does not belong to the new search path. Its domain test cases can initially be covered by regular transitions with `geom_neighborids_iter` and `add_edge!`. A new convenience function will follow later as a wrapper with configurable edge direction. No search is offered by the new API before `finish_init!`.

**Only after successful validation of this state** will the shared-memory backend be started. The API does not have to wait for its completion.

## 12. Test and Acceptance Plan

### Test Distribution

| Suggested File | Focus |
|---|---|
| `test/geometric_space.jl` | Boundaries, coordinate types, metrics, wrapping with and without boundary constraints on non-periodic axes, movement |
| `test/geometric_neighborhood.jl` | `ModelTypes`, direct and pipeable registration, type derivation and selected error cases |
| `test/geometric_neighbors.jl` | Initially KD-tree backend: six search forms, snapshots, filters, multiple spaces/types; later cross-backend cases |
| `test/mpi/test_geometric_neighbors.jl` | MPI wrapper and additional distributed resource tests |

Shared test models defined at the top level like in `test/core.jl` and `test/spatial_neighbors.jl`. Do not hide `create_model` and `create_simulation` within the same newly compiled function; respect the existing World-age restriction. Parametric agent types like in `test/parametric_types.jl` should be covered.

### Key Cases

- Single and multiple query/neighbor types, different position fields.
- Same type in multiple spaces and multiple neighborhoods.
- Two neighborhoods with the same space but different `posfunc` (apartment/workplace): independent search results, position snapshots and type derivation; registration does not modify the shared space object. Also test differing allowed coordinate types for a common space.
- Automatically inferred NamedTuple projection and explicit type specification as an alternative to inference.
- Without explicit specification: common concrete projection type across multiple neighbor types; when types differ, registration refers to `project_type`.
- Null-byte projection and narrow position storage types.
- Mortality, ID reuse, new agents and `:Independent`.
- Empty local population, empty node, globally empty neighborhood; zero or all excluded by filter.
- No self-entry, but different agent IDs with identical positions/projections.
- Distance exactly at radius, zero radius and greater than period; no duplicate hits from periodic KD-tree query images or later wrapped cells.
- Non-zero lower bounds and mixed periodic axes.
- Euclidean, Cityblock/Manhattan and Chebyshev metrics including their search extents against full reference search; weighted metrics to be rejected.
- Non-periodic overflows, `NaN`, infinite values and overflows in the space helper functions per package A.
- `geom_move` intersection `(7,9) + (1,2) -> (7.5,10)`, start at boundary, outward/inward/parallel, raster rounding and warning limit.
- Repeated `apply!`, different selected neighborhoods and changes between calls.
- Comparison of all six APIs sorted by ID, not based on guaranteed result order.
- In C: `@inferred` for generic name access with literal names from a transition.
- In D/F: Result vectors and iterators with concrete `eltype`, even when empty; `@inferred` in public search path with literal name.
- After completion of `apply!`, `prepared = nothing`; initially no persistent KD-tree/array references, later also no open geometric windows. Independent simulation copies.

### Commands

Run tests in the development project, not against a separately registered Vahana version:

```sh
julia --project=. test/geometric_space.jl
julia --project=. test/geometric_neighborhood.jl
julia --project=. test/geometric_neighbors.jl
julia --project=. test/runtests.jl

mpiexecjl -n 2 julia --project=. test/mpi/test_geometric_neighbors.jl
mpiexecjl -n 4 julia --project=. test/mpi/test_geometric_neighbors.jl
mpiexecjl -n 2 julia --project=. test/mpi/test_instrumentation.jl
```

The new independently callable test files must include their required imports themselves or use a common small test setup. Additionally, run existing relevant MPI regressions.

The first milestone already checks global KD-tree replication with multiple ranks. A real multi-node run is also meaningful for this; however, API validation does not have to wait for an unimplemented shared-memory backend.

For the later `NodeSharedCellList`, **a real multi-node run is a separate acceptance step**, including nodes without local neighbors and differing local population sizes. A local run with four ranks typically tests only one shared-memory node. A missing cluster run must not be presented as successful multi-node acceptance.

`Project.toml` supports Julia 1.9 to 1.12. At minimum, check inference/code generation tests and MPI API behavior across supported versions; avoid unintended version bumps.

### Performance and Memory Checks

After correct results and warm compiler:

- inferred types and allocations in literal search path;
- iterator vs. materialization;
- construction, communication, search and deallocation separately;
- stored position bytes for `Int16`/`Int32` respectively `Float32`/`Float64`, including additional KD-tree working matrix;
- initially private global data and tree bytes per rank; this is the deliberately unoptimized API baseline;
- later shared-memory data bytes per node and private staging data per rank;
- behavior with increasing number of ranks per node.

In the later shared-memory backend, shared pages must not be counted as independent copies when interpreting RSS. In addition to OS measurements, log actual array/window sizes, buffer sizes and owners. Do not set fixed nanosecond thresholds in regression tests.

## 13. Recommended Implementation Stages

### Milestone 1: New API with current KD-tree principle

1. **Spaces and helper functions** including numerical tests.
2. **Registration and type derivation** with pipeable API and `ReplicatedKDTree` as backend configuration (completed).
3. **Generated fields, inferable name access, and independent simulation containers.** Check access via `sim` and literal symbol from a transition.
4. **Local snapshots, private global replication, and KD-tree adapter** with all six search forms; check inference with and without hits and test results against full reference search.
5. **`apply!` integration, instrumentation, and removal of the old Spatial-API path.** Transfer existing content tests to the new API; do not adopt any caching functionality.
6. **API testing and explicit acceptance of this state:** test real model transitions including edge generation, MPI, type inference, filters/projections, copies, and repeated calls. Incorporate feedback on the API before further backend development.

### Milestone 2: additional node-local shared backend

7. **Cell list and single-node shared-memory protocol** implementing the same public search contract.
8. **Hierarchical multi-node replication and collective sharing** to be added; run joint tests against KD-tree and full reference.
9. **Backend comparison and memory benchmark** with real multi-node acceptance.

### Separate Additions

10. **Convenience functions and Graph-API extension** via shared infrastructure; no new independent search path.

Each stage should deliver a small, verifiable state. No second independent spatial stack, no additional transition argument, and no cache/partitioner implementation as hidden prerequisite.
