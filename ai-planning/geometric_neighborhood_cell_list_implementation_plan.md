# Work Package E: NodeSharedCellList Implementation Plan

The [cell-list concept](geometric_neighborhood_cell_list_concept.md) is the design reference. This plan specifies the implementation order and the checks required to move between stages. It supersedes the leader-only construction proposal for E in the [general implementation plan](geometric_neighborhood_implementation_plan.md#8-later-work-package-e-node-local-shared-cell-list).

## Scope

Add `NodeSharedCellList` as an opt-in backend. Keep `ReplicatedKDTree` as the default and keep the existing six query functions. Each `geom_prepare!` rebuilds one shared, read-only snapshot per node. Each node receives the complete filtered snapshot. Cell owners receive their portions through `MPI_Alltoallv_c` and place records in parallel into final shared ranges without sorting the agent payload. Queries do not communicate.

Implement contiguous ownership ranges and require equal numbers of ranks per node initially. Do not implement the two leader-based benchmark variants, an exchange selector, selective communication, or cross-call caching. Reusing a future partitioner's mapping is an extension, not a dependency for the first implementation. Keep agent IDs in the first implementation for self-exclusion and ID-returning queries. An `ignore_ids` mode is only a possible later optimization.

## 1. Define configuration and geometry

- Add `src/geom_backends/NodeSharedCellList.jl`, include it in `src/Vahana.jl`, and export the backend alongside `ReplicatedKDTree`. Implement `geom_validate_backend` and `geom_prepared_type` through the existing registration and generated-runtime hooks. Keep the prepared type concrete for each registration, including a bounded choice of representations for `cell_index=:auto`.
- Validate `cell_index` (`:dense`, `:sparse`, or `:auto`), the positive finite cell size, and the concrete `isbits` projection type. Honor the space-specific radius rules: `GridSpace` allows zero and positive radii below one, with a default cell size of one at radius zero; `ContinuousSpace` rejects a zero radius even with an explicit cell size.
- Implement cell assignment and candidate bounds separately for `GridSpace` and `ContinuousSpace`, using the concept's cell-count, effective-width, and boundary rules. Define one cell order shared by dense linear IDs, sparse tuple keys, and owner ranges. Derive the arithmetic limits for supported workloads before implementing the fast path; use `@mayassert` for unsupported runtime inputs, not slower fallbacks.
- Implement the overflow-safe `:auto` decision from the global neighbor count and grid size. Keep the initial thresholds internal and provisional. An unrepresentable dense layout is a valid reason for `:auto` to choose sparse, but an explicit dense request must not silently change layouts.

**Gate:** Registration and geometry tests pass without building an MPI index. Cover inclusive grid bounds, continuous upper faces, periodic seams, cell-count limits, zero and subunit grid radii, and rejection of zero continuous radius. Check that dense IDs and sparse tuple keys produce the same cell order. Exercise the automatic selection arithmetic without allocating huge arrays.

## 2. Collect rank-local cell vectors

- Iterate over the local neighbor agents. Use the per-type collection logic in `src/GeometricMethods.jl` as the semantic reference for mortality, filters, projection, and IDs. For each surviving neighbor, evaluate `posfunc` once, compute its cell, and `push!` its record into that cell's typed vector. Allocate vectors only for occupied cells.
- `(position, ID, projection)` is the full record shape, not an unconditional storage format. Omit the projection field when the registered projection type has zero size, including `Nothing`; the query API still returns the corresponding typed value. Keep the ID field in every record.
- The result is a set of rank-local cell vectors keyed by cell. Do not flatten or sort the records, compute index offsets, or build the dense or sparse published index at this stage.

**Gate:** Check the keys and contents of the local cell vectors, including filtered and empty input, multiple neighbor types, and distinct agents sharing a position.

## 3. Route cell contributions and compute shared offsets

- Obtain world-to-local rank mapping from `mpi.comm` and `mpi.shmcomm`; explicitly verify equal ranks per node. Choose one deterministic set of contiguous cuts in the common cell order, independent of index layout. Define and test the cut representation so sparse ownership does not require a representable product of axis cell counts. Allow ranks with empty ownership ranges.
- Define the packed wire representation for cell keys, contribution lengths, and the aligned fields. Pack one bundle per local owner and reuse its send displacement for the corresponding rank on each node. Do not copy the bundle separately for every destination. Leave block-copy optimizations for measurements in Vahana.
- Exchange peer counts, then use `MPI_Alltoallv_c`. Use `MPI_Count` and `MPI_Aint`; displacements are in datatype extent units. Build receive displacements in sender order and send displacements in packed-bundle order, with checked sizes and conversions. Omit the zero-byte projection lane. Empty senders and receivers participate in the same collective sequence.

- After receiving the contributions, each owner sums their vector lengths per cell. Exchange owner totals within each node to compute each rank's start in the shared payload and, for sparse, its start in the shared key and offset arrays. Within those ranges, compute the published cell offsets in cell order: dense stores an offset for every cell, including empty ones; sparse stores an offset only for each occupied cell and publishes the occupied keys in order. Both use the same prefix-sum rule and one final sentinel. These offsets describe the shared index and are distinct from the MPI transfer displacements.

**Gate:** Verify that each owner receives exactly the expected cell contributions and computes the expected shared ranges and offsets for both layouts. Include repeated cell keys from different senders, empty ranks, more ranks than cells, and asymmetric populations. Test layout arithmetic near the `Cint` range without allocating large payloads. Use several ranks on one node and, if helpful for debugging, simulated node groups; neither replaces the physical multi-node gate.

## 4. Allocate, populate, and release shared windows

- Allocate typed shared windows collectively on `mpi.shmcomm` using the sizes and ranges computed in step 3. A single rank can supply the storage, while each owner writes its disjoint payload and index ranges. Give one rank responsibility for writing the final offset sentinel. Keep the windows alongside the typed views in the prepared state.
- Copy each received cell contribution into its final shared range without sorting the payload. Keep positions, IDs, and any stored projections aligned. Publish the completed snapshot using the tested window synchronization protocol: shared-window access epoch, writer sync, node barrier, reader sync. Do not allow callbacks to read the arrays before publication. Keep any zero-length shortcuts collective; a rank's local emptiness alone must not change the call sequence.
- Wire `geom_prepare!` and `geom_release!` into the existing `src/Simulation.jl` lifecycle. Verify common neighborhood selection and ordering and the required read permissions before backend collectives. Preparation precedes `prepare_write!`; release follows the callbacks and clears `runtime.prepared`. Explicitly close and free all windows in the same node-local order.
- Report fatal preparation errors and abort the MPI job on multi-rank runs; propagate the exception in single-process runs. Do not introduce failure-agreement collectives. Keep structural checks for matching collectives and safe buffer sizes active even when model-input assertions are disabled.

**Gate:** Exercise real `apply!` calls with one rank and multiple ranks on one physical node. Inspect the published index in test callbacks without calling the query API. Check both layouts, repeated prepare/release, and identity and custom projection snapshots. Use a subprocess for MPI abort tests. Verify that ranks can read other owners' shared ranges and that no rank maintains a private global replica as part of the prepared state.

## 5. Implement and test neighborhood queries

- Implement `geom_hits` against the published `runtime.prepared` state. Select the representation once per query, enumerate candidate cell ranges without periodic duplicates, exclude the querying agent by ID, and filter by exact `geom_distance`. Implement the existing ID, projection, pair, and projection-type hooks. An empty index must yield no hits; a zero-byte projection must return its registered value without stored payload bytes.
- Use the agreed fatal assertion path for query precondition failures. Do not add MPI collectives or fallback searches to queries.

**Gate:** Exercise all six public query forms in real `apply!` callbacks against a brute-force reference, irrespective of result order. Verify self-exclusion with overlapping types and preservation of distinct agents at the same position. Cover both layouts and `:auto`, all three metrics, mixed periodicity, boundary hits, co-located distinct agents, multiple neighbor types, and empty results. Compare with `ReplicatedKDTree` on inputs it supports (currently Euclidean). Check inference for literal-name calls with and without hits and verify that the iterator does not materialize a result buffer on the normal path.

## 6. Multi-node acceptance and performance

- Extend the serial and MPI spatial tests under `test/` and `test/mpi/`, reusing model fixtures and the reference search where practical. Run the existing geometric and spatial suites to protect the KD-tree path; include the new tests in the normal test entry points.
- Run on at least two physical nodes with equal ranks per node and a placement supported by Vahana's MPI initialization. Include uneven input volumes, an empty rank, a node with no locally owned neighbors, and a globally empty snapshot. Validate all searching ranks and repeated prepare/release with both layouts.
- Measure actual-field workloads with assertions disabled after warm-up. Separate collection/grouping, pack, MPI exchange, shared placement/publication, query, and release. Report shared bytes per node and peak private staging by rank. Compare with `ReplicatedKDTree` across cell densities, query volumes, projection sizes, ranks per node, and skewed owner workloads. Use those measurements to tune `:auto` and decide whether block copies or other optimizations are worth adding.

**Done:** The backend passes the reference and MPI tests on physical nodes, the shared snapshot is published and released correctly, and measurements confirm there is no private global replica on every rank. Record single-host and multi-node results separately.
