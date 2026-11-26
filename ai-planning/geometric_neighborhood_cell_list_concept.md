# Work Package E: Node-Shared Cell List

## 1. Goal and execution order

This concept describes **Node-Local Shared Indexes** from `~/mdu2/texte/plan_geometric_neighborhood/geometric_neighborhood.org`. It elaborates work package E in the [implementation plan](geometric_neighborhood_implementation_plan.md#8-later-work-package-e-node-local-shared-cell-list) within the [geometric neighborhood API](geometric_neighborhood_api.md).

The backend changes **how the snapshot is stored and searched, not which data is replicated**. Each node receives all living neighbors that pass the registered type and filter restrictions. Each node holds one read-only index shared by all its ranks. This is the fully replicated baseline for evaluating partitioning and later selective communication: no occupancy masks, query routing, halos, changes in ownership, or caching across calls. Spatial locality does not reduce the logical replication volume, though sender load and MPI transport costs can vary.

For each selected neighborhood, the runtime sequence is:

| When | Phase | Result |
| --- | --- | --- |
| `geom_prepare!`, after `prepare_read!` and before `prepare_write!` | [Build local cell lists](#2-build-local-cell-lists) | Each rank maps its own filtered neighbors to cells in the common grid. |
| Same `geom_prepare!` call | [Build and publish the global cell list](#3-build-and-publish-the-global-cell-list) | Every node has a shared index over the global snapshot. The returned state is stored in `runtime.prepared`. |
| Transition callbacks in `apply!` | [Search for neighbors](#4-search-for-neighbors) | Each rank queries the published index locally, without MPI. |
| After all callbacks | [Release the snapshot](#5-release-and-failure-handling) | `geom_release!` frees shared resources, then `apply!` clears `runtime.prepared`. |

### Backend configuration and priorities

Select `NodeSharedCellList` at registration; `ReplicatedKDTree` remains the default backend. The optional `cell_size` is a finite, strictly positive target width in coordinate units. The proposed default is the search radius when positive. For `GridSpace`, radii below one coordinate unit are allowed, including zero; at radius zero, the default `cell_size` is one coordinate unit. For `ContinuousSpace`, the radius must be strictly positive; reject zero at registration, even when an explicit `cell_size` is given. The `cell_index` keyword selects the **published index**: `:dense`, `:sparse`, or `:auto` (default). Registration validates the options and requires a concrete `isbits` projection type. Initial metric support is limited to Euclidean, Cityblock, and Chebyshev.

The initial backend always stores agent IDs for self-exclusion and ID-returning queries. An `ignore_ids` mode is a possible later optimization, not part of this work package.

**Performance takes priority over supporting rare corner cases.** Define the supported domain explicitly. Use `@mayassert` for unsupported inputs and stop the simulation if a check fires rather than switching silently to a slower search. With assertions disabled, the preconditions still apply. Compute and validate invariants at registration or preparation where possible. Checks required for memory safety or matching MPI collectives must not depend on `@mayassert`. Empty snapshots and empty local contributions are normal MPI inputs, not exceptional cases to reject.

Implement the existing hooks in `src/geom_backends/NodeSharedCellList.jl`; no new `apply!` argument or backend-specific branch in the public query API is needed. Before enabling E, ensure all ranks agree on selected neighborhoods and process them in the same registration order. The current `unique`-based selection preserves caller order; it does not check cross-rank agreement. Check the query and neighbor read requirements before backend collectives.

## 2. Build local cell lists

This is the first part of `geom_prepare!`. Each rank works only with its own neighbors in the readable snapshot:

1. **Construct the cell geometry for the registered space.** Every rank uses the same geometry, derived from the space and `cell_size`, not from the local population. Use separate implementations for `GridSpace` and `ContinuousSpace`: each determines the cell count per axis, maps positions to cell keys, and converts query intervals to candidate cell ranges. See [Space-specific geometry](#space-specific-geometry) below.
2. **Collect the local snapshot.** Reuse the per-agent-type collector for living neighbors that pass the registered filter. Keep each position, `AgentID`, and projection aligned through any local concatenation. A borrowed identity-projection vector is read-only and can be consumed during preparation, before callbacks can update it. Use `@mayassert` for invalid mapped coordinates and unsupported numerical cases, rather than clipping positions or introducing a slower path.
3. **Group records by cell during collection.** Compute each neighbor's cell coordinates once, from its position, and append the position, ID, and any stored projection to that cell's rank-local contribution. One typed vector per occupied cell is a starting representation; the dense layout does not require a private vector for every possible cell on each rank. A rank with no eligible neighbors has no cell contributions. In `:auto` mode, use the global filtered count to choose the published layout before payload transfer. If needed, convert the keys of occupied cells for the chosen layout without recomputing cell membership from positions.

The rank-local contributions are preparation data, not part of the published `runtime.prepared` state. A rank that detects a fatal preparation failure follows the abort path in [Release and failure handling](#5-release-and-failure-handling) rather than skipping the next collective.

### Space-specific geometry

**`GridSpace`: discrete positions.** An axis from `lo` through `hi` contains `hi - lo + 1` valid positions: the integers `lo, lo + 1, …, hi`. This is the number of possible agent positions, not the number of search cells. `cell_size` controls how these positions are grouped into search cells. Use integer offsets from `lo` and widened integer arithmetic for cell assignment; do not round large integer coordinates through `Float64`. A one-position axis needs only one cell if supported. On a periodic axis, `hi` is a valid position distinct from `lo` and is one step away from it across the seam; the period is `hi - lo + 1`. The discrete query geometry must support radii below one, including zero: only agents at exactly the same position can match in that case.

**`ContinuousSpace`: continuous positions.** An axis has length `hi - lo`, not a count of valid positions. Divide this extent into search cells using `cell_size`, and map coordinates to cells using continuous offsets from `lo`. On a non-periodic axis the upper face `hi` belongs to the last cell. On a periodic axis `hi` is the seam and represents the same position as `lo`; the period is `hi - lo`. The continuous query geometry assumes a strictly positive radius and must account for floating-point rounding in the supported range.

#### Cell count and effective width

Let `s = cell_size`. For each axis, choose the cell count `n` by rounding down, then divide the whole axis into `n` equal-width cells. Use this rule for both periodic and non-periodic axes; there is no shorter remainder cell.

| Space | Axis measure `L` | Cell count `n` | Effective width `w` |
| --- | --- | --- | --- |
| `GridSpace` | `hi - lo + 1` valid positions | `max(1, min(L, floor(L / s)))` | Exact ratio `L / n` |
| `ContinuousSpace` | `hi - lo` coordinate units | `max(1, floor(L / s))` | `L / n` coordinate units |

Rounding down keeps `w >= s` in exact arithmetic unless the entire axis is shorter than `s`. When `s` defaults to the radius, this limits candidate-cell lookups; the trade-off is potentially more agents per cell. The grid cap avoids creating more cells than valid positions when `s < 1`. Check representability before converting counts or allocating index arrays.

- **`GridSpace`:** The zero-based key for position `x` is `floor((x - lo) * n / L)`. Use widened integer arithmetic and integer division, without rounding the effective width or converting positions to floating point. Check that intermediates are representable. For `(lo, hi) = (1, 7)` and `s = 3`, `n = 2` and `w = 7/2`; the cells contain `{1, 2, 3, 4}` and `{5, 6, 7}`. Equal widths do not imply equal numbers of discrete positions per cell.
- **`ContinuousSpace`:** For an interior or canonical position, the zero-based key is `floor((x - lo) / w)`, subject to the floating-point preconditions of the supported domain. A non-periodic position at `hi` belongs to cell `n - 1`; a periodic position at `hi` is normalized to `lo` and belongs to cell `0`. Use the same geometry for cell assignment and candidate-range computation, accounting for rounding so valid hits are not missed.

For the same periodic bounds `(1, 7)`, the grid period is seven and positions `7` and `1` are one step apart. The continuous period is six and `7` and `1` represent the same location.

Only the geometry is space-specific. Both implementations produce cell keys and candidate cell ranges for the same dense or sparse index. They share the payload layout, MPI protocol, candidate traversal, and exact-hit filtering. Dispatch on the space type before traversing candidates, not for each hit.

### Dense and sparse cell index layouts

Both layouts use the same cell geometry. The layout determines both the keys used to prepare each rank's contribution and how the published index maps a cell to its records.

| Layout | Cell key used for exchange and indexing | Published index per node and lookup |
| --- | --- | --- |
| Dense | Linear cell ID. No private dense table is required on each rank. | `cell_count + 1` offsets in linear cell order, including empty cells; no key array. Two adjacent offsets locate the record range; equal offsets mean an empty cell. |
| Sparse | Tuple of integer cell coordinates. The total number of possible cells need not be representable. | Sorted keys for occupied cells plus their offsets. Binary-search the keys to find a record range. |

For `:auto`, use the grid dimensions and global count of filtered neighbors to choose the same layout on every node **before** allocating index arrays. Do not build a sparse index just to count occupied cells. Prefer dense if the total cell count and offset length are representable, the estimated peak dense index storage per node fits an internal byte budget, and the cell count is not large relative to the payload count. Start with **64 MiB** for dense offsets plus grid-sized build scratch and a relative limit of `max(4096, 4 × neighbor_count)` cells; tune both limits with forced dense and sparse runs. Otherwise choose sparse. Check products and size comparisons without overflow. Explicit `:dense` and `:sparse` choices are not silently changed; the automatic limits do not apply to explicit choices, but representability and collective failure checks still do.

## 3. Build and publish the global cell list

This is the second part of `geom_prepare!`. Combine the rank-local contributions so that every node can search the complete filtered snapshot. The **global** cell list refers to the data covered by the index, not to a single MPI window spanning nodes: there is one copy per node in that node's shared memory.

Both layouts group the same compact payload arrays by cell: registered `SVector` positions, `AgentID`s, and any stored projections aligned at the same one-based indices. Offsets are one-based, with the sentinel `neighbor_count + 1`. The dense build derives offsets by prefix-summing cell counts across the whole grid. The sparse build orders the occupied keys and derives offsets from their counts.

A globally empty snapshot must yield no hits. It may use a compact representation with no agents or keys and one offset equal to `1` instead of allocating a grid-sized table. A zero-byte projection needs no payload transfer, datatype, or window; the accessors still provide a value of the registered projection type for each hit. The prepared type is concrete and determined at registration. It holds the geometry, typed views, and owning MPI windows, with a finite choice of index representation so `:auto` can change layouts between calls without changing the runtime's prepared type.

### Exchange and shared-memory construction

**Initial strategy: route each cell to its owner rank on every node, then assemble the shared index in parallel without sorting agent records.** Use `mpi.comm` for the exchange and `mpi.shmcomm` for construction and publication. The first implementation requires the same number of ranks on every node; verify this explicitly. Derive the mapping from world ranks to local rank numbers from the communicators, not from assumptions about world-rank placement.

1. **Assign cells to local ranks.** Use the same ordered grid and the same contiguous cell ranges for dense and sparse. Linear IDs and tuple keys are two representations of that order; sparse storage merely omits empty cells and does not change the assignment. Use the same assignment on every node. For the default partition, order the ranges by local rank so that node-local prefix sums give each rank one contiguous portion of the published payload. This assignment is for building the index; it does not change simulation agent ownership.
2. **Exchange rank-local contributions.** Bundle each rank's cell contributions by owner and send each bundle to the corresponding local rank on every node via `MPI_Alltoallv_c`. A bundle can reuse the same send-buffer range for each destination node. The receiving rank must know the cell key and length of each incoming contribution; the exact encoding is open. No node-leader gather or per-sender derived MPI receive type is needed. Empty senders still participate. A node with no local neighbors still receives the global contributions for its owners.
3. **Compute offsets and write in parallel.** Each rank counts records for its owned cells. Node-local counts and prefix sums determine each rank's start in the shared payload and, for sparse, the start of its occupied-key and offset ranges. Allocate the shared windows collectively on the node. A single rank may provide the backing storage, but every owner writes its own disjoint ranges, using the same slot for a record's position, ID, and any stored projection. Compute the cell offsets, then place records directly in the final cell ranges without sorting the payload. For sparse, order the occupied keys within each owner's range. There is one published global replica per node; each rank's receive buffer normally holds only its own portion.

When Vahana's cell partitioner is available, reuse its cell-to-rank mapping if it is already known at assignment time and applies to this grid. Otherwise use the contiguous-range default. Reuse requires a common mapping to local owner ranks on all nodes. If the partitioner assigns noncontiguous ranges to an owner, compute disjoint write ranges in cell order; the default's single prefix sum over local ranks does not describe that layout.

The [exchange benchmark](../tools/benchmark_cell_list_exchange.jl) supports this starting choice: with two simulated nodes of five ranks, 100,000 synthetic 8-byte records per node, and 64, 4,096, or 100,000 cells, counted placement ran in 0.45–0.57 ms versus 0.72–0.80 ms for sorted cell-owner placement and was faster than both leader-based variants. This was a single-host run with uniformly sampled cell numbers, not a measurement of inter-node networking, geometric cell assignment, or real projection storage. Test the world-wide exchange on physical nodes and with skewed occupancy before generalizing the result.

Whether to place individual records or copy contiguous cell blocks, and how best to lay out staging buffers, are optimizations to measure with real Vahana payloads. Measure peak staging separately from the published snapshot. Keep the leader-based `reorder` and `direct` strategies in the exchange benchmark as comparisons; they are not required in the initial backend.

### Large counts and compatibility

Exchange one payload count per peer across `mpi.comm`, then transfer the payload with `MPI_Alltoallv_c`. Use `MPI_Count` for the transfer counts and `MPI_Aint` for the displacements. Displacements are measured in units of the corresponding send or receive datatype extent, not bytes. Reuse the checked prefix-sum layout where it fits: build receive offsets from the counts in sender order, and build send offsets from the packed owner bundles. When the same bundle goes to the corresponding owner on several nodes, those destinations reuse its displacement; do not prefix-sum the expanded per-destination counts. Check conversions, array lengths, and window byte sizes before use and follow the fatal abort path on error. Test count and displacement arithmetic around the `Cint` limit without allocating large payload arrays.

### Publish the shared index

Expose typed views only after the data are ready. Reuse Vahana's `shared_array` handling or avoid querying an empty segment when its length is known. Keep the windows alive while views can be used. A barrier alone does not synchronize shared memory. The tested candidate protocol uses a passive `lock_all` epoch: after the last store, `Win_sync` on every writer, then a node barrier, then `Win_sync` on readers before their first loads. Finalize this sequence with the construction strategy. No writes occur during searches. All ranks follow the same phase order, including for a single process or a globally empty snapshot; omitted zero-length operations must be decided from shared metadata, not local emptiness.

The development checkout uses MPI.jl 0.20.23 with MPICH 4.3.1. The `MPI_Alltoallv_c` binding used by the benchmark is present locally, which does not guarantee the loaded MPI library on another system supports it. `Win_sync` is available directly, and `lock_all` and `unlock_all` are available through `MPI.API`. A local two-rank probe passed for the standard gather interfaces and shared-window publication with `SVector`, tuples, and a custom `isbits` projection, including empty data. The cell-owner protocol with real payload arrays and physical multi-node behavior still need backend tests.

## 4. Search for neighbors

This is the read-only part of `apply!`, inside transition callbacks. It uses the published `runtime.prepared` state and does not communicate via MPI.

1. The common query path gets the calling agent's position from its read state and passes the position and agent ID to `geom_hits`. An empty index yields no hits immediately. Validate query-side preconditions with `@mayassert` outside the candidate loop.
2. Use the registered space's geometry implementation to compute conservative candidate cell ranges once per query. The radius is a sufficient per-axis extent for all three initial metrics in exact arithmetic. Use discrete bounds for `GridSpace` and outward rounding where needed for `ContinuousSpace`, so the ranges are conservative relative to `geom_distance` in the supported domain. Split wrapped intervals into disjoint ranges to visit any periodic cell at most once. Define and assert numerical and geometric support limits where safe bounds or tractable enumeration are not available on the fast path. Do not add extended-precision or full-snapshot fallback searches for exceptional inputs.
3. Choose the layout-specific traversal once per query. For each candidate cell, compute the linear cell ID and read two offsets in the dense layout, or binary-search the occupied keys in the sparse layout. Walk the resulting payload ranges without temporary result buffers or per-hit validation of invariants.
4. Exclude the querying agent by `AgentID`. Test `geom_distance(space, query, position) <= radius` for the remaining candidates. Distinct agents at the same position are still eligible. The typed lazy hit iterator yields exact payload indices only. The existing accessor hooks read IDs, projections, or both directly at these indices, without another spatial search. Keep literal-name calls to all six public forms inferable, including empty results. Result ordering is not part of the contract.

## 5. Release and failure handling

After all transitions have finished, `geom_release!` ends any active access epoch and explicitly frees the windows in consistent order on all node ranks. The common lifecycle then clears `runtime.prepared`. Shared views must not be used after their windows are freed.

An enabled `@mayassert` throws an `AssertionError` on the affected rank; it does not by itself stop the other ranks. Catch the error at the preparation or query boundary, report the failing rank and error, and call `MPI.Abort(mpi.comm, 1)` in a multi-rank run. Use the same fatal path for detected preparation errors that would otherwise strand peers in a collective. In a single-process run, let the error propagate normally. There is no failure-agreement collective, retry, or replacement search, and no extra MPI communication on the successful path. Do not attempt rank-local collective cleanup or general recovery from transition exceptions or MPI runtime failures.

## 6. Implementation and acceptance

1. **Local lists and queries with ordinary arrays.** Test cell assignment, both published layouts, and typed traversal independently of MPI. Compare forced dense, forced sparse, and both outcomes of `:auto` against a brute-force reference with the same metric and snapshot, and against `ReplicatedKDTree` on common inputs. Cover ordinary grid and continuous inputs, mixed periodicity and seam crossings, exact-radius hits, identical positions, self-exclusion, and empty results. For `GridSpace`, test radius zero and positive radii below one: distinct agents at the same position match, and agents at different positions do not. Verify that radius zero defaults to a cell size of one coordinate unit in `GridSpace` and is rejected at registration in `ContinuousSpace`, even with an explicit cell size. Test unsupported runtime corner cases as `@mayassert` failures. Check overflow-safe layout selection and layout changes between snapshots. Keep the cell-key representation internal; it is not a commitment to a future partitioning grid.
2. **Shared global lists and real `apply!` calls.** Test one rank, multiple ranks on one node, and at least two physical nodes, with uneven populations, an empty rank, an empty node, and a globally empty snapshot. Include zero-byte and custom `isbits` projections. Compare all six result forms with the reference. Check that self-exclusion preserves distinct agents at the query position. Check inference, snapshot isolation, repeated preparation with automatic layout changes, and explicit release. Use equal ranks per node initially and rank placement supported by the current Vahana MPI initialization. Test that routing does not rely on contiguous world-rank numbering. A workstation run with several ranks is not multi-node acceptance.
3. **Performance and memory.** Measure collection and local grouping, packing, owner exchange, shared placement and publication, query, and release separately. Include skewed workloads to expose load imbalance across cell owners. Record window bytes per node and peak build scratch by rank; do not sum shared pages as private RSS. Compare forced dense, forced sparse, and automatic selection against `ReplicatedKDTree` across query volumes, ranks per node, cell sizes, spatial densities, and projection sizes. Measure warmed runs with assertions disabled, including iterator allocations and candidate-cell lookup costs. Run correctness and rejection tests with assertions enabled.

## References

- [MPI.jl: communicators](https://juliaparallel.org/MPI.jl/stable/reference/comm/)
- [MPI.jl: collective communication](https://juliaparallel.org/MPI.jl/stable/reference/collective/) and [buffers](https://juliaparallel.org/MPI.jl/stable/reference/buffers/)
- [MPI.jl: one-sided communication](https://juliaparallel.org/MPI.jl/stable/reference/onesided/) and [low-level API](https://juliaparallel.org/MPI.jl/stable/reference/api/)
- [MPI 4.1: flush and sync](https://www.mpi-forum.org/docs/mpi-4.1/mpi41-report/node331.htm) and [shared-memory synchronization examples](https://www.mpi-forum.org/docs/mpi-4.1/mpi41-report/node342.htm)
