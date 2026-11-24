# TODO

## Collect geometric neighbors per agent type without redundant copies

- [x] Refactor the optional local collection helpers in `src/GeometricMethods.jl` to collect one batch per registered neighbor type. Check the `:Immortal` hint once per type and dispatch to a dedicated helper. The mortal helper keeps the `read.died` check; the immortal helper never reads the death flags. Both apply `neighbor_filter` if present and keep positions, IDs, and projections aligned. Leave merging and replication to the backend; do not add a `borrow_identity` flag.
- [x] For an unfiltered `:Immortal` batch, every slot contributes one entry. Evaluate `posfunc` once per state and generate the IDs with `collect(base_id .+ (1:length(states)))`, where `base_id = agent_id(typeid(sim, agent_type), AgentNr(0))`. Do not add a separate slot-limit check here. With `project === identity` and a matching registered projection type, return `read.state` itself as the projection vector, not a copy. For filtered batches, retain only the matching slots; for other projections, compute the registered projection values.
- [x] Adapt `ReplicatedKDTree` to the per-type collector. Only positions are merged for the shared tree. Self-exclusion and result accessors use cumulative batch ends to locate IDs and projections; the pair accessor resolves the index once. A single batch needs neither merging nor segment lookup.
- [x] Preserve snapshot ownership. MPI retains the owned projection batch from `join`. In serial, the backend copies a borrowed `read.state` for `:Independent` neighbors before callbacks can overwrite it in place.
- [x] Run the existing geometric tests in serial and with two MPI ranks. Extend `test/spatial_neighbors.jl` to verify that both buffered and `:Immortal, :Independent` neighbors expose the old identity projection throughout the current `apply!` and the updated state in the next one. Both variants passed in both runs.
- [ ] Complete targeted multi-type coverage: verify alignment for filtered and unfiltered batches and non-identity projections, self-exclusion at segment boundaries, and empty batches on individual ranks.
- [x] Compare end-to-end `geom_prepare!` and collection against the previous combined join in serial and on two MPI ranks on one node (results below). Multi-node scaling and filtered workloads were not benchmarked.

**Preparation benchmark.** Baseline: `b72e5da`; comparison: working tree with per-type batches. Julia 1.12.6 on a Ryzen 9 7900X, one thread per rank, 131,072 immortal agents globally, uniformly sampled 2D positions, no filter, 64-byte states. The two-type case splits agents equally and uses the same 48-byte projection as the one-type projected control. Median of 19 runs after 3 warmups, full GC and rank barrier before each sample. Times and Julia-allocated bytes are maxima across ranks per sample; bytes are not peak resident memory. Preparation includes KD-tree construction but excludes simulation initialization and queries. Collection (including joins) was timed independently.

| Ranks | Types / projection | Prepare ms, old → new | Prepare MiB, old → new | Collect ms, old → new |
|---:|---|---:|---:|---:|
| 1 | 1 / identity | 27.75 → 21.84 | 33.20 → 9.21 | 5.94 → 0.43 |
| 1 | 1 / projected | 29.59 → 21.97 | 39.20 → 15.21 | 8.26 → 2.10 |
| 1 | 2 / projected | 30.21 → 22.59 | 43.70 → 18.21 | 9.03 → 2.26 |
| 2 | 1 / identity | 26.14 → 23.43 | 30.71 → 18.72 | 7.76 → 1.76 |
| 2 | 1 / projected | 27.29 → 23.59 | 31.71 → 19.72 | 5.24 → 1.79 |
| 2 | 2 / projected | 26.25 → 23.64 | 35.36 → 22.72 | 5.76 → 2.41 |

At 4,096 global agents, two-type preparation also improved: 0.738 → 0.541 ms serial and 0.776 → 0.655 ms on two ranks. Per-type collection incurs additional work: at 131,072 agents on two ranks, current collection took 2.41 ms for two types versus 1.79 ms for one with the same projected payload. This difference includes merging positions as well as extra collectives; it is not an isolated MPI latency measurement.

**Background.** The old `ConnectSpatial.jl` collected and joined one type at a time. Its `_get_ids_poss_states` wrapper appended the per-type results when several types were requested (and appended the projection data only when an edge constructor was present). The previous collector flattened the local data before `join`. Moving that flattening after `join` would only move the projection copy; keeping the per-type vectors is what avoids it.

**Local benchmark before the collector refactor.** With 600,000 agents of one immortal type, no filter, and identity projection (Julia 1.12.6 on aarch64, median of 17 warmed runs excluding reported GC time), the previous collector took 1.81 ms / 18.31 MiB for 16-byte states and 3.06 ms / 45.78 MiB for 64-byte states. Returning `read.state` directly with one `agent_id` call per slot took 1.23 ms and 1.42 ms, respectively, at 9.16 MiB in both cases. Returning `read.state` and generating IDs from a base ID plus the slot range took 0.19 ms and 0.40 ms at the same 9.16 MiB. The variants produced the same results; MPI, tree construction, and query costs were not measured.

## Other follow-ups

- [ ] **Deferred until the cell index is implemented.** Keep collecting IDs for now; do not change the temporary KD-tree or MPI joins for this optimization. Once the cell index defines how hits identify individual agents, revisit self-exclusion and benchmark an optional `ignore_ids` mode for geometric neighborhoods used only through `geom_neighborstates` or `geom_neighborstates_iter`. Skip the MPI join of neighbor IDs even when query and neighbor types overlap: the querying agent is local to its rank, so self-exclusion can use a local ID-to-index mapping. Measure preparation time, MPI traffic, and memory use, and verify that other agents at the same position remain valid hits.

- [ ] Switch `join` in `src/MPI.jl` to `MPI_Allgatherv_c` (MPI-4 large counts). Exchange local vector lengths as `Int64` values using `Allgather`, then transfer the data using `MPI_Count` counts and `MPI_Aint` displacements. Check compatibility with older MPI libraries, test count and displacement calculations at the `Int32` limit, and benchmark against the current `Alltoallv` implementation.
