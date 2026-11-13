# Instrumentation dashboard notes

- `dashboard.py`: current Dash layout, directory scan, session-local controls,
  sidebar pages, callbacks and bounded process-local trace/phase caches.
- `fast_reader.py`: current chunked vectorized HDF5 reader. Prefer event enum
  attributes over fallback mappings; retain legacy rank/field handling.
- `analysis.py`: step and rank plots, phase aggregation and transition metrics.
  `event_views.py`: shared event filtering and rank-local timeline figures.
  See `CONCEPT.md` for their meaning and limitations.
- `app.py`: command-line entry point; also contains older dashboard helper
  functions retained for compatibility with legacy tests.
- `../../src/Instrumentation.jl`: authoritative current event schema and
  measurement semantics. `../../test/mpi/test_instrumentation.jl` tests events
  and HDF5 persistence. Avoid summing nested durations or treating missing GC
  data as zero.
- `../instrumentation_marimo/instrumentation_insights.py`: marimo comparison
  notebook. If a session is running, its kernel is authoritative; inspect or
  modify it via the `marimo-pair` skill, not direct edits.
- `~/mdu2/archive/vahana-related/Implementierungsplan_Vahana_Instrumentierung.md`:
  instrumentation design and implementation plan; it may lag current code.

Run tests from this directory with `.venv/bin/python -m unittest` or via the
project environment. Keep dashboard UI text in English. Do not store per-user
selection in Python globals; cached trace data must not be mutated by callbacks.
