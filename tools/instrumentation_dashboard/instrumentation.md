# Instrumentation dashboard

For post-hoc analysis, the repository contains a local Dash prototype that reads Vahana instrumentation HDF5 files **directly**. It uses Python `h5py` to read `/instrumentation/events`; it does not need a Julia process or an intermediate JSON/Arrow export.

## Setup and run

From the repository root:

```bash
uv sync --project tools/instrumentation_dashboard
tools/instrumentation_dashboard/start.sh
```

With no argument, the launcher uses `h5/InstrumentationExample.h5`. You can
also pass an HDF5 file/base name and Dash options, e.g.:

```bash
tools/instrumentation_dashboard/start.sh results/my_run --port 8060
```

Then open <http://127.0.0.1:8050> (or the port you selected). The app binds to
localhost by default and starts in Dash debug/hot-reload mode; use `--no-debug`
to disable it. Do not expose debug mode on a public or untrusted network. Event
data is read at startup, so replacing the HDF5 file requires restarting the app.

See `tools/instrumentation_dashboard/README.md` in the repository for details and tests. The current design/limitations are recorded in `tools/instrumentation_dashboard/CONCEPT.md`; `tools/instrumentation_dashboard/mockup.html` is only a synthetic visual reference.

## Current views

- `apply_total` duration by transition, with median and maximum across ranks.
- A transition-filtered Gantt timeline, with one lane per MPI rank, a translucent `apply_total` background, and event-category colors. Uncovered time is marked as not separately instrumented (not automatically idle); very short intervals use markers while preserving their actual durations in hover details.
- Live-agent distribution by rank and agent type, from the rank-local `items` value on `:transition` events.
- Per-agent min/mean/max and standard deviation when diagnostic `transition_stats` events were recorded.

This is an initial local prototype: it loads the event dataset into memory, and does not yet provide multi-run comparison or a high-scale, incremental trace browser.

## HDF5 schema and compatibility

The reader understands the current `/instrumentation/events` compound dataset, including fixed-width `VString{32}` values, MPI rank IDs, and split Int128 diagnostic sums. It supports both the shared MPI file and per-rank HDF5 files. For older records without an explicit `rank` field, it infers rank from HDF5 partition attributes or the per-rank filename.

The current file format stores enum fields as integer IDs without persisting their name-to-ID mapping. The Python reader therefore mirrors the enum order from `src/Instrumentation.jl`; keep `tools/instrumentation_dashboard/hdf5_reader.py` synchronized if those enum definitions change. Adding enum-name metadata to a future HDF5 schema would remove this coupling.

## Measurement caveats

Per-agent timings are diagnostic measurements and add overhead. Enable diagnostics consistently on all MPI ranks before the measured operations, and use those timings for diagnosis rather than benchmarking:

```julia
enable_instrumentation!(true)
enable_diagnostics!(sim, true)
```

The transition `items` value is the rank-local number of live agents of that type at entry to `apply!`, not the number of allocated storage slots. Counts are recorded for types that have a transition event in that `apply!` call.
