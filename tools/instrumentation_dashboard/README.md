# Vahana instrumentation dashboard (Dash)

This prototype reads Vahana's HDF5 instrumentation data directly using `h5py`.
It does not start Julia and does not create a JSON/Arrow intermediate file.

## Setup and run with uv

From the repository root, `uv` creates/synchronizes the project environment
from `pyproject.toml` and `uv.lock`:

```bash
uv sync --project tools/instrumentation_dashboard
```

The launcher defaults to the checked-in sample file and starts Dash with hot
reload enabled:

```bash
tools/instrumentation_dashboard/start.sh
# Open http://127.0.0.1:8050
```

Pass either a concrete HDF5 filename or a base filename for per-rank files
named `<base>_<rank>.h5`:

```bash
tools/instrumentation_dashboard/start.sh results/my_run
```

The app binds to localhost by default. To select a different port or host:

```bash
tools/instrumentation_dashboard/start.sh results/my_run --port 8060
```

The launcher enables Dash's automatic Python-code reload and browser refresh by
default. Add `--no-debug` to turn it off:

```bash
tools/instrumentation_dashboard/start.sh results/my_run --no-debug
```

The HDF5 events are loaded at process startup. Editing Python files triggers a
reload and rereads the file; changing/replacing the HDF5 file by itself does not
currently trigger a reload. Restart the app to load changed data. Do not expose
Dash debug mode on a public or untrusted network.

## Current views

- `apply_total` duration by transition (median and maximum across ranks)
- Event timeline/Gantt view for the selected transition, with rank-local relative
  timestamps, a translucent `apply_total` background interval, and event-category
  colors. Uncovered portions are explicitly described as not separately
  instrumented—not automatically idle time. Sub-pixel events appear as markers
  with their true duration in the tooltip.
- Live-agent distribution across ranks for the selected agent type
- Per-agent diagnostic timing table when `transition_stats` events are present

Run the direct-reader and Dash integration tests (includes the real three-rank
`h5/InstrumentationExample.h5` sample):

```bash
uv run --project tools/instrumentation_dashboard \
  python tools/instrumentation_dashboard/test_reader.py -v
uv run --project tools/instrumentation_dashboard \
  python tools/instrumentation_dashboard/test_app.py -v
```

The app reads `/instrumentation/events` directly. It decodes the fixed-width
`VString{32}` fields and current enum IDs written by Vahana. Older compound
records without an explicit `rank` field are supported when HDF5 partition
attributes or per-rank filename suffixes are available.

### HDF5 schema note

The current event dataset stores enum values as integers but does not persist a
name-to-ID enum table. `hdf5_reader.py` therefore contains the current enum
mapping from `src/Instrumentation.jl`. If the Julia enum order changes, update
the Python mapping in the same change. A later schema version should save enum
name mappings in the `/instrumentation` group so readers no longer need a
hard-coded copy.

## Design notes

- `CONCEPT.md` records the current Dash/direct-HDF5 decisions, the meaning of uncovered timeline time, and known limitations.
- `mockup.html` is a visual reference with synthetic values; it is not the live Dash UI.
