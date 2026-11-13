# Vahana instrumentation dashboard

Explore and compare local HDF5 instrumentation traces with Dash, without a
Julia process or intermediate conversion.

```bash
tools/instrumentation_dashboard/start.sh
# optional concrete initial file
tools/instrumentation_dashboard/start.sh h5/InstrumentationExample.h5
```

Open the printed local URL. Enter a server-local HDF5 directory or browse its
parent/child directories; Scan finds files with instrumentation datasets. Pick
two traces, select an active trace for single-run views, and use the sidebar
to switch between Filters, Plots, Timelines, Events and Agents. The Filters page's transition
table filters both plots; the phase table and step control the rank plot, and
the step filter also limits all table metrics. The rank table filters the rank
plot and agent view. The first plot's duration
selector switches between per-step and cumulative run comparisons. The first
plot shows `apply_total` when all phases are selected and the selected phase
sum otherwise. All three tables show a non-selectable selected-total row. Phase buttons select all,
none, pre-transition or post-transition individual phases; the latter groups
include synchronizing work. Timelines provides rank-local transition event
timelines for both traces; Events provides a paginated table filtered by the
shared selections. The Agents page filters by transition and type. Directory
and trace choices are stored locally in the browser.

The dashboard binds to `127.0.0.1` by default. Do not expose it to untrusted
users: directory browsing can read server-local files. Use `--no-debug` to
disable the development reloader. The command also accepts `--host` and
`--port`. See [CONCEPT.md](CONCEPT.md) for measurement semantics, caching,
legacy-file behavior and limitations.

Run tests from this directory with `.venv/bin/python -m unittest`.
