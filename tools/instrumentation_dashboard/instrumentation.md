# Instrumentation dashboard

The local Dash dashboard reads `/instrumentation/events` directly with `h5py`,
without a Julia process or intermediate export. From the repository root:

```bash
tools/instrumentation_dashboard/start.sh
# Or start in a particular directory:
tools/instrumentation_dashboard/start.sh /path/to/h5
```

Open the printed localhost URL. In the app, browse or enter a server-local
folder, scan for traces, select two runs and choose the active run for
single-run views. The sidebar separates the filters, the two-run step-duration
and rank-phase plots, rank-local timelines, a paginated event table, and the
agent-distribution page. Select multiple transition names, phase and step on
the Filters page.

The launcher uses Dash debug/hot reload by default; `--no-debug` disables it.
Do not expose this filesystem-browsing application to an untrusted network.
Selected files are cached by file metadata and reloaded when their size or
modification time changes. The cache is local to the server process.

See [README.md](README.md) for usage and [CONCEPT.md](CONCEPT.md) for the exact
measurement semantics and limitations. In particular, `apply_total` includes
synchronization, recorded communication bytes are not necessarily physical
network traffic, and agent counts refer to live agents at transition entry.
