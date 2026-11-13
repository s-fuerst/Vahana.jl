# /// script
# dependencies = [
#     "h5py==3.16.0",
#     "marimo",
#     "numpy==2.5.4",
#     "pandas==3.1.0",
#     "plotly==7.1.0",
#     "polars==2.1.0",
# ]
# requires-python = ">=3.13"
# ///

import marimo

__generated_with = "0.25.2"
app = marimo.App(width="columns")


@app.cell(column=0, hide_code=True)
def imports():
    import re
    from pathlib import Path
    from typing import Any

    import h5py
    import numpy as np
    import plotly.express as px
    import plotly.graph_objects as go
    import marimo as mo

    import polars as pl


    return Any, Path, go, h5py, mo, np, pl, px, re


@app.cell(hide_code=True)
def reader(Any, Path, h5py, np, pl, re):
    """Direct reader for Vahana's /instrumentation/events HDF5 dataset.

    Reads the event compound dataset in-place with h5py and decodes the
    fields vectorized with numpy, in chunks to keep peak memory bounded.
    Enum names are read from the 0-indexed string attributes
    `event_labels`, `contexts`, and `kinds` in the `/instrumentation` group.
    """
    _CHUNK_SIZE = 250_000
    _REQUIRED_FIELDS = {
        "label", "caller", "type", "context", "kind", "step", "transition_nr", "items",
        "start_ns", "dur_ns", "send_items", "recv_items", "send_bytes",
        "recv_bytes", "agent_min_ns", "agent_max_ns", "agent_sum_ns",
        "agent_sum_sq_hi", "agent_sum_sq_lo", "gc_pause_ns",
    }
    _RANK_FILE_RE = re.compile(r"_(\\d+)\\.h5$")


    def _vstring_bytes(bulk: np.ndarray, field: str) -> np.ndarray:
        """Extract a VString{32} column of a bulk compound read as uint8 (n, 32)."""
        column = bulk[field]
        while column.dtype.names:
            if column.dtype.names == ("data",):
                column = column["data"]
            else:
                column = np.column_stack([column[name] for name in column.dtype.names])
        return np.ascontiguousarray(column, dtype=np.uint8).reshape(-1, 32)


    def _decode_vstring(raw: np.ndarray) -> np.ndarray:
        """Decode a NUL-padded uint8 (n, 32) column into an object array of str."""
        packed = raw.view("S32").reshape(-1)
        uniques, codes = np.unique(packed, return_inverse=True)
        table = np.empty(len(uniques), dtype=object)
        table[:] = [bytes(u).rstrip(b"\0").decode("utf-8", errors="replace") for u in uniques]
        return table[codes]


    def _read_enum_names(group: h5py.Group, attr: str) -> list[str]:
        return [
            value.decode("utf-8") if isinstance(value, bytes) else str(value)
            for value in np.asarray(group.attrs[attr]).reshape(-1)
        ]


    def _resolve_files(filename: str | Path) -> list[tuple[Path, int | None]]:
        """Resolve a Vahana HDF5 filename or a base name with per-rank files."""
        requested = Path(filename).expanduser()
        if requested.is_file():
            match = _RANK_FILE_RE.search(requested.name)
            return [(requested.resolve(), int(match.group(1)) if match else None)]

        if requested.suffix.lower() == ".h5":
            base = requested.with_suffix("")
        else:
            base = requested
            candidate = requested.with_suffix(".h5")
            if candidate.is_file():
                return [(candidate.resolve(), None)]

        candidates = []
        for path in base.parent.glob(base.name + "_*.h5"):
            match = _RANK_FILE_RE.search(path.name)
            if match:
                candidates.append((path.resolve(), int(match.group(1))))
        candidates.sort(key=lambda item: item[1])
        if candidates:
            return candidates

        raise FileNotFoundError(
            f"No Vahana HDF5 file found for {filename!s}; expected the file itself, "
            "<base>.h5, or per-rank files <base>_<rank>.h5."
        )


    def _decode_chunk(
        bulk: np.ndarray,
        label_names: list[str],
        context_names: list[str],
        kind_names: list[str],
        source_file: str,
    ) -> pl.DataFrame:
        label_ids = bulk["label"].astype(np.int64)
        context_ids = bulk["context"].astype(np.int64)
        kind_ids = bulk["kind"].astype(np.int64)
        frame = pl.DataFrame({
            "label": np.asarray(label_names, dtype=object)[label_ids],
            "label_id": label_ids,
            "caller": _decode_vstring(_vstring_bytes(bulk, "caller")),
            "type": _decode_vstring(_vstring_bytes(bulk, "type")),
            "context": np.asarray(context_names, dtype=object)[context_ids],
            "kind": np.asarray(kind_names, dtype=object)[kind_ids],
            "rank": bulk["rank"].astype(np.int64),
            "step": bulk["step"].astype(np.int64),
            "transition_nr": bulk["transition_nr"].astype(np.int64),
            "items": bulk["items"].astype(np.int64),
            "start_ns": bulk["start_ns"].astype(np.int64),
            "dur_ns": bulk["dur_ns"].astype(np.int64),
            "send_items": bulk["send_items"].astype(np.int64),
            "recv_items": bulk["recv_items"].astype(np.int64),
            "send_bytes": bulk["send_bytes"].astype(np.int64),
            "recv_bytes": bulk["recv_bytes"].astype(np.int64),
            "agent_min_ns": bulk["agent_min_ns"].astype(np.int64),
            "agent_max_ns": bulk["agent_max_ns"].astype(np.int64),
            "agent_sum_ns": bulk["agent_sum_ns"].astype(np.int64),
            "agent_sum_sq_hi": bulk["agent_sum_sq_hi"].astype(np.int64),
            "agent_sum_sq_lo": bulk["agent_sum_sq_lo"].astype(np.int64),
            "gc_pause_ns": bulk["gc_pause_ns"].astype(np.int64),
            "source_file": source_file,
        }).with_columns(pl.col("label").str.starts_with("ready_wait_").alias("diag"))
        return frame


    def agent_sum_sq_ns(frame: pl.DataFrame) -> list[int]:
        """Combine the Int128 hi/lo columns into Python ints on demand."""
        hi = frame.get_column("agent_sum_sq_hi").to_list()
        lo = frame.get_column("agent_sum_sq_lo").to_list()
        return [
            (int(h) << 64) + (int(l) & ((1 << 64) - 1))
            for h, l in zip(hi, lo)
        ]


    def load_instrumentation_hdf5(filename: str | Path) -> pl.DataFrame:
        """Load a Vahana instrumentation trace directly from HDF5.

        Accepts a concrete ``.h5`` file, a base filename (with or without
        ``.h5``), or a base filename whose data is stored in ``_<rank>.h5``
        files. Returns one normalized row per event, including the MPI rank
        and readable enum names.
        """
        parts: list[pl.DataFrame] = []
        file_metadata: list[dict[str, Any]] = []
        for path, _rank_from_filename in _resolve_files(filename):
            with h5py.File(path, "r") as h5:
                group = h5["instrumentation"]
                dataset = group["events"]
                fields = set(dataset.dtype.names or ())
                missing = _REQUIRED_FIELDS - fields
                if missing:
                    raise ValueError(
                        f"{path} has an unsupported instrumentation event schema; "
                        f"missing fields: {sorted(missing)}"
                    )

                label_names = _read_enum_names(group, "event_labels")
                context_names = _read_enum_names(group, "contexts")
                kind_names = _read_enum_names(group, "kinds")
                file_metadata.append({
                    "path": str(path),
                    "mpi_size": int(group.attrs.get("mpi_size", 1)),
                    "n_events": int(len(dataset)),
                })

                total = len(dataset)
                for start in range(0, total, _CHUNK_SIZE):
                    bulk = dataset[start : start + _CHUNK_SIZE][...]
                    parts.append(
                        _decode_chunk(bulk, label_names, context_names, kind_names, str(path))
                    )

        if not parts:
            raise ValueError(f"No instrumentation events were found in {filename!s}.")
        frame = pl.concat(parts)
        frame = frame.with_columns((pl.col("dur_ns") / 1_000_000.0).alias("duration_ms"))
        frame = frame.sort(
            ["transition_nr", "rank", "start_ns", "dur_ns"]
        )
        return frame


    return (load_instrumentation_hdf5,)


@app.cell
def title(mo):
    mo.md("""
    # Vahana Instrumentation Insights
    """)
    return


@app.cell(hide_code=True)
def _(mo):
    mo.md(r"""
    ## Select the h5 folder by clicking on the folder symbol
    """)
    return


@app.cell(hide_code=True)
def path_input(Path, mo):
    _default_dir = Path(__file__).resolve().parents[2] / "h5"
    root_dir = mo.ui.file_browser(
        initial_path=_default_dir,
        selection_mode="directory",
        restrict_navigation=False,
        multiple=False,
        value=[_default_dir],
        label="HDF5 root directory",
    )
    root_dir
    return (root_dir,)


@app.cell(hide_code=True)
def file_choice(Path, h5py, mo, root_dir):
    def _has_instrumentation(path: Path) -> bool:
        try:
            with h5py.File(path, "r") as f:
                return "instrumentation/events" in f
        except Exception:
            return False


    trace_root = root_dir.path(index=0)
    _h5_files = sorted(
        p.name for p in trace_root.glob("*.h5") if _has_instrumentation(p)
    )
    _default_trace = 'c1_EqualAgentNumbers_mpisize_10.h5' if 'c1_EqualAgentNumbers_mpisize_10.h5' in _h5_files else ("InstrumentationExample.h5" if "InstrumentationExample.h5" in _h5_files else (_h5_files[0] if _h5_files else ""))
    file_choice = mo.ui.dropdown(options=_h5_files, value=_default_trace, label="HDF5 trace 1")
    file_choice_2 = mo.ui.dropdown(options=_h5_files, value=_default_trace, label="HDF5 trace 2")


    return file_choice, file_choice_2, trace_root


@app.cell
def trace_controls(active_run_button, file_choice, file_choice_2, mo):
    mo.hstack(
        [file_choice, file_choice_2, active_run_button],
        justify="start", align="end", gap=1, wrap=True,
    )

    return


@app.cell(hide_code=True)
def _(events, mo, pl):
    if events.filter(pl.col("step") == -1).height:
        mo.callout(
            mo.md(
                "**Missing step information.** This trace contains events without "
                "a model step counter (`step = -1`). Some analysis tools will "
                "not work correctly without step information."
            ),
            kind="warn",
        )
    else:
        mo.md("")
    return


@app.cell(hide_code=True)
def filter_md(mo):
    mo.md("""
    ### Transition selection
    Select one or more table rows to filter both views; use the header checkbox to
    select or clear all rows. Timing and GC metrics refer to the active run and
    selected phase. Durations sum that phase per call and rank, then use the maximum
    across ranks; Calls counts calls where the phase was recorded. GC share is total
    phase GC time divided by total rank-local phase duration across calls and ranks.

    **Apply sent/received** sums instrumented bytes across the entire apply call,
    not just the selected phase; these are recorded volumes, not necessarily
    physical network traffic. A dash means no corresponding data in the active run.
    The step filter affects only View 2, not these table metrics.
    """)
    return


@app.cell
def transition_table_display(transition_table):
    transition_table
    return


@app.cell
def selected_transitions(transition_summary, transition_table):
    selected_transitions = transition_table.value.get_column("Transition").to_list()
    transition_selection_label = (
        "All transitions" if len(selected_transitions) == transition_summary.height
        else (", ".join(selected_transitions) if selected_transitions else "No transitions")
    )

    return selected_transitions, transition_selection_label


@app.cell
def _(mo, phase_choice, step_filter):
    mo.hstack([phase_choice, step_filter], justify="start", align="end", gap=1)
    return


@app.cell
def active_run_button(mo):
    active_run_button = mo.ui.switch(
        value=False,
        label="Use trace 2",
    )

    return (active_run_button,)


@app.cell
def active_run(
    active_run_button,
    events_1,
    events_2,
    file_choice,
    file_choice_2,
    mo,
):
    active_run_name = file_choice_2.value if active_run_button.value else file_choice.value
    events = events_2 if active_run_button.value else events_1
    mo.md(f"**Active run for View 2:** {'2' if active_run_button.value else '1'} — `{active_run_name}`")

    return active_run_name, events


@app.cell(hide_code=True)
def load(file_choice, load_instrumentation_hdf5, trace_root):
    if not file_choice.value:
        raise ValueError(f"No .h5 file in {trace_root}; pick a directory that contains HDF5 traces.")
    events_1 = load_instrumentation_hdf5(trace_root / file_choice.value)

    return (events_1,)


@app.cell
def load_trace_2(file_choice_2, load_instrumentation_hdf5, trace_root):
    if not file_choice_2.value:
        raise ValueError(f"No second .h5 file in {trace_root}.")
    events_2 = load_instrumentation_hdf5(trace_root / file_choice_2.value)

    return (events_2,)


@app.cell(hide_code=True)
def view1_agg(events_1, events_2, pl):
    def _aggregate_apply_by_transition(events):
        return (
            events.filter(pl.col("label") == "apply_total")
            .group_by("transition_nr", "step", "caller")
            .agg(pl.col("duration_ms").max().alias("duration_ms"))
        )
    apply_by_transition_1 = _aggregate_apply_by_transition(events_1)
    apply_by_transition_2 = _aggregate_apply_by_transition(events_2)

    return apply_by_transition_1, apply_by_transition_2


@app.cell(hide_code=True)
def view1_plot(
    apply_by_transition_1,
    apply_by_transition_2,
    file_choice,
    file_choice_2,
    go,
    pl,
    selected_transitions,
    step_duration_mode,
    transition_selection_label,
):
    _fig = go.Figure()
    for _name, _data, _color in [
        (f"Run 1: {file_choice.value}", apply_by_transition_1, "#4c78a8"),
        (f"Run 2: {file_choice_2.value}", apply_by_transition_2, "#f58518"),
    ]:
        _data = _data.filter(pl.col("caller").is_in(selected_transitions))
        _steps = _data.group_by("step").agg(pl.col("duration_ms").sum()).sort("step")
        if step_duration_mode.value == "Cumulative":
            _steps = _steps.with_columns(pl.col("duration_ms").cum_sum())
        _fig.add_scatter(
            x=_steps.get_column("step").to_list(),
            y=_steps.get_column("duration_ms").to_list(),
            mode="lines+markers", name=_name, line_color=_color,
        )
    _fig.update_layout(
        title=f"Apply duration per step — {transition_selection_label} — {step_duration_mode.value}",
        xaxis_title="Step",
        yaxis_title="Cumulative duration (ms)" if step_duration_mode.value == "Cumulative" else "Duration (ms)",
    )
    fig_apply_total_duration = _fig

    return (fig_apply_total_duration,)


@app.cell(hide_code=True)
def transition_filter_cell(
    events,
    events_1,
    events_2,
    mo,
    phase_choice,
    phase_durations,
    pl,
):
    _names = (
        pl.concat([
            events_1.filter(pl.col("label") == "transition").select("caller"),
            events_2.filter(pl.col("label") == "transition").select("caller"),
        ]).unique().sort("caller")
    )
    _callers = (
        events.filter(pl.col("label") == "apply_total")
        .select("transition_nr", "caller").unique()
    )
    _phase_per_rank = (
        phase_durations.filter(pl.col("phase") == phase_choice.value)
        .group_by("transition_nr", "rank")
        .agg(pl.col("duration_ms").sum(), pl.col("gc_ms").sum())
        .join(_callers, on="transition_nr", how="inner")
    )
    _calls = _phase_per_rank.group_by("caller", "transition_nr").agg(
        pl.col("duration_ms").max().alias("duration_ms")
    )
    _timing = _calls.group_by("caller").agg(
        pl.len().alias("Calls"),
        pl.col("duration_ms").mean().round(2).alias("Mean phase (ms)"),
        pl.col("duration_ms").max().round(2).alias("Max phase (ms)"),
    )
    _gc = _phase_per_rank.group_by("caller").agg(
        pl.when(pl.col("duration_ms").sum() > 0)
        .then(100.0 * pl.col("gc_ms").sum() / pl.col("duration_ms").sum())
        .otherwise(None)
        .round(2)
        .alias("GC share of phase (%)")
    )
    _communication = (
        events.filter(pl.col("context") == "apply")
        .group_by("caller").agg(
            (pl.col("send_bytes").sum() / 1_048_576).round(2).alias("Apply sent (MiB)"),
            (pl.col("recv_bytes").sum() / 1_048_576).round(2).alias("Apply received (MiB)"),
        )
    )
    transition_summary = (
        _names.join(_timing, on="caller", how="left")
        .join(_communication, on="caller", how="left")
        .join(_gc, on="caller", how="left")
        .rename({"caller": "Transition"})
    )
    transition_table = mo.ui.table(
        transition_summary,
        selection="multi",
        initial_selection=list(range(transition_summary.height)),
        pagination=False,
        show_data_types=False,
        label=f"Transitions — active run, phase: {phase_choice.value}",
    )

    return transition_summary, transition_table


@app.cell
def step_filter(events, mo, pl):
    _step_options = {"All steps": None}
    _step_options.update({str(s): s for s in events.filter(pl.col("label") == "transition").get_column("step").unique().sort().to_list()})
    step_filter = mo.ui.dropdown(options=_step_options, value="All steps", label="Step (View 2)")

    return (step_filter,)


@app.cell(hide_code=True)
def phase_selected(
    events,
    phase_choice,
    phase_durations,
    pl,
    selected_transitions,
    step_filter,
):
    _caller_nrs = (
        events.filter(pl.col("label") == "transition")
        .select("transition_nr", "caller")
        .unique()
    )
    _nrs = _caller_nrs.filter(
        pl.col("caller").is_in(selected_transitions)
    ).get_column("transition_nr").to_list()

    phase_selected = phase_durations.filter(
        pl.col("phase") == phase_choice.value
    )
    phase_selected = phase_selected.filter(pl.col("transition_nr").is_in(_nrs))
    phase_selected = phase_selected.join(
        events.select("transition_nr", "step").unique(),
        on="transition_nr",
        how="left",
    )
    if step_filter.value is not None:
        phase_selected = phase_selected.filter(pl.col("step") == step_filter.value)

    return (phase_selected,)


@app.cell(hide_code=True)
def phase_choice(mo):
    _PHASES = (
        "prepare_read",
        "prepare_write",
        "prepare_spatial",
        "transition",
        "edges_remove",
        "edges_add",
        "finish_read",
        "finish_write",
        "finish_spatial",
    )
    phase_choice = mo.ui.dropdown(
        options=list(_PHASES) + ["pre_transition", "post_transition"],
        value="transition",
        label="Phase",
    )
    return (phase_choice,)


@app.cell(hide_code=True)
def phase_durations(events, np, pl):
    _COLLECTIVE_KINDS = {"alltoall", "alltoallv", "allreduce", "win_fence"}

    # Real barriers are excluded by label; `prepare_spatial` also carries
    # kind = :barrier but is a measurable phase and must stay visible.
    _app = events.filter(
        (pl.col("context") == "apply")
        & (pl.col("label") != "apply_total")
        & ~pl.col("label").str.starts_with("barrier")
    ).with_columns(
        (pl.col("start_ns") + pl.col("dur_ns")).alias("_end_ns"),
        (pl.col("gc_pause_ns") / 1_000_000.0).alias("gc_ms"),
        pl.col("kind").is_in(_COLLECTIVE_KINDS).alias("_is_coll"),
    )

    # An event is a direct child of apply_total if no other apply-context event
    # (same rank/transition) starts before it and ends after it.
    _app = _app.sort(
        ["transition_nr", "rank", "start_ns", "dur_ns"],
        descending=[False, False, False, True],
    )
    _app = _app.with_columns(
        pl.col("_end_ns").shift(1).over(["transition_nr", "rank"]).alias("_prev_end")
    )
    _app = _app.with_columns(
        pl.col("_prev_end").cum_max().over(["transition_nr", "rank"]).alias("_max_end_before")
    )
    _app = _app.with_columns(
        (pl.col("_end_ns") <= pl.col("_max_end_before").fill_null(-1)).alias("_is_nested")
    )

    # Index of the enclosing top-level phase, used to order local work relative
    # to the transition loop.
    _app = _app.with_columns(
        pl.when(~pl.col("_is_nested")).then(pl.int_range(0, pl.len())).alias("_top_idx")
    )
    _app = _app.with_columns(
        pl.col("_top_idx").forward_fill().over(["transition_nr", "rank"]).alias("_top_idx")
    )

    # Group id per (transition, rank) block; the frame is sorted by those keys.
    _app = _app.with_columns(
        (
            (pl.col("transition_nr").shift(1) != pl.col("transition_nr"))
            | (pl.col("rank").shift(1) != pl.col("rank"))
        ).fill_null(True).cum_sum().alias("_grp")
    )
    _app = _app.with_columns(
        pl.col("start_ns").min().over(["transition_nr", "rank"]).alias("_min_start")
    )

    # A phase is synchronizing when it is itself collective or wraps a collective
    # sub-event: spatial_collect wraps alltoallv joins, finish_write wraps
    # win_fence windows, prepare_spatial wraps the whole spatial preparation.
    # Such durations are bounded by the slowest rank and hide local imbalance.
    # Counting the collective starts inside each event's interval keeps this
    # per event, so local work nested in a synchronizing wrapper stays visible.
    _span = int(
        _app.select((pl.col("_end_ns") - pl.col("_min_start")).max()).item() + 1
    )
    _app = _app.with_columns(
        (pl.col("start_ns") - pl.col("_min_start") + pl.col("_grp") * _span)
        .cast(pl.Int64).alias("_norm_start"),
        (pl.col("_end_ns") - pl.col("_min_start") + pl.col("_grp") * _span)
        .cast(pl.Int64).alias("_norm_end"),
    )
    _coll_starts = np.sort(
        _app.filter(pl.col("_is_coll")).get_column("_norm_start").to_numpy()
    )
    _inside = (
        np.searchsorted(_coll_starts, _app.get_column("_norm_end").to_numpy(), side="right")
        - np.searchsorted(_coll_starts, _app.get_column("_norm_start").to_numpy(), side="right")
    )
    _app = _app.with_columns(pl.Series("_inside", _inside))
    _app = _app.with_columns(
        (pl.col("_is_coll") | (pl.col("_inside") > 0)).alias("_synchronizing"),
    )

    _ind = (
        _app.group_by(["transition_nr", "rank", "label"], maintain_order=True)
        .agg(
            pl.col("duration_ms").sum().alias("duration_ms"),
            pl.col("gc_ms").sum().alias("gc_ms"),
            pl.col("_synchronizing").max().alias("synchronizing"),
        )
        .rename({"label": "phase"})
        .select(["transition_nr", "rank", "duration_ms", "gc_ms", "phase", "synchronizing"])
    )

    # pre/post_transition: the local (non-synchronizing) work before / after the
    # transition loop inside apply!. Ordering is used instead of timestamps,
    # because barriers between the phases synchronize the ranks. Synchronizing
    # phases are replaced by the local phases inside them: only the outermost
    # local events are summed, so a wrapper like prepare_spatial does not swallow
    # the local work inside it and nothing is counted twice.
    _top = _app.filter(~pl.col("_is_nested")).with_columns(
        pl.col("transition_nr").cum_count().over(["transition_nr", "rank"]).alias("_pos")
    )
    _t_pos = (
        _top.filter(pl.col("label") == "transition")
        .group_by(["transition_nr", "rank"])
        .agg(
            pl.col("_pos").min().alias("first_t"),
            pl.col("_pos").max().alias("last_t"),
        )
    )

    _local = _app.filter(~pl.col("_synchronizing"))
    _local = _local.join(
        _top.select("_top_idx", "_pos"),
        on="_top_idx",
        how="left",
    )
    _local = _local.with_columns(pl.col("_pos").alias("_block_pos"))
    _local = _local.join(_t_pos, on=["transition_nr", "rank"], how="left")

    # keep only the outermost local events (a local event wrapping another local
    # event would otherwise be counted twice)
    _local = _local.sort(
        ["transition_nr", "rank", "start_ns", "dur_ns"],
        descending=[False, False, False, True],
    )
    _local = _local.with_columns(
        pl.col("_end_ns").shift(1).over(["transition_nr", "rank"]).alias("_prev_end")
    )
    _local = _local.with_columns(
        pl.col("_prev_end").cum_max().over(["transition_nr", "rank"]).alias("_max_end_before")
    )
    _local = _local.filter(pl.col("_end_ns") > pl.col("_max_end_before").fill_null(-1))

    _pre = (
        _local.filter(pl.col("_block_pos") < pl.col("first_t"))
        .group_by(["transition_nr", "rank"])
        .agg(
            pl.col("duration_ms").sum().alias("duration_ms"),
            pl.col("gc_ms").sum().alias("gc_ms"),
        )
        .with_columns(
            pl.lit("pre_transition").alias("phase"),
            pl.lit(False).alias("synchronizing"),
        )
    )
    _post = (
        _local.filter(pl.col("_block_pos") > pl.col("last_t"))
        .group_by(["transition_nr", "rank"])
        .agg(
            pl.col("duration_ms").sum().alias("duration_ms"),
            pl.col("gc_ms").sum().alias("gc_ms"),
        )
        .with_columns(
            pl.lit("post_transition").alias("phase"),
            pl.lit(False).alias("synchronizing"),
        )
    )

    phase_durations = pl.concat(
        [_ind,
         _pre.select(["transition_nr", "rank", "duration_ms", "gc_ms", "phase", "synchronizing"]),
         _post.select(["transition_nr", "rank", "duration_ms", "gc_ms", "phase", "synchronizing"])],
    )
    return (phase_durations,)


@app.cell(hide_code=True)
def view2_agg(phase_selected, pl):
    phase_per_rank = (
        phase_selected.group_by("rank", maintain_order=True)
        .agg(
            pl.col("duration_ms").sum().alias("total_ms"),
            pl.col("gc_ms").sum().alias("gc_ms"),
            pl.col("duration_ms").mean().alias("mean_ms"),
            pl.len().alias("n_events"),
        )
        .with_columns(
            pl.when(pl.col("total_ms") == 0)
            .then(pl.lit(None, dtype=pl.Float64))
            .otherwise(100 * pl.col("gc_ms") / pl.col("total_ms"))
            .alias("gc_share_pct")
        )
        .sort("rank")
    )

    phase_by_transition = (
        phase_selected.group_by(["step", "transition_nr"], maintain_order=True)
        .agg(
            pl.col("duration_ms").min().alias("min_ms"),
            pl.col("duration_ms").median().alias("median_ms"),
            pl.col("duration_ms").max().alias("max_ms"),
            pl.col("duration_ms").std().alias("std_ms"),
            pl.col("gc_ms").median().alias("gc_median_ms"),
            pl.col("gc_ms").max().alias("gc_max_ms"),
        )
        .with_columns(
            pl.when(pl.col("median_ms") == 0)
            .then(pl.lit(None, dtype=pl.Float64))
            .otherwise(pl.col("max_ms") / pl.col("median_ms"))
            .alias("imbalance_ratio"),
            pl.when(pl.col("median_ms") == 0)
            .then(pl.lit(None, dtype=pl.Float64))
            .otherwise(100 * pl.col("gc_median_ms") / pl.col("median_ms"))
            .alias("gc_share_pct"),
        )
        .sort(["step", "transition_nr"])
    )
    phase_by_transition = phase_by_transition.with_columns(
        pl.col("std_ms").fill_null(0.0)
    )
    return


@app.cell(hide_code=True)
def view2_plot(
    active_run_name,
    events,
    go,
    phase_choice,
    phase_selected,
    pl,
    px,
    selected_transitions,
    step_filter,
    transition_selection_label,
):
    _step_selected = step_filter.value is not None
    _x_is_step = not _step_selected and len(selected_transitions) == 1
    _xcol = "step" if _x_is_step else "transition_nr"

    _by_step_rank = (
        phase_selected.group_by([_xcol, "rank"], maintain_order=True)
        .agg(
            pl.col("duration_ms").sum().alias("total_ms"),
            pl.col("gc_ms").sum().alias("gc_ms"),
        )
        .with_columns(
            (pl.col("total_ms") - pl.col("gc_ms")).clip(lower_bound=0.0).alias("compute_ms")
        )
        .sort([_xcol, "rank"])
    )

    _colors = px.colors.qualitative.Plotly
    _ranks = phase_selected.get_column("rank").unique().sort().to_list()

    fig_phase = go.Figure()
    for _i, _rank in enumerate(_ranks):
        _color = _colors[_i % len(_colors)]
        _sub = _by_step_rank.filter(pl.col("rank") == _rank)
        # same color per rank; compute is solid, GC is hatched on top
        fig_phase.add_bar(
            x=_sub.get_column(_xcol).cast(pl.String).to_list(),
            y=_sub.get_column("compute_ms").to_list(),
            name=f"Rank {_rank}",
            legendgroup=f"rank{_rank}",
            offsetgroup=f"rank{_rank}",
            marker_color=_color,
            showlegend=True,
        )
        fig_phase.add_bar(
            x=_sub.get_column(_xcol).cast(pl.String).to_list(),
            y=_sub.get_column("gc_ms").to_list(),
            name=f"Rank {_rank} · GC",
            legendgroup=f"rank{_rank}",
            offsetgroup=f"rank{_rank}",
            marker_color=_color,
            marker_pattern_shape="/",
            showlegend=False,
        )
    if _step_selected:
        _transition_names = (
            events.filter((pl.col("label") == "transition") & (pl.col("step") == step_filter.value))
            .select("transition_nr", "caller").unique()
            .join(phase_selected.select("transition_nr").unique(), on="transition_nr", how="semi")
            .sort("transition_nr")
        )
        fig_phase.update_xaxes(
            tickmode="array",
            tickvals=_transition_names.get_column("transition_nr").cast(pl.String).to_list(),
            ticktext=_transition_names.get_column("caller").to_list(),
            tickangle=-45,
        )
    fig_phase.update_layout(
        barmode="stack",
        title=f"{phase_choice.value} per {'step' if _x_is_step else 'transition'} and rank"
              f" ({transition_selection_label}; {f'step {step_filter.value}; ' if _step_selected else ''}{active_run_name}; hatched = GC, barriers excluded)",
        xaxis_title="Step" if _x_is_step else ("Transition name" if _step_selected else "Transition"),
        yaxis_title="Duration (ms)",
    )

    _nothing = 1
    return (fig_phase,)


@app.cell(column=1, hide_code=True)
def view1_md(mo):
    mo.md("""
    ## View 1: Apply duration over steps — run comparison
    Each line represents one selected HDF5 trace. The **transition table**
    selects which `apply!` calls contribute. For each call, the maximum
    `apply_total` duration across ranks is counted once; selected calls are then
    summed per step. Choose **Per step** or **Cumulative** for the running sum.
    These are instrumented apply durations, not necessarily wall-clock durations.
    """)
    return


@app.cell(hide_code=True)
def step_duration_mode(mo):
    step_duration_mode = mo.ui.dropdown(
        options=["Per step", "Cumulative"],
        value="Per step",
        label="Duration display",
    )
    step_duration_mode

    return (step_duration_mode,)


@app.cell
def _(fig_apply_total_duration):
    fig_apply_total_duration
    return


@app.cell(hide_code=True)
def view2_md(mo):
    mo.md("""
    ## View 2: Phase comparison across ranks

    Duration of the selected phase per rank for the active run. Use the switch above to select trace 1 or trace 2. The step filter applies only to this view; when selected, the x-axis shows transition names. Real barriers are excluded.

    `pre_transition` / `post_transition` sum the **local** work before / after
    the transition loop inside `apply_total`, using event order. Synchronizing
    phases are replaced by the local phases inside them: a phase counts as
    synchronizing if it is itself a collective (`alltoall`, `alltoallv`,
    `allreduce`, `win_fence`) or wraps one — `prepare_spatial`, `spatial_collect`
    and `finish_write` all do. Their durations are bounded by the slowest rank
    and would hide local imbalance.

    Select `prepare_spatial` to see the synchronizing total instead, and
    `spatial_kdtree` for the local work inside it.
    """)
    return


@app.cell
def _(fig_phase):
    fig_phase
    return


if __name__ == "__main__":
    app.run()
