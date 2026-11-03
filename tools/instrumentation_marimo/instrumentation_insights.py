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
app = marimo.App(width="medium")


@app.cell(hide_code=True)
def imports():
    import re
    from pathlib import Path
    from typing import Any

    import h5py
    import numpy as np
    import pandas as pd
    import plotly.express as px
    import plotly.graph_objects as go
    import marimo as mo

    import polars as pl


    return Any, Path, go, h5py, mo, np, pd, px, re


@app.cell(hide_code=True)
def reader(Any, Path, h5py, np, pd, re):
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
    ) -> pd.DataFrame:
        label_ids = bulk["label"].astype(np.int64)
        context_ids = bulk["context"].astype(np.int64)
        kind_ids = bulk["kind"].astype(np.int64)
        frame = pd.DataFrame({
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
        })
        frame["diag"] = frame["label"].str.startswith("ready_wait_")
        return frame


    def agent_sum_sq_ns(frame: pd.DataFrame) -> list[int]:
        """Combine the Int128 hi/lo columns into Python ints on demand."""
        return [
            (int(hi) << 64) + (int(lo) & ((1 << 64) - 1))
            for hi, lo in zip(frame["agent_sum_sq_hi"], frame["agent_sum_sq_lo"])
        ]


    def load_instrumentation_hdf5(filename: str | Path) -> pd.DataFrame:
        """Load a Vahana instrumentation trace directly from HDF5.

        Accepts a concrete ``.h5`` file, a base filename (with or without
        ``.h5``), or a base filename whose data is stored in ``_<rank>.h5``
        files. Returns one normalized row per event, including the MPI rank
        and readable enum names.
        """
        parts: list[pd.DataFrame] = []
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
        frame = pd.concat(parts, ignore_index=True)
        frame["duration_ms"] = frame["dur_ns"] / 1_000_000.0
        frame = frame.sort_values(
            ["transition_nr", "rank", "start_ns", "dur_ns"], ignore_index=True
        )
        frame.attrs["source_files"] = [m["path"] for m in file_metadata]
        frame.attrs["mpi_size"] = max(m["mpi_size"] for m in file_metadata)
        frame.attrs["n_events_per_file"] = [m["n_events"] for m in file_metadata]
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
    file_choice = mo.ui.dropdown(
        options=_h5_files,
        value="InstrumentationExample.h5" if "InstrumentationExample.h5" in _h5_files else (_h5_files[0] if _h5_files else ""),
        label="HDF5 trace",
    )
    file_choice
    return file_choice, trace_root


@app.cell(hide_code=True)
def load(file_choice, load_instrumentation_hdf5, trace_root):
    if not file_choice.value:
        raise ValueError(f"No .h5 file in {trace_root}; pick a directory that contains HDF5 traces.")

    events = load_instrumentation_hdf5(trace_root / file_choice.value)
    return (events,)


@app.cell
def view1_md(mo):
    mo.md("""
    ## View 1: `apply_total` duration per transition

    Median and maximum total duration across all ranks. Gaps between the
    sub-intervals are not automatically idle time - they may contain work that
    is not separately instrumented.
    """)
    return


@app.cell
def view1_agg(events):
    apply_total = events[events["label"] == "apply_total"]

    apply_by_transition = (
        apply_total.groupby("transition_nr", as_index=False)["duration_ms"]
        .agg(median_ms="median", max_ms="max", min_ms="min", ranks="size")
        .rename(columns={"transition_nr": "transition"})
        .sort_values("transition")
    )
    return (apply_by_transition,)


@app.cell
def view1_plot(apply_by_transition, go):
    fig = go.Figure()
    fig.add_bar(
        x=apply_by_transition["transition"].astype(str),
        y=apply_by_transition["median_ms"],
        name="Median (all ranks)",
        marker_color="#4c78a8",
    )
    fig.add_bar(
        x=apply_by_transition["transition"].astype(str),
        y=apply_by_transition["max_ms"],
        name="Max (all ranks)",
        marker_color="#f58518",
    )
    fig.update_layout(
        barmode="group",
        title="apply_total duration per transition",
        xaxis_title="Transition",
        yaxis_title="Duration (ms)",
    )
    fig


    return


@app.cell
def filter_md(mo):
    mo.md("""
    ### Transition name filter

    The dropdown selects one of the transition names (`caller` of the
    `transition` events) or `All steps`. With a name selected, the plot shows
    the steps on the x-axis; `All steps` shows every transition number.
    """)
    return


@app.cell
def transition_filter_cell(events, mo):
    _callers = (
        events.loc[events["label"] == "transition", ["transition_nr", "caller"]]
        .drop_duplicates()
        .sort_values("transition_nr")
        ["caller"]
        .unique()
        .tolist()
    )
    transition_filter = mo.ui.dropdown(
        options=_callers + ["All steps"],
        value="All steps",
        label="Transition name",
    )
    transition_filter
    return (transition_filter,)


@app.cell
def phase_selected(events, phase_choice, phase_durations, transition_filter):
    _caller_nrs = (
        events.loc[events["label"] == "transition", ["transition_nr", "caller"]]
        .drop_duplicates()
    )
    if transition_filter.value == "All steps":
        _nrs = None
    else:
        _nrs = _caller_nrs.loc[
            _caller_nrs["caller"] == transition_filter.value, "transition_nr"
        ].tolist()

    phase_selected = phase_durations[
        phase_durations["phase"] == phase_choice.value
    ]
    if _nrs is not None:
        phase_selected = phase_selected[
            phase_selected["transition_nr"].isin(_nrs)
        ]
    phase_selected = phase_selected.merge(
        events[["transition_nr", "step"]].drop_duplicates(),
        on="transition_nr",
        how="left",
    )

    return (phase_selected,)


@app.cell
def view2_md(mo):
    mo.md("""
    ## View 2: Phase comparison across ranks

    Duration of the selected phase per rank. Real barriers are excluded.

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
    phase_choice
    return (phase_choice,)


@app.cell(hide_code=True)
def phase_durations(events, np, pd):
    _COLLECTIVE_KINDS = {"alltoall", "alltoallv", "allreduce", "win_fence"}

    # Real barriers are excluded by label; `prepare_spatial` also carries
    # kind = :barrier but is a measurable phase and must stay visible.
    _app = events[
        (events["context"] == "apply")
        & (events["label"] != "apply_total")
        & (~events["label"].str.startswith("barrier"))
    ].copy()
    _app["_end_ns"] = _app["start_ns"] + _app["dur_ns"]
    _app["gc_ms"] = _app["gc_pause_ns"] / 1_000_000.0
    _app["_is_coll"] = _app["kind"].isin(_COLLECTIVE_KINDS)

    # An event is a direct child of apply_total if no other apply-context event
    # (same rank/transition) starts before it and ends after it.
    _app = _app.sort_values(
        ["transition_nr", "rank", "start_ns", "dur_ns"],
        ascending=[True, True, True, False],
    )
    _app["_max_end_before"] = _app.groupby(["transition_nr", "rank"])["_end_ns"].transform(
        lambda s: s.shift(1).cummax()
    )
    _app["_is_nested"] = _app["_end_ns"] <= _app["_max_end_before"].fillna(-1)

    # Index of the enclosing top-level phase, used to order local work relative
    # to the transition loop.
    _app["_top_idx"] = np.where(~_app["_is_nested"], np.arange(len(_app)), np.nan)
    _app["_top_idx"] = _app.groupby(["transition_nr", "rank"])["_top_idx"].ffill()

    # A phase is synchronizing when it is itself collective or wraps a collective
    # sub-event: spatial_collect wraps alltoallv joins, finish_write wraps
    # win_fence windows, prepare_spatial wraps the whole spatial preparation.
    # Such durations are bounded by the slowest rank and hide local imbalance.
    # Counting the collective starts inside each event's interval keeps this
    # per event, so local work nested in a synchronizing wrapper stays visible.
    _grp = _app.groupby(["transition_nr", "rank"], sort=False).ngroup()
    _min_start = _app.groupby(["transition_nr", "rank"])["start_ns"].transform("min")
    _span = (_app["_end_ns"] - _min_start).max() + 1
    _norm_start = (_app["start_ns"] - _min_start + _grp * _span).to_numpy()
    _norm_end = (_app["_end_ns"] - _min_start + _grp * _span).to_numpy()
    _coll_starts = np.sort(_norm_start[_app["_is_coll"].to_numpy()])
    _inside = (
        np.searchsorted(_coll_starts, _norm_end, side="right")
        - np.searchsorted(_coll_starts, _norm_start, side="right")
    )
    _app["_synchronizing"] = _app["_is_coll"].to_numpy() | (_inside > 0)

    _ind = (
        _app.groupby(["transition_nr", "rank", "label"], as_index=False)
        .agg(duration_ms=("duration_ms", "sum"),
             gc_ms=("gc_ms", "sum"),
             synchronizing=("_synchronizing", "max"))
        .rename(columns={"label": "phase"})
    )

    # pre/post_transition: the local (non-synchronizing) work before / after the
    # transition loop inside apply!. Ordering is used instead of timestamps,
    # because barriers between the phases synchronize the ranks. Synchronizing
    # phases are replaced by the local phases inside them: only the outermost
    # local events are summed, so a wrapper like prepare_spatial does not swallow
    # the local work inside it and nothing is counted twice.
    _top = _app[~_app["_is_nested"]].reset_index(drop=True)
    _top["_pos"] = _top.groupby(["transition_nr", "rank"]).cumcount()
    _t_pos = (
        _top[_top["label"] == "transition"]
        .groupby(["transition_nr", "rank"])["_pos"]
        .agg(first_t="min", last_t="max")
    )

    _local = _app[~_app["_synchronizing"]].copy()
    _local["_block_pos"] = _local["_top_idx"].map(_top.set_index("_top_idx")["_pos"])
    _local = _local.merge(_t_pos, on=["transition_nr", "rank"], how="left")

    # keep only the outermost local events (a local event wrapping another local
    # event would otherwise be counted twice)
    _local = _local.sort_values(
        ["transition_nr", "rank", "start_ns", "dur_ns"],
        ascending=[True, True, True, False],
    )
    _local["_max_end_before"] = _local.groupby(["transition_nr", "rank"])["_end_ns"].transform(
        lambda s: s.shift(1).cummax()
    )
    _local = _local[_local["_end_ns"] > _local["_max_end_before"].fillna(-1)]

    _pre = (
        _local[_local["_block_pos"] < _local["first_t"]]
        .groupby(["transition_nr", "rank"], as_index=False)
        .agg(duration_ms=("duration_ms", "sum"), gc_ms=("gc_ms", "sum"))
        .assign(phase="pre_transition", synchronizing=False)
    )
    _post = (
        _local[_local["_block_pos"] > _local["last_t"]]
        .groupby(["transition_nr", "rank"], as_index=False)
        .agg(duration_ms=("duration_ms", "sum"), gc_ms=("gc_ms", "sum"))
        .assign(phase="post_transition", synchronizing=False)
    )

    phase_durations = pd.concat(
        [_ind,
         _pre[["transition_nr", "rank", "duration_ms", "gc_ms", "phase", "synchronizing"]],
         _post[["transition_nr", "rank", "duration_ms", "gc_ms", "phase", "synchronizing"]]],
        ignore_index=True,
    )

    return (phase_durations,)


@app.cell(hide_code=True)
def view2_agg(np, phase_selected):
    phase_per_rank = (
        phase_selected.groupby("rank", as_index=False)
        .agg(total_ms=("duration_ms", "sum"),
             gc_ms=("gc_ms", "sum"),
             mean_ms=("duration_ms", "mean"),
             n_events=("duration_ms", "size"))
        .assign(gc_share_pct=lambda d: 100 * d["gc_ms"] / d["total_ms"].replace(0, np.nan))
        .sort_values("rank")
    )

    phase_by_transition = (
        phase_selected.groupby(["step", "transition_nr"], as_index=False)
        .agg(min_ms=("duration_ms", "min"),
             median_ms=("duration_ms", "median"),
             max_ms=("duration_ms", "max"),
             std_ms=("duration_ms", "std"),
             gc_median_ms=("gc_ms", "median"),
             gc_max_ms=("gc_ms", "max"))
        .assign(imbalance_ratio=lambda d: d["max_ms"] / d["median_ms"],
                gc_share_pct=lambda d: 100 * d["gc_median_ms"] / d["median_ms"].replace(0, np.nan))
        .sort_values(["step", "transition_nr"])
    )
    phase_by_transition["std_ms"] = phase_by_transition["std_ms"].fillna(0.0)

    return


@app.cell(hide_code=True)
def view2_plot(go, phase_choice, phase_selected, px, transition_filter):
    _x_is_step = transition_filter.value != "All steps"
    _xcol = "step" if _x_is_step else "transition_nr"

    _by_step_rank = (
        phase_selected.groupby([_xcol, "rank"], as_index=False)
        .agg(total_ms=("duration_ms", "sum"), gc_ms=("gc_ms", "sum"))
        .assign(compute_ms=lambda d: (d["total_ms"] - d["gc_ms"]).clip(lower=0.0))
        .sort_values([_xcol, "rank"])
    )

    _colors = px.colors.qualitative.Plotly
    _ranks = sorted(phase_selected["rank"].unique())

    phase_fig = go.Figure()
    for _i, _rank in enumerate(_ranks):
        _color = _colors[_i % len(_colors)]
        _sub = _by_step_rank[_by_step_rank["rank"] == _rank]
        # same color per rank; compute is solid, GC is hatched on top
        phase_fig.add_bar(
            x=_sub[_xcol].astype(str),
            y=_sub["compute_ms"],
            name=f"Rank {_rank}",
            legendgroup=f"rank{_rank}",
            offsetgroup=f"rank{_rank}",
            marker_color=_color,
            showlegend=True,
        )
        phase_fig.add_bar(
            x=_sub[_xcol].astype(str),
            y=_sub["gc_ms"],
            name=f"Rank {_rank} · GC",
            legendgroup=f"rank{_rank}",
            offsetgroup=f"rank{_rank}",
            marker_color=_color,
            marker_pattern_shape="/",
            showlegend=False,
        )
    phase_fig.update_layout(
        barmode="stack",
        title=f"{phase_choice.value} per {'step' if _x_is_step else 'transition'} and rank"
              f" ({transition_filter.value}; hatched = GC, barriers excluded)",
        xaxis_title="Step" if _x_is_step else "Transition",
        yaxis_title="Duration (ms)",
    )
    phase_fig
    return


if __name__ == "__main__":
    app.run()
