"""Aggregation and figure helpers shared by dashboard pages."""
from __future__ import annotations
import numpy as np
import pandas as pd
import plotly.graph_objects as go

PRE_PHASES = ["prepare_read", "prepare_write", "prepare_spatial"]
POST_PHASES = ["edges_remove", "edges_add", "finish_read", "finish_write", "finish_spatial"]
PHASES = PRE_PHASES + ["transition"] + POST_PHASES
COLLECTIVES = {
    "alltoall", "alltoallv", "allreduce", "win_fence", "allgather", "allgatherv",
}


def phases(events: pd.DataFrame) -> pd.DataFrame:
    """Reproduce notebook phase aggregates; exclude barriers and avoid nested local double counts."""
    app = events[(events.context == "apply") & (events.label != "apply_total") & ~events.label.str.startswith("barrier")].copy()
    columns = ["transition_nr", "rank", "phase", "duration_ms", "gc_ms"]
    if app.empty:
        return pd.DataFrame(columns=columns)
    app["gc_ms"] = app.gc_pause_ns / 1e6
    individual = app.groupby(["transition_nr", "rank", "label"], sort=False, as_index=False).agg(duration_ms=("duration_ms", "sum"), gc_ms=("gc_ms", lambda s: s.sum(min_count=1))).rename(columns={"label": "phase"})
    extra = []
    for (nr, rank), group in app.groupby(["transition_nr", "rank"], sort=False):
        group = group.sort_values(["start_ns", "dur_ns"], ascending=[True, False])
        starts = np.sort(group.loc[group.kind.isin(COLLECTIVES), "start_ns"].to_numpy())
        max_end = -1
        top_pos = -1
        rows = []
        for row in group.itertuples(index=False):
            end = int(row.start_ns) + int(row.dur_ns)
            nested = end <= max_end
            if not nested:
                top_pos += 1
            sync = row.kind in COLLECTIVES or np.searchsorted(starts, end, side="right") > np.searchsorted(starts, row.start_ns, side="right")
            rows.append((row, end, nested, top_pos, sync))
            max_end = max(max_end, end)
        transition_positions = [pos for row, _, nested, pos, _ in rows if not nested and row.label == "transition"]
        if not transition_positions:
            continue
        max_local_end = -1
        selected = {"pre_transition": [], "post_transition": []}
        for row, end, _, pos, sync in rows:
            if sync:
                continue
            outer = end > max_local_end
            max_local_end = max(max_local_end, end)
            if not outer:
                continue
            name = "pre_transition" if pos < min(transition_positions) else "post_transition" if pos > max(transition_positions) else None
            if name:
                selected[name].append(row)
        for name, local in selected.items():
            if local:
                gc_values = [r.gc_pause_ns for r in local]
                extra.append({"transition_nr": nr, "rank": rank, "phase": name,
                              "duration_ms": sum(r.duration_ms for r in local),
                              "gc_ms": sum(gc_values) / 1e6 if all(pd.notna(x) for x in gc_values) else np.nan})
    return pd.concat([individual[columns], pd.DataFrame(extra, columns=columns)], ignore_index=True)


def transition_summary(events: pd.DataFrame, all_names: list[str], phase: str | list[str], phase_data: pd.DataFrame) -> pd.DataFrame:
    selected = [phase] if isinstance(phase, str) else phase
    metadata = events.loc[events.label == "apply_total", ["transition_nr", "caller"]].drop_duplicates()
    ranked = phase_data[phase_data.phase.isin(selected)].groupby(["transition_nr", "rank"], as_index=False).agg(duration_ms=("duration_ms", "sum"), gc_ms=("gc_ms", lambda s: s.sum(min_count=1))).merge(metadata, on="transition_nr", how="inner")
    calls = ranked.groupby(["caller", "transition_nr"], as_index=False).duration_ms.max()
    timing = calls.groupby("caller").duration_ms.agg(Calls="count", **{"Mean phase (ms)": "mean", "Max phase (ms)": "max"})
    gc = ranked.groupby("caller").agg(duration=("duration_ms", "sum"), gc=("gc_ms", lambda s: s.sum(min_count=1)))
    gc["GC share (%)"] = 100 * gc.gc / gc.duration.replace(0, np.nan)
    comm = events[events.context == "apply"].groupby("caller")[["send_bytes", "recv_bytes"]].sum() / (1024 ** 2)
    comm.columns = ["Apply sent (MiB)", "Apply received (MiB)"]
    apply_calls = events.loc[events.label == "apply_total", ["transition_nr", "rank", "caller"]].drop_duplicates()
    barrier_events = events[(events.context == "apply") & events.label.str.startswith("barrier")]
    barriers = barrier_events.groupby(["transition_nr", "rank"], as_index=False).duration_ms.sum()
    per_rank = apply_calls.merge(barriers, on=["transition_nr", "rank"], how="left")
    per_rank["duration_ms"] = per_rank.duration_ms.fillna(0)
    barrier_mean = per_rank.groupby("caller").duration_ms.mean().rename("Mean apply barrier (ms)")
    result = pd.DataFrame({"Transition": all_names}).set_index("Transition").join(timing).join(gc[["GC share (%)"]]).join(comm).join(barrier_mean).reset_index()
    return result.round(2).replace({np.nan: None})


def selected_summary(events: pd.DataFrame, names: list[str], selected_phases: list[str], phase_data: pd.DataFrame) -> dict | None:
    """Recompute aggregates from calls, rather than summing averages or percentages."""
    if not names or not selected_phases:
        return None
    calls = events.loc[(events.label == "apply_total") & events.caller.isin(names), "transition_nr"].unique()
    if not len(calls):
        return None
    selected_events = events[events.transition_nr.isin(calls)].copy()
    selected_events["caller"] = "Selected total"
    summary = transition_summary(selected_events, ["Selected total"], selected_phases, phase_data)
    return summary.iloc[0].to_dict()


def transition_totals(events: pd.DataFrame, names: list[str], selected_phases: list[str], phase_data: pd.DataFrame) -> dict | None:
    """Sum displayed per-transition phase times; retain weighted GC and byte totals."""
    total = selected_summary(events, names, selected_phases, phase_data)
    if total is None:
        return None
    rows = transition_summary(events, names, selected_phases, phase_data)
    for column in ("Mean phase (ms)", "Max phase (ms)", "Mean apply barrier (ms)"):
        values = pd.to_numeric(rows[column], errors="coerce")
        total[column] = round(float(values.sum()), 2) if values.notna().any() else None
    return total


def phase_summary(events: pd.DataFrame, phase_data: pd.DataFrame) -> pd.DataFrame:
    names = events.loc[events.label == "apply_total", "caller"].drop_duplicates().tolist()
    rows = []
    for phase in PHASES:
        summary = selected_summary(events, names, [phase], phase_data) or {}
        rows.append({"Phase": phase, **{key: summary.get(key) for key in
                     ("Calls", "Mean phase (ms)", "Max phase (ms)", "GC share (%)")}})
    return pd.DataFrame(rows)


def step_figure(runs: list[tuple[str, pd.DataFrame]], names: list[str], cumulative: bool,
                selected_phases: list[str] | None = None,
                phase_data: list[pd.DataFrame] | None = None) -> go.Figure:
    """Compare apply totals, or max-rank sums of selected phases, by step."""
    if selected_phases is not None and (phase_data is None or len(phase_data) != len(runs)):
        raise ValueError("Phase data is required for each trace when filtering phases")
    fig = go.Figure()
    for index, (label, events) in enumerate(runs):
        rows = events[(events.label == "apply_total") & events.caller.isin(names)]
        if selected_phases is None:
            calls = rows.groupby(["transition_nr", "step"], as_index=False).duration_ms.max()
        else:
            metadata = rows[["transition_nr", "step"]].drop_duplicates()
            selected = phase_data[index]
            selected = selected[selected.phase.isin(selected_phases)]
            by_rank = selected.groupby(["transition_nr", "rank"], as_index=False).duration_ms.sum()
            calls = by_rank.groupby("transition_nr", as_index=False).duration_ms.max().merge(metadata, on="transition_nr", how="inner")
        steps = calls.groupby("step", as_index=False).duration_ms.sum().sort_values("step")
        if cumulative:
            steps["duration_ms"] = steps.duration_ms.cumsum()
        fig.add_scatter(x=steps.step, y=steps.duration_ms, mode="lines+markers", name=label)
    title = ", ".join(names) if names else "No transitions"
    metric = "Apply duration" if selected_phases is None else "Selected phase duration"
    phase_label = "" if selected_phases is None else f" — {', '.join(selected_phases) if selected_phases else 'No phases'}"
    fig.update_layout(title=f"{metric} by step — {title}{phase_label} — {'Cumulative' if cumulative else 'Per step'}", xaxis_title="Step", yaxis_title="Cumulative duration (ms)" if cumulative else "Duration (ms)")
    return fig


def phase_figure(events: pd.DataFrame, phase_data: pd.DataFrame, names: list[str], phase: str | list[str], step: int | list[int] | None, active_name: str, ranks: list[int] | None = None) -> go.Figure:
    selected = [phase] if isinstance(phase, str) else phase
    meta = events.loc[events.label == "transition", ["transition_nr", "caller", "step"]].drop_duplicates()
    data = phase_data[phase_data.phase.isin(selected)].merge(meta, on="transition_nr", how="inner")
    if ranks is not None:
        data = data[data['rank'].isin(ranks)]
    data = data[data.caller.isin(names)]
    selected_steps = [step] if isinstance(step, int) else step
    if selected_steps is not None:
        data = data[data.step.isin(selected_steps)]
    xcol = "transition_nr" if selected_steps is not None or len(names) != 1 else "step"
    data = data.groupby([xcol, "rank"], as_index=False).agg(duration_ms=("duration_ms", "sum"), gc_ms=("gc_ms", lambda s: s.sum(min_count=1))).sort_values([xcol, "rank"])
    fig = go.Figure()
    for rank, sub in data.groupby("rank"):
        x = sub[xcol].astype(str)
        gc = sub.gc_ms.fillna(0)
        fig.add_bar(x=x, y=(sub.duration_ms-gc).clip(lower=0), name=f"Rank {rank}", legendgroup=str(rank), offsetgroup=str(rank))
        fig.add_bar(x=x, y=gc, name=f"Rank {rank} GC", legendgroup=str(rank), offsetgroup=str(rank), marker_pattern_shape="/", showlegend=False)
    if selected_steps is not None:
        ticks = meta[meta.step.isin(selected_steps) & meta.caller.isin(names)].sort_values("transition_nr").drop_duplicates("transition_nr")
        ticktext = ticks.caller if len(selected_steps) == 1 else ticks.step.astype(str) + " · " + ticks.caller
        fig.update_xaxes(tickmode="array", tickvals=ticks.transition_nr.astype(str), ticktext=ticktext, tickangle=-45)
    phase_label = ", ".join(selected) if selected else "No phases"
    fig.update_layout(barmode="stack", title=f"{phase_label} by {'step' if xcol == 'step' else 'transition'} and rank — {active_name}", xaxis_title="Step" if xcol == "step" else "Transition", yaxis_title="Duration (ms)")
    return fig
