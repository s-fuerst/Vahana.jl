"""Filtered event data and rank-local timeline figures."""
from __future__ import annotations

import numpy as np
import pandas as pd
import plotly.graph_objects as go
from plotly.colors import qualitative

from analysis import PHASES


def filter_events(events: pd.DataFrame, names: list[str], selected_phases: list[str],
                  ranks: list[int], bounds: tuple[int, int] | None) -> pd.DataFrame:
    """Filter by selected calls, ranks and steps; restrict details to selected phase intervals.

    All phases means all event types. For a subset, include phase events and
    nested apply-context events whose full interval is inside a selected phase.
    Timestamps are compared only within the same transition and rank.
    """
    all_names = set(events.loc[events.label == "apply_total", "caller"])
    all_transitions = bool(all_names) and all_names.issubset(names)
    if all_transitions:
        # Keep initialization and other events that do not belong to an apply call.
        filtered = events[events['rank'].isin(ranks)].copy()
    else:
        calls = events[(events.label == "apply_total") & events.caller.isin(names) & events['rank'].isin(ranks)]
        if bounds is not None:
            calls = calls[calls.step.between(*bounds)]
        keys = calls[["transition_nr", "rank"]].drop_duplicates()
        filtered = events.merge(keys, on=["transition_nr", "rank"], how="inner")
    if bounds is not None:
        filtered = filtered[filtered.step.between(*bounds)]
    if set(selected_phases) == set(PHASES):
        return filtered
    if not selected_phases or filtered.empty:
        return filtered.iloc[0:0]
    filtered = filtered[filtered.context == "apply"].copy()
    keep = np.zeros(len(filtered), dtype=bool)
    filtered = filtered.reset_index(drop=True)
    for _, indices in filtered.groupby(["transition_nr", "rank"], sort=False).groups.items():
        group = filtered.loc[indices]
        windows = group[group.label.isin(selected_phases)].sort_values("start_ns")
        if windows.empty:
            continue
        starts = windows.start_ns.to_numpy(dtype=np.int64)
        ends = np.maximum.accumulate(starts + windows.dur_ns.to_numpy(dtype=np.int64))
        positions = np.searchsorted(starts, group.start_ns.to_numpy(dtype=np.int64), side="right") - 1
        valid = positions >= 0
        included = np.zeros(len(group), dtype=bool)
        included[valid] = (group.start_ns.to_numpy(dtype=np.int64)[valid] + group.dur_ns.to_numpy(dtype=np.int64)[valid]) <= ends[positions[valid]]
        keep[np.asarray(indices)] = included
    return filtered.loc[keep]


def event_table_frame(events: pd.DataFrame) -> pd.DataFrame:
    """Prepare user-facing columns; zero start is the minimum of filtered events."""
    data = events.drop(columns=["source_file"], errors="ignore").copy()
    if not data.empty:
        # Subtract in integer nanoseconds before converting, to avoid float precision loss.
        origin = int(data["start_ns"].min())
        relative_ns = data["start_ns"] - origin
        data["start_rel_ms"] = relative_ns / 1_000_000.0
        data["end_rel_ms"] = (relative_ns + data["dur_ns"]) / 1_000_000.0
    else:
        data["start_rel_ms"] = pd.Series(dtype="float64")
        data["end_rel_ms"] = pd.Series(dtype="float64")
    data.drop(columns=["start_ns", "dur_ns"], inplace=True, errors="ignore")
    if "gc_pause_ns" in data:
        data["gc_pause_ms"] = data.pop("gc_pause_ns") / 1_000_000.0
    leading = ["rank", "step", "transition_nr", "caller", "label", "start_rel_ms", "end_rel_ms", "duration_ms",
               "items", "type", "context", "kind", "send_items", "recv_items", "send_bytes",
               "recv_bytes", "gc_pause_ms"]
    leading = [column for column in leading if column in data]
    agent = [column for column in data if column.startswith("agent_")]
    remaining = [column for column in data if column not in leading and column not in agent]
    return data[leading + remaining + agent]


def _empty(message: str) -> go.Figure:
    return go.Figure().add_annotation(text=message, showarrow=False)


def _interval_depths(events: pd.DataFrame) -> np.ndarray:
    """Infer nesting per rank from visible intervals; equal intervals are ambiguous."""
    depths = np.zeros(len(events), dtype=int)
    for _, group in events.reset_index(drop=True).groupby(["rank", "transition_nr"], sort=False):
        intervals = sorted(
            ((int(row.start_ns), int(row.start_ns) + int(row.dur_ns), index)
             for index, row in group.iterrows()),
            key=lambda interval: (interval[0], -interval[1], interval[2]),
        )
        stack = []
        for start, end, index in intervals:
            while stack and (start >= stack[-1][1] or end > stack[-1][1]):
                stack.pop()
            depths[index] = len(stack)
            if end > start:
                stack.append((start, end))
    return depths


def _gc_ms(events: pd.DataFrame) -> pd.Series:
    return pd.to_numeric(events.get("gc_pause_ns", pd.Series(np.nan, index=events.index)), errors="coerce") / 1e6


def _add_gc_overlay(fig: go.Figure, events: pd.DataFrame, gc: pd.Series, width: float,
                    color: str, legendgroup: str) -> None:
    """Show the GC share at the right edge; its actual position is not recorded."""
    measured = gc.notna() & (gc > 0) & (events.duration_ms > 0)
    if not measured.any():
        return
    rows = events.loc[measured]
    portions = gc.loc[measured].clip(lower=0).clip(upper=rows.duration_ms)
    fig.add_bar(name=f"{legendgroup} GC", y=[f"Rank {r}" for r in rows['rank']],
                x=portions.tolist(), base=(rows.start_ms + rows.duration_ms - portions).tolist(),
                orientation="h", width=width, marker={"color": color, "pattern": {"shape": "/"}},
                legendgroup=legendgroup, showlegend=False, hoverinfo="skip", meta="gc_overlay")


def event_timeline(events: pd.DataFrame, apply_events: pd.DataFrame, trace_name: str | None = None) -> go.Figure:
    """Show selected calls on each rank's clock, aligned to its first selected apply."""
    apply_rows = apply_events.copy()
    if apply_rows.empty:
        return _empty("No apply events for the current filters.")
    keys = apply_rows[["rank", "transition_nr"]].drop_duplicates()
    details = events[(events.context == "apply") & (events.label != "apply_total")].merge(keys, on=["rank", "transition_nr"], how="inner")
    origins = apply_rows.groupby("rank").start_ns.min()
    apply_rows["start_ms"] = (apply_rows.start_ns - apply_rows['rank'].map(origins)) / 1e6
    fig = go.Figure()
    apply_gc = _gc_ms(apply_rows)
    apply_metadata = [[float(row.start_ms), float(row.duration_ms), 0, 0, 0,
                       int(row.step), int(row.transition_nr), str(row.caller),
                       None if pd.isna(gc) else float(gc)]
                      for row, gc in zip(apply_rows.itertuples(), apply_gc)]
    apply_gc_hover = ["<br>GC: unavailable" if pd.isna(gc) else f"<br>GC: {gc:.4f} ms" for gc in apply_gc]
    fig.add_bar(name="apply_total (context)", y=[f"Rank {r}" for r in apply_rows['rank']],
                x=apply_rows.duration_ms.tolist(), base=apply_rows.start_ms.tolist(), orientation="h", width=0.85,
                customdata=apply_metadata, marker_color="rgba(115,130,124,0.22)", legendrank=0,
                legendgroup="apply_total (context)", hovertext=apply_gc_hover,
                hovertemplate="%{y}<br>Step %{customdata[5]}<br>Transition %{customdata[6]} · %{customdata[7]}<br>Start %{customdata[0]:.4f} ms<br>Duration %{customdata[1]:.4f} ms%{hovertext}<extra>%{fullData.name}</extra>")
    _add_gc_overlay(fig, apply_rows, apply_gc, 0.85, "rgba(115,130,124,0.5)", "apply_total (context)")
    if not details.empty:
        details = details[details['rank'].isin(origins.index)].copy()
        details["start_ms"] = (details.start_ns - details['rank'].map(origins)) / 1e6
        details["depth"] = _interval_depths(details)
        labels = sorted(details.label.unique())
        colors = {label: qualitative.Plotly[index % len(qualitative.Plotly)] for index, label in enumerate(labels)}
        first_starts = details.groupby("label").start_ms.min()
        legend_order = sorted(labels, key=lambda label: (first_starts[label], label))
        legend_ranks = {label: index for index, label in enumerate(legend_order)}
        shown = set()
        # Draw parents first so narrower children remain visible on top.
        for (depth, label), group in details.groupby(["depth", "label"], sort=True):
            send_bytes = group.get("send_bytes", pd.Series(0, index=group.index)).fillna(0)
            send_items = group.get("send_items", pd.Series(0, index=group.index)).fillna(0)
            gc = _gc_ms(group)
            event_hover = [("<br>GC: unavailable" if pd.isna(g) else f"<br>GC: {g:.4f} ms") +
                           (f"<br>Sent: {int(nbytes):,} bytes ({int(items):,} items)"
                            if nbytes > 0 or items > 0 else "")
                           for g, nbytes, items in zip(gc, send_bytes, send_items)]
            fig.add_bar(name=str(label), y=[f"Rank {r}" for r in group['rank']],
                        x=group.duration_ms.tolist(), base=group.start_ms.tolist(), orientation="h",
                        width=0.65 * 0.7 ** int(depth), marker_color=colors[label],
                        legendgroup=str(label), legendrank=legend_ranks[label] + 1, showlegend=label not in shown,
                        customdata=[[float(row.start_ms), float(row.duration_ms), int(row.depth) + 1,
                                     int(nbytes), int(items), int(row.step), int(row.transition_nr), str(row.caller),
                                     None if pd.isna(g) else float(g)]
                                    for row, nbytes, items, g in zip(group.itertuples(), send_bytes, send_items, gc)],
                        hovertext=event_hover,
                        hovertemplate="%{y}<br>Step %{customdata[5]}<br>Transition %{customdata[6]} · %{customdata[7]}<br>Start %{customdata[0]:.4f} ms<br>Duration %{customdata[1]:.4f} ms<br>Visible nesting level %{customdata[2]:.0f}%{hovertext}<extra>%{fullData.name}</extra>")
            _add_gc_overlay(fig, group, gc, 0.65 * 0.7 ** int(depth), colors[label], str(label))
            shown.add(label)
    fig.update_layout(title=f"Event timeline: {trace_name}" if trace_name else "Event timeline: selected transitions", xaxis_title="Time relative to the first selected apply on each rank (ms)", yaxis_title="Rank", barmode="overlay", hovermode="closest", height=max(300, 50 * apply_rows['rank'].nunique() + 130))
    return fig
