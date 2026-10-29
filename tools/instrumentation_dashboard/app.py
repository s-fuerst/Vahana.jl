"""Local Dash UI for Vahana instrumentation HDF5 files."""
from __future__ import annotations

import argparse
from pathlib import Path

import numpy as np
import pandas as pd
import plotly.graph_objects as go
from dash import Dash, Input, Output, dcc, html

from hdf5_reader import load_instrumentation_hdf5


COLORS = {
    "compute": "#15806a",
    "mpi": "#438cbc",
    "wait": "#d38a24",
    "setup": "#8a72bb",
}
PAGE_STYLE = {
    "fontFamily": "Inter, ui-sans-serif, system-ui, sans-serif",
    "backgroundColor": "#f4f7f5",
    "color": "#1d2a2e",
    "minHeight": "100vh",
    "padding": "24px clamp(16px, 3vw, 42px) 44px",
}
CARD_STYLE = {
    "background": "#fff",
    "border": "1px solid #e2e9e5",
    "borderRadius": "10px",
    "padding": "16px 18px",
    "boxShadow": "0 2px 8px rgba(28, 50, 42, .04)",
}


def _rank_label(rank: int) -> str:
    return f"Rank {int(rank)}"


def _event_category(row: pd.Series) -> str:
    label = str(row["label"])
    if label.startswith("ready_wait"):
        return "wait"
    if row["kind"] != "phase":
        return "mpi"
    if label == "transition":
        return "compute"
    return "setup"


def _run_summary(events: pd.DataFrame) -> dict[str, str]:
    apply_events = events.loc[events["label"] == "apply_total"]
    if apply_events.empty:
        elapsed = "—"
    else:
        makespan_by_transition = apply_events.groupby("transition_nr")["dur_ns"].max()
        elapsed = f"{makespan_by_transition.sum() / 1e9:.3f} s"
    return {
        "elapsed": elapsed,
        "transitions": str(events.loc[events["label"] == "apply_total", "transition_nr"].nunique()),
        "ranks": str(events["rank"].nunique()),
        "events": f"{len(events):,}",
    }


def make_timeseries_figure(events: pd.DataFrame) -> go.Figure:
    apply_events = events.loc[events["label"] == "apply_total"]
    fig = go.Figure()
    if apply_events.empty:
        fig.add_annotation(text="Keine apply_total-Ereignisse", showarrow=False)
    else:
        summary = (
            apply_events.groupby("transition_nr")["duration_ms"]
            .agg(median="median", maximum="max")
            .reset_index()
        )
        fig.add_trace(go.Scatter(
            x=summary["transition_nr"], y=summary["median"], mode="lines+markers",
            name="Median über Ranks", line={"color": COLORS["compute"], "width": 2},
            hovertemplate="Transition %{x}<br>Median %{y:.3f} ms<extra></extra>",
        ))
        fig.add_trace(go.Scatter(
            x=summary["transition_nr"], y=summary["maximum"], mode="lines",
            name="Maximum über Ranks", line={"color": COLORS["wait"], "dash": "dot", "width": 1.5},
            hovertemplate="Transition %{x}<br>Maximum %{y:.3f} ms<extra></extra>",
        ))
    fig.update_layout(
        title="apply! duration by transition", xaxis_title="Transition",
        yaxis_title="Dauer (ms)", hovermode="x unified", height=320,
        margin={"l": 55, "r": 18, "t": 55, "b": 48},
        legend={"orientation": "h", "y": 1.15, "x": 0},
        paper_bgcolor="white", plot_bgcolor="white",
    )
    fig.update_xaxes(showgrid=True, gridcolor="#edf1ef")
    fig.update_yaxes(rangemode="tozero", showgrid=True, gridcolor="#edf1ef")
    return fig


def _instrumented_coverage_percent(detail: pd.DataFrame, apply_events: pd.DataFrame) -> float:
    """Union coverage of child event intervals; nested durations are not summed."""
    ratios = []
    for total in apply_events.itertuples(index=False):
        total_ms = float(total.duration_ms)
        if total_ms <= 0:
            continue
        intervals = detail.loc[detail["rank"] == total.rank, ["start_ms", "duration_ms"]]
        spans = sorted(
            (max(0.0, float(row.start_ms)), min(total_ms, float(row.start_ms + row.duration_ms)))
            for row in intervals.itertuples(index=False)
            if row.duration_ms > 0
        )
        covered = 0.0
        end = 0.0
        for start, stop in spans:
            if stop <= start:
                continue
            if start > end:
                covered += stop - start
            elif stop > end:
                covered += stop - end
            end = max(end, stop)
        ratios.append(min(1.0, covered / total_ms))
    return float(np.median(ratios)) if ratios else 0.0


def make_gantt_figure(events: pd.DataFrame, transition_nr: int | None) -> go.Figure:
    fig = go.Figure()
    if transition_nr is None:
        fig.add_annotation(text="Keine Transition ausgewählt", showarrow=False)
        return fig

    selected = events.loc[
        (events["transition_nr"] == int(transition_nr)) & (events["context"] == "apply")
    ]
    apply_events = selected.loc[selected["label"] == "apply_total"].copy()
    origins = {
        int(row.rank): int(row.start_ns)
        for row in apply_events.itertuples(index=False)
    }
    detail = selected.loc[~selected["label"].isin(["apply_total", "transition_stats"])].copy()
    rank_values = sorted(int(rank) for rank in selected["rank"].unique())
    rank_labels = [_rank_label(rank) for rank in rank_values]
    if detail.empty and apply_events.empty:
        fig.add_annotation(text="Keine apply_total- oder Detailereignisse für diese Transition", showarrow=False)
        fig.update_layout(height=260)
        return fig

    if not detail.empty:
        detail["rank_label"] = detail["rank"].map(_rank_label)
        local_origins = detail["rank"].map(origins).fillna(detail["start_ns"])
        detail["start_ms"] = ((detail["start_ns"] - local_origins) / 1e6).clip(lower=0.0)
        detail["category"] = detail.apply(_event_category, axis=1)

    # The broad, translucent band is the measured apply_total interval. The
    # colored segments are the intervals with individual instrumentation;
    # uncovered portions are not labelled as idle, because they may contain
    # work that is not separately instrumented.
    if not apply_events.empty:
        apply_events["rank_label"] = apply_events["rank"].map(_rank_label)
        fig.add_trace(go.Bar(
            name="apply_total · Gesamtintervall",
            y=apply_events["rank_label"], x=apply_events["duration_ms"], base=0,
            orientation="h", width=0.84,
            marker={"color": "rgba(115, 130, 124, 0.20)", "line": {"color": "rgba(95, 110, 104, 0.48)", "width": 1}},
            customdata=np.column_stack([apply_events["transition_nr"], apply_events["duration_ms"]]),
            hovertemplate="apply_total · %{y}<br>Gesamtdauer %{x:.3f} ms<extra></extra>",
        ))

    legend_names = {"compute": "Berechnung", "mpi": "MPI", "wait": "ready_wait", "setup": "Setup"}
    short_event_threshold_ms = max(0.05, float(apply_events["duration_ms"].max()) / 900) if not apply_events.empty else 0.05
    for category, color in COLORS.items():
        group = detail.loc[detail["category"] == category] if not detail.empty else detail
        if group.empty:
            continue
        name = legend_names[category]
        long_events = group.loc[group["duration_ms"] > short_event_threshold_ms]
        if not long_events.empty:
            long_hover = np.column_stack([
                long_events["label"].astype(str), long_events["type"].astype(str),
                long_events["items"].astype(str), long_events["send_bytes"].astype(str),
                long_events["recv_bytes"].astype(str), long_events["start_ms"],
            ])
            fig.add_trace(go.Bar(
                name=name, y=long_events["rank_label"], x=long_events["duration_ms"],
                base=long_events["start_ms"], orientation="h",
                marker={"color": color, "line": {"width": 0}}, customdata=long_hover, width=0.48,
                hovertemplate=(
                    "%{customdata[0]} · %{customdata[1]}<br>"
                    "%{y}<br>Start %{customdata[5]:.4f} ms · Dauer %{x:.4f} ms<br>"
                    "Items %{customdata[2]} · send %{customdata[3]} B · recv %{customdata[4]} B"
                    "<extra></extra>"
                ),
            ))

        short_events = group.loc[group["duration_ms"] <= short_event_threshold_ms]
        if not short_events.empty:
            short_hover = np.column_stack([
                short_events["label"].astype(str), short_events["type"].astype(str),
                short_events["duration_ms"], short_events["items"].astype(str),
            ])
            fig.add_trace(go.Scatter(
                name=f"{name} · kurze Ereignisse", x=short_events["start_ms"],
                y=short_events["rank_label"], mode="markers",
                marker={"color": color, "size": 8, "symbol": "diamond", "line": {"color": "white", "width": 1}},
                customdata=short_hover, showlegend=False,
                hovertemplate=(
                    "%{customdata[0]} · %{customdata[1]}<br>%{y}<br>"
                    "Start %{x:.4f} ms · tatsächliche Dauer %{customdata[2]:.4f} ms<br>"
                    "Items %{customdata[3]}<extra></extra>"
                ),
            ))

    max_apply_ms = float(apply_events["duration_ms"].max()) if not apply_events.empty else 0.0
    max_detail_ms = float((detail["start_ms"] + detail["duration_ms"]).max()) if not detail.empty else 0.0
    max_end = max(max_apply_ms, max_detail_ms)
    uncovered_percent = 100.0 - 100.0 * _instrumented_coverage_percent(detail, apply_events)
    subtitle = (
        "Grau = apply_total · farbige Balken = instrumentierte Intervalle · "
        f"ca. {uncovered_percent:.0f}% nicht separat instrumentiert (nicht automatisch Leerlauf)"
    )
    fig.update_layout(
        title={"text": f"Ereignis-Timeline · Transition {transition_nr}<br><sup>{subtitle}</sup>", "x": 0.01},
        xaxis_title="Zeit relativ zum lokalen apply!-Start (ms)",
        yaxis_title="MPI-Rank", barmode="overlay", height=max(300, 58 * len(rank_labels) + 90),
        margin={"l": 86, "r": 18, "t": 94, "b": 55},
        legend={"orientation": "h", "y": 1.13, "x": 0},
        paper_bgcolor="white", plot_bgcolor="white",
    )
    fig.update_xaxes(range=[0, max(0.05, max_end * 1.03)], showgrid=True, gridcolor="#edf1ef")
    fig.update_yaxes(categoryorder="array", categoryarray=rank_labels, autorange="reversed")
    return fig


def make_agent_distribution_figure(events: pd.DataFrame, transition_nr: int | None, agent_type: str | None) -> go.Figure:
    fig = go.Figure()
    subset = events.loc[
        (events["label"] == "transition") &
        (events["transition_nr"] == transition_nr) &
        (events["type"] == agent_type)
    ] if transition_nr is not None and agent_type else events.iloc[0:0]
    if subset.empty:
        fig.add_annotation(text="Keine Agentenzahl-Ereignisse für diese Auswahl", showarrow=False)
    else:
        subset = subset.sort_values("rank")
        rank_labels = [_rank_label(r) for r in subset["rank"]]
        mean_count = float(subset["items"].mean())
        fig.add_trace(go.Bar(
            x=rank_labels, y=subset["items"], name="Lebende Agenten",
            marker_color=COLORS["compute"],
            customdata=np.column_stack([subset["type"], subset["transition_nr"]]),
            hovertemplate="%{x}<br>%{y:,} Agenten<br>Typ %{customdata[0]} · Transition %{customdata[1]}<extra></extra>",
        ))
        fig.add_hline(y=mean_count, line_dash="dash", line_color=COLORS["wait"],
                      annotation_text=f"Mittel {mean_count:,.0f}", annotation_position="top left")
    fig.update_layout(
        title="Lebende Agenten am Eintritt in apply!", xaxis_title="MPI-Rank",
        yaxis_title="Agenten", height=310, showlegend=False,
        margin={"l": 55, "r": 18, "t": 60, "b": 45},
        paper_bgcolor="white", plot_bgcolor="white",
    )
    fig.update_yaxes(rangemode="tozero", showgrid=True, gridcolor="#edf1ef")
    return fig


def make_diagnostic_table(events: pd.DataFrame, transition_nr: int | None):
    rows = events.loc[
        (events["label"] == "transition_stats") &
        (events["transition_nr"] == transition_nr) &
        (events["items"] > 0)
    ] if transition_nr is not None else events.iloc[0:0]
    if rows.empty:
        return html.Div(
            "Keine Agenten-Timing-Samples für diese Transition. Für Diagnosemessungen "
            "vor dem Lauf enable_diagnostics!(sim, true) aktivieren; sie fügen Overhead hinzu.",
            style={"color": "#647175", "fontSize": "13px", "padding": "12px 2px"},
        )

    body = []
    for event in rows.sort_values(["type", "rank"]).itertuples(index=False):
        n = int(event.items)
        mean_ns = int(event.agent_sum_ns) / n
        variance = max(0.0, int(event.agent_sum_sq_ns) / n - mean_ns**2)
        body.append(html.Tr([
            html.Td(event.type), html.Td(_rank_label(event.rank)), html.Td(f"{n:,}"),
            html.Td(f"{event.agent_min_ns / 1e3:.2f}"),
            html.Td(f"{mean_ns / 1e3:.2f}"),
            html.Td(f"{event.agent_max_ns / 1e3:.2f}"),
            html.Td(f"{np.sqrt(variance) / 1e3:.2f}"),
        ]))
    header = html.Thead(html.Tr([
        html.Th("Agententyp"), html.Th("Rank"), html.Th("N"),
        html.Th("Min µs"), html.Th("Mittel µs"), html.Th("Max µs"), html.Th("σ µs"),
    ]))
    return html.Table([header, html.Tbody(body)], style={"width": "100%", "borderCollapse": "collapse"})


def _card(label: str, value: str) -> html.Div:
    return html.Div([
        html.Div(label, style={"fontSize": "12px", "color": "#68767a"}),
        html.Div(value, style={"fontSize": "24px", "fontWeight": 700, "marginTop": "7px"}),
    ], style={**CARD_STYLE, "minWidth": "130px", "flex": "1"})


def create_app(events: pd.DataFrame, source_file: str | Path) -> Dash:
    """Create a Dash app around an already-loaded HDF5 event dataframe."""
    summary = _run_summary(events)
    transitions = sorted(int(t) for t in events.loc[events["label"] == "apply_total", "transition_nr"].unique())
    agent_types = sorted(str(t) for t in events.loc[events["label"] == "transition", "type"].unique() if str(t))
    ranks = sorted(int(r) for r in events["rank"].unique())
    initial_transition = transitions[0] if transitions else None
    initial_type = agent_types[0] if agent_types else None

    app = Dash(__name__, title="Vahana instrumentation")
    app.layout = html.Div([
        html.Header([
            html.Div("Vahana / Instrumentation", style={"fontSize": "22px", "fontWeight": 750}),
            html.Div(f"Direkt aus HDF5 · {Path(source_file).name}", style={"color": "#68767a", "fontSize": "12px", "marginTop": "4px"}),
        ], style={"marginBottom": "20px"}),
        html.Div([
            _card("Transitionzeit · Σ max/Ranks", summary["elapsed"]),
            _card("Transitionen", summary["transitions"]),
            _card("MPI-Ranks", summary["ranks"]),
            _card("Ereignisse", summary["events"]),
        ], style={"display": "flex", "gap": "12px", "flexWrap": "wrap", "marginBottom": "14px"}),
        html.Div([
            html.Label("Transition", htmlFor="transition-select", style={"fontSize": "12px", "fontWeight": 650}),
            dcc.Dropdown(
                id="transition-select",
                options=[{"label": f"Transition {t}", "value": t} for t in transitions],
                value=initial_transition, clearable=False,
            ),
            html.Label("Agententyp", htmlFor="agent-type-select", style={"fontSize": "12px", "fontWeight": 650, "marginTop": "12px", "display": "block"}),
            dcc.Dropdown(
                id="agent-type-select",
                options=[{"label": t, "value": t} for t in agent_types],
                value=initial_type, clearable=False,
            ),
            html.Div("Die Timeline normalisiert Zeit pro Rank relativ zum lokalen apply!-Start.",
                     style={"fontSize": "11px", "color": "#68767a", "marginTop": "8px"}),
        ], style={**CARD_STYLE, "marginBottom": "14px"}),
        html.Div([
            html.Section([dcc.Graph(id="timeseries", figure=make_timeseries_figure(events), config={"displaylogo": False})], style=CARD_STYLE),
            html.Section([dcc.Graph(id="gantt", figure=make_gantt_figure(events, initial_transition), config={"displaylogo": False})], style=CARD_STYLE),
        ], style={"display": "grid", "gridTemplateColumns": "repeat(auto-fit, minmax(420px, 1fr))", "gap": "14px", "marginBottom": "14px"}),
        html.Div([
            html.Section([dcc.Graph(id="agent-distribution", figure=make_agent_distribution_figure(events, initial_transition, initial_type), config={"displaylogo": False})], style=CARD_STYLE),
            html.Section([
                html.H3("Agenten-Ausführungsstatistik", style={"fontSize": "15px", "marginTop": 0}),
                html.Div(id="diagnostic-table", children=make_diagnostic_table(events, initial_transition)),
            ], style=CARD_STYLE),
        ], style={"display": "grid", "gridTemplateColumns": "repeat(auto-fit, minmax(360px, 1fr))", "gap": "14px"}),
        html.Footer("Diagnose-Timings haben Mess-Overhead und sind zur Diagnose gedacht, nicht für Benchmarks.",
                    style={"color": "#758185", "fontSize": "11px", "marginTop": "16px"}),
    ], style=PAGE_STYLE)

    @app.callback(
        Output("gantt", "figure"),
        Output("agent-distribution", "figure"),
        Output("diagnostic-table", "children"),
        Input("transition-select", "value"),
        Input("agent-type-select", "value"),
    )
    def update_transition_views(transition_nr: int | None, agent_type: str | None):
        return (
            make_gantt_figure(events, transition_nr),
            make_agent_distribution_figure(events, transition_nr, agent_type),
            make_diagnostic_table(events, transition_nr),
        )

    return app


def main() -> None:
    parser = argparse.ArgumentParser(description="Inspect Vahana instrumentation HDF5 files in a local Dash app.")
    parser.add_argument("filename", help="HDF5 file, base filename, or base for per-rank _<rank>.h5 files")
    parser.add_argument("--host", default="127.0.0.1", help="Bind address (default: localhost only)")
    parser.add_argument("--port", type=int, default=8050, help="HTTP port (default: 8050)")
    parser.add_argument("--debug", action="store_true", help="Enable Dash development mode and hot reload")
    args = parser.parse_args()

    events = load_instrumentation_hdf5(args.filename)
    app = create_app(events, args.filename)
    print(f"Loaded {len(events):,} instrumentation events from {len(events.attrs['source_files'])} HDF5 file(s).")
    print(f"Open http://{args.host}:{args.port}")
    app.run(host=args.host, port=args.port, debug=args.debug)


if __name__ == "__main__":
    main()
