"""Multi-page instrumentation dashboard with per-browser shared run selections."""
from __future__ import annotations

from functools import lru_cache
from pathlib import Path
import re
import h5py
import pandas as pd
from dash import Dash, Input, Output, State, dash_table, dcc, html, no_update
from dash.exceptions import PreventUpdate
from dash.dash_table.Format import Format, Group, Scheme

from analysis import PHASES, PRE_PHASES, POST_PHASES, phases, phase_figure, phase_summary, selected_summary, step_figure, transition_summary, transition_totals
from fast_reader import load_trace
from hdf5_reader import _resolve_files
from event_views import event_table_frame, event_timeline, filter_events

CARD = {"background": "white", "padding": "16px", "borderRadius": "8px", "marginBottom": "12px"}


def parse_step_range(value: str | None) -> tuple[int, int] | None:
    """Parse an inclusive single step or range; None means no text filter."""
    if not value or not value.strip() or value.strip().lower() == "all":
        return None
    match = re.fullmatch(r"\s*(-?\d+)\s*(?:-\s*(-?\d+)\s*)?", value)
    if not match:
        raise ValueError("Enter all, a step, or an inclusive range such as 5-7.")
    start = int(match.group(1))
    end = int(match.group(2)) if match.group(2) is not None else start
    if start > end:
        raise ValueError("The first step must not exceed the last step.")
    return start, end


def scope_steps(events: pd.DataFrame, phase_data: pd.DataFrame, value: str | None) -> tuple[pd.DataFrame, pd.DataFrame]:
    """Filter event and phase metrics to calls whose apply event has a selected step."""
    bounds = parse_step_range(value)
    if bounds is None:
        return events, phase_data
    calls = events.loc[(events.label == "apply_total") & events.step.between(*bounds), "transition_nr"].unique()
    return events[events.transition_nr.isin(calls)], phase_data[phase_data.transition_nr.isin(calls)]


def scan_directory(directory: str) -> tuple[list[dict], list[dict], str]:
    path = Path(directory).expanduser().resolve()
    if not path.is_dir():
        raise ValueError(f"Not a directory: {path}")
    folders = [{"label": "↰ Parent directory", "value": str(path.parent)}]
    folders += [{"label": p.name + "/", "value": str(p)} for p in sorted(path.iterdir()) if p.is_dir()]
    files = []
    for p in sorted(path.glob("*.h5")):
        try:
            with h5py.File(p, "r") as f:
                if "instrumentation/events" in f:
                    files.append({"label": p.name, "value": str(p)})
        except (OSError, ValueError):
            continue
    rank_candidates = {}
    all_candidates = {}
    for entry in files:
        candidate = Path(entry["value"])
        match = re.match(r"^(.*)_(\d+)\.h5$", candidate.name)
        if not match:
            continue
        all_candidates.setdefault(match.group(1), []).append(int(match.group(2)))
        try:
            with h5py.File(candidate, "r") as f:
                dataset = f["instrumentation/events"]
                counts = f["instrumentation"].attrs.get("n_events_per_rank", [])
                # A filename suffix alone may be an MPI size, not a rank.
                if len(counts) != 1 or "rank" not in (dataset.dtype.names or ()) or not len(dataset):
                    continue
                ranks = dataset.fields("rank")[:]
                rank = int(match.group(2))
                if not (ranks == rank).all():
                    continue
                rank_candidates.setdefault(match.group(1), []).append(rank)
        except (OSError, ValueError, TypeError):
            continue
    for base, ranks in sorted(rank_candidates.items()):
        if len(ranks) >= 2 and len(ranks) == len(set(ranks)) and sorted(ranks) == sorted(all_candidates[base]):
            files.append({"label": f"{base} (all rank files)", "value": str(path / base)})
    return folders, files, str(path)


def _signature(path: str) -> tuple[str, tuple[tuple[str, int, int], ...]]:
    requested = str(Path(path).resolve())
    files = []
    for p, _ in _resolve_files(requested):
        stat = p.stat()
        files.append((str(p), stat.st_mtime_ns, stat.st_size))
    return requested, tuple(files)


@lru_cache(maxsize=4)
def _cached_trace(path: str, files: tuple[tuple[str, int, int], ...]) -> pd.DataFrame:
    return load_trace(path)


def get_trace(path: str) -> pd.DataFrame:
    return _cached_trace(*_signature(path))


@lru_cache(maxsize=4)
def _cached_phases(path: str, files: tuple[tuple[str, int, int], ...]) -> pd.DataFrame:
    return phases(_cached_trace(path, files))


def get_phases(path: str) -> pd.DataFrame:
    return _cached_phases(*_signature(path))


def _names(*runs: pd.DataFrame) -> list[str]:
    return sorted({str(c) for run in runs for c in run.loc[run.label == "transition", "caller"] if str(c)})


def create_dashboard(initial_directory: str | Path, initial_file: str | Path | None = None) -> Dash:
    app = Dash(__name__, title="Vahana instrumentation")
    initial_directory = str(Path(initial_directory).expanduser().resolve())
    initial_file = str(Path(initial_file).resolve()) if initial_file else None
    app.layout = html.Div([
        dcc.Store(id="initial-file", data=initial_file),
        dcc.Store(id="trace-preferences", storage_type="local"),
        dcc.Store(id="trace-restore-complete", data=False),
        dcc.Store(id="selection-state"),
        dcc.Store(id="phase-selection-state"),
        dcc.Store(id="rank-selection-state"),
        html.Aside([
            html.H2("Vahana"), html.P("Instrumentation"),
            dcc.RadioItems(id="page", options=[{"label": "Filters", "value": "filters"}, {"label": "Plots", "value": "plots"}, {"label": "Timelines", "value": "timelines"}, {"label": "Events", "value": "events"}, {"label": "Agents", "value": "agents"}], value="plots", labelStyle={"display": "block", "padding": "10px 0"}),
        ], style={"width": "160px", "flexShrink": 0, "padding": "16px", "background": "#eaf1ef"}),
        html.Main([
            html.H1("Instrumentation insights"),
            html.Section([
                html.Label("HDF5 directory"),
                dcc.Input(id="directory", value=initial_directory, type="text", debounce=True,
                          persistence=True, persistence_type="local", style={"width": "75%"}),
                html.Button("Scan", id="scan", n_clicks=0),
                dcc.Dropdown(id="directories", placeholder="Browse directories", clearable=True),
                html.Div(id="scan-status"),
                html.Div([
                    html.Div([html.Label("Trace 1"), dcc.Dropdown(id="trace-1", clearable=False, persistence=True, persistence_type="local")], style={"flex": "1", "minWidth": "230px"}),
                    html.Div([html.Label("Trace 2"), dcc.Dropdown(id="trace-2", clearable=False, persistence=True, persistence_type="local")], style={"flex": "1", "minWidth": "230px"}),
                    html.Div([html.Label("Active trace for single-run views"), dcc.RadioItems(id="active-trace", options=[{"label": "Trace 1", "value": 1}, {"label": "Trace 2", "value": 2}], value=1, inline=True, persistence=True, persistence_type="local")], style={"minWidth": "240px"}),
                ], style={"display": "flex", "gap": "16px", "flexWrap": "wrap"}),
            ], style=CARD),
            html.Div(id="load-error", style={"color": "#a21d1d"}),
            html.Section(id="filters-page", children=[
                html.H2("Step selection"),
                html.Label("Step or inclusive range (tables and phase plot)"),
                dcc.Input(id="step-range", type="text", value="all", placeholder="all or 5-7", debounce=True),
                html.Div(id="step-range-error", style={"color": "#a21d1d"}),
                html.H2("Phase selection"),
                html.P("Select one or more phases. Pre/Post select the named individual phases; they do not reproduce the former dynamically calculated local-work aggregates. Phase metrics cover all transitions in the active trace and selected steps."),
                html.Button("Select all", id="phase-all", n_clicks=0),
                html.Button("Remove all", id="phase-none", n_clicks=0),
                html.Button("Pre-transition", id="phase-pre", n_clicks=0),
                html.Button("Post-transition", id="phase-post", n_clicks=0),
                dash_table.DataTable(id="phase-table", row_selectable="multi", selected_row_ids=[], data=[], columns=[], sort_action="native", style_table={"overflowX": "auto"}, style_cell={"padding": "7px", "textAlign": "left", "minWidth": "105px"}),
                html.Div(id="phase-total"),
                html.H2("Transition selection"),
                html.P("Select one or more transitions for both plots. Metrics refer to the active trace and selected phases. Apply sent/received covers the entire apply call, not just those phases."),
                html.Button("Select all", id="select-all", n_clicks=0),
                html.Button("Remove all", id="remove-all", n_clicks=0, style={"marginLeft": "8px"}),
                dash_table.DataTable(id="transition-table", row_selectable="multi", selected_row_ids=[], data=[], columns=[], sort_action="native", style_table={"overflowX": "auto"}, style_cell={"padding": "7px", "textAlign": "left", "minWidth": "105px"}),
                html.Div(id="transition-total"),
                html.P("Mean and max phase duration use the maximum rank duration per call. Mean apply barrier time sums recorded barrier events per call and rank, then averages over calls and ranks; it covers the whole apply, independent of selected phases. Missing barrier events count as zero. The selected-transition total sums displayed time means and maxima; maxima may occur in different calls. GC share is total rank-local GC pause divided by total phase duration. Byte fields are recorded volumes, not necessarily physical traffic."),
                html.H2("Rank selection"),
                html.P("Select ranks for the phase and agent plots. The run comparison and other table metrics continue to use all ranks. Mean phase time is the sum of selected phases per call, averaged over recorded calls. The total row averages these rank means; GC share is duration-weighted."),
                html.Button("Select all", id="rank-all", n_clicks=0),
                html.Button("Remove all", id="rank-none", n_clicks=0, style={"marginLeft": "8px"}),
                dash_table.DataTable(id="rank-table", row_selectable="multi", selected_row_ids=[], data=[], columns=[], sort_action="native", style_table={"overflowX": "auto"}, style_cell={"padding": "7px", "textAlign": "left", "minWidth": "105px"}),
                html.Div(id="rank-total"),
            ], style={**CARD, "display": "none"}),
            html.Section(id="plots-page", children=[
                html.H2("Duration over steps — run comparison"),
                html.P("When all phases are selected, this compares apply_total. Otherwise, it compares the sum of the selected phases. For each apply call, the maximum rank duration is counted once; selected calls are summed per step. Barriers are not included in selected-phase sums. This is not necessarily the step's wall-clock duration."),
                html.Label("Apply duration display"),
                dcc.RadioItems(id="duration-mode", options=[{"label": "Per step", "value": "step"}, {"label": "Cumulative", "value": "cumulative"}], value="step", inline=True),
                dcc.Graph(id="step-plot"),
                html.H2("Phase comparison across ranks"),
                html.P("Selected individual phase durations are summed. Synchronizing phases may include waiting. Hatched areas indicate GC; missing GC data is not shown as measured zero."),
                dcc.Graph(id="phase-plot"),
            ], style=CARD),
            html.Section(id="timelines-page", children=[
                html.H2("Event timelines"),
                html.P("Both traces use the global filters, which are configured from the active trace. Each timeline shows all selected steps and transitions. Narrower bars indicate nesting inferred from the visible intervals on each rank; identical intervals can be ambiguous. Zoom in to see short events. Time starts at each rank's first selected apply; ranks are not globally synchronized. The gray apply_total interval is context, including when only some phases are selected. Hatched portions show the amount of GC time, not when it occurred; nested events include their children's GC time. Missing GC data is shown in the tooltip. Gaps are not necessarily idle time."),
                html.H3("Trace 1"), dcc.Graph(id="timeline-event-plot"),
                html.H3("Trace 2"), dcc.Graph(id="timeline-event-plot-2"),
            ], style={**CARD, "display": "none"}),
            html.Section(id="events-page", children=[
                html.H2("Events"),
                html.P("Events for the active trace, filtered by step, transition, phase and rank. With all transitions and phases selected, all events are shown, including initialization. With selected transitions, events are restricted to those apply calls; with selected phases, only their intervals and nested apply events are shown. Start times are relative to the minimum start of all filtered events, before pagination. MPI clocks are rank-local and are not synchronized; relative start times do not establish cross-rank order. Nanosecond fields are displayed as text to preserve integer precision."),
                html.Div(id="events-status"),
                dash_table.DataTable(id="events-table", data=[], columns=[], page_action="custom", page_current=0, page_size=50, page_count=0, sort_action="custom", sort_mode="single", sort_by=[], style_table={"overflowX": "auto"}, style_cell={"padding": "6px", "textAlign": "left", "minWidth": "110px"}),
            ], style={**CARD, "display": "none"}),
            html.Section(id="agents-page", children=[
                html.H2("Agent distribution"), html.P("Live agent counts at transition entry, per rank and agent type, for the active trace."),
                html.Label("Transition"), dcc.Dropdown(id="agent-transition", clearable=False),
                html.Label("Agent type"), dcc.Dropdown(id="agent-type", clearable=False),
                dcc.Graph(id="agent-plot"),
            ], style={**CARD, "display": "none"}),
        ], style={"flex": 1, "minWidth": 0, "padding": "20px"}),
    ], style={"display": "flex", "fontFamily": "system-ui, sans-serif", "background": "#f4f7f5", "minHeight": "100vh"})

    @app.callback(Output("step-range-error", "children"), Input("step-range", "value"))
    def validate_step_range(value):
        try:
            parse_step_range(value)
            return ""
        except ValueError as exc:
            return str(exc)

    @app.callback(Output("directory", "value"), Input("directories", "value"), prevent_initial_call=True)
    def browse(path):
        return path if path else no_update

    @app.callback(Output("trace-preferences", "data"), Input("directory", "value"),
                  Input("trace-1", "value"), Input("trace-2", "value"),
                  Input("trace-restore-complete", "data"), prevent_initial_call=True)
    def remember_traces(directory, first, second, restored):
        if not restored or not directory or not first or not second:
            raise PreventUpdate
        return {"directory": str(Path(directory).expanduser().resolve()), "trace1": first, "trace2": second}

    @app.callback(Output("directories", "options"), Output("trace-1", "options"), Output("trace-2", "options"), Output("trace-1", "value"), Output("trace-2", "value"), Output("scan-status", "children"), Output("trace-restore-complete", "data"), Input("directory", "value"), Input("scan", "n_clicks"), State("trace-1", "value"), State("trace-2", "value"), State("initial-file", "data"), State("trace-preferences", "data"), State("trace-restore-complete", "data"))
    def scan(directory, clicks, first, second, initial, preferences, restored):
        try:
            folders, files, resolved = scan_directory(directory or "")
            if initial and str(Path(initial).parent) == resolved and not any(f["value"] == initial for f in files):
                try:
                    _resolve_files(initial)
                except FileNotFoundError:
                    pass
                else:
                    files.append({"label": f"{Path(initial).name} (all rank files)", "value": initial})
            paths = [f["value"] for f in files]
            if not paths:
                return folders, files, files, no_update, no_update, f"No instrumentation files in {resolved}", no_update
            preferences = preferences if isinstance(preferences, dict) and preferences.get("directory") == resolved else {}
            saved_first, saved_second = preferences.get("trace1"), preferences.get("trace2")
            if not restored:
                first = saved_first if saved_first in paths else first
                second = saved_second if saved_second in paths else second
            first = first if first in paths else initial if initial in paths else paths[0]
            second = second if second in paths else first
            return folders, files, files, first, second, f"{len(files)} instrumentation file(s) in {resolved}", True
        except (ValueError, OSError) as exc:
            return no_update, no_update, no_update, no_update, no_update, str(exc), no_update

    @app.callback(Output("filters-page", "style"), Output("plots-page", "style"), Output("timelines-page", "style"), Output("events-page", "style"), Output("agents-page", "style"), Input("page", "value"))
    def page(value):
        return tuple({**CARD, "display": "block" if value == name else "none"}
                     for name in ("filters", "plots", "timelines", "events", "agents"))

    @app.callback(Output("phase-table", "data"), Output("phase-table", "columns"),
                  Input("trace-1", "value"), Input("trace-2", "value"), Input("active-trace", "value"), Input("step-range", "value"))
    def phase_rows(path1, path2, active, step_range):
        path = path1 if active == 1 else path2
        if not path:
            return [], []
        try:
            events, phase_data = scope_steps(get_trace(path), get_phases(path), step_range)
            data = phase_summary(events, phase_data)
            data["id"] = data.Phase
            return data.to_dict("records"), [{"name": c, "id": c} for c in data.columns if c != "id"]
        except ValueError:
            return no_update, no_update
        except Exception:
            return [], []

    @app.callback(Output("phase-table", "selected_rows"), Output("phase-table", "selected_row_ids"),
                  Output("phase-selection-state", "data"),
                  Input("phase-all", "n_clicks"), Input("phase-none", "n_clicks"),
                  Input("phase-pre", "n_clicks"), Input("phase-post", "n_clicks"),
                  Input("phase-table", "data"), State("phase-table", "selected_row_ids"),
                  State("phase-selection-state", "data"))
    def select_phases(all_clicks, none_clicks, pre_clicks, post_clicks, data, selected, state):
        if not data:
            return [], [], None
        clicks = [all_clicks or 0, none_clicks or 0, pre_clicks or 0, post_clicks or 0]
        previous = state.get("clicks") if state else None
        if previous is None:
            chosen = {"transition"}
        elif clicks[0] != previous[0]:
            chosen = set(PHASES)
        elif clicks[1] != previous[1]:
            chosen = set()
        elif clicks[2] != previous[2]:
            chosen = set(PRE_PHASES)
        elif clicks[3] != previous[3]:
            chosen = set(POST_PHASES)
        else:
            chosen = set(selected or [])
        indices = [i for i, row in enumerate(data) if row["id"] in chosen]
        return indices, [data[i]["id"] for i in indices], {"clicks": clicks}

    def total_table(label, row, columns):
        if row is None:
            return html.P(f"{label}: no selected data.")
        return html.Table([
            html.Thead(html.Tr([html.Th(c) for c in columns])),
            html.Tbody(html.Tr([html.Td(label if c in ("Transition", "Phase", "Rank") else
                                        ("—" if pd.isna(row.get(c)) else row.get(c))) for c in columns])),
        ], style={"width": "100%", "textAlign": "left", "background": "#eef5f2", "marginBottom": "12px"})

    @app.callback(Output("phase-total", "children"), Output("transition-total", "children"),
                  Input("trace-1", "value"), Input("trace-2", "value"), Input("active-trace", "value"),
                  Input("phase-table", "selected_row_ids"), Input("transition-table", "selected_row_ids"), Input("step-range", "value"))
    def totals(path1, path2, active, selected_phases, selected_names, step_range):
        path = path1 if active == 1 else path2
        phase_cols = ["Phase", "Mean phase (ms)", "Max phase (ms)", "GC share (%)"]
        transition_cols = ["Transition", "Mean phase (ms)", "Max phase (ms)", "GC share (%)", "Apply sent (MiB)", "Apply received (MiB)", "Mean apply barrier (ms)"]
        if not path:
            return total_table("Selected phases", None, phase_cols), total_table("Selected transitions", None, transition_cols)
        try:
            events, data = scope_steps(get_trace(path), get_phases(path), step_range)
            names = events.loc[events.label == "apply_total", "caller"].drop_duplicates().tolist()
            phase_total = selected_summary(events, names, selected_phases or [], data)
            transition_total = transition_totals(events, selected_names or [], selected_phases or [], data)
            return total_table("Selected phases", phase_total, phase_cols), total_table("Selected transitions", transition_total, transition_cols)
        except ValueError:
            return no_update, no_update
        except Exception as exc:
            return html.P(f"Unable to calculate phase total: {exc}"), html.P(f"Unable to calculate transition total: {exc}")

    @app.callback(Output("transition-table", "data"), Output("transition-table", "columns"), Output("agent-transition", "options"), Output("agent-transition", "value"), Output("agent-type", "options"), Output("agent-type", "value"), Output("load-error", "children"), Input("trace-1", "value"), Input("trace-2", "value"), Input("active-trace", "value"), Input("phase-table", "selected_row_ids"), Input("step-range", "value"), State("agent-transition", "value"), State("agent-type", "value"))
    def load_controls(path1, path2, active, phase, step_range, current_transition=None, current_type=None):
        if not path1 or not path2:
            return [], [], [], None, [], None, "Select two instrumentation traces."
        try:
            first, second = get_trace(path1), get_trace(path2)
            path = path1 if active == 1 else path2
            events = first if active == 1 else second
            filtered_events, filtered_phases = scope_steps(events, get_phases(path), step_range)
            data = transition_summary(filtered_events, _names(first, second), phase or [], filtered_phases)
            data["id"] = data.Transition
            transitions = events[events.label == "transition"]
            nrs = sorted(int(x) for x in transitions.transition_nr.unique())
            types = sorted(str(x) for x in transitions["type"].unique() if str(x))
            transition_value = current_transition if current_transition in nrs else (nrs[0] if nrs else None)
            type_value = current_type if current_type in types else (types[0] if types else None)
            return data.to_dict("records"), [{"name": c, "id": c} for c in data.columns if c != "id"], [{"label": str(n), "value": n} for n in nrs], transition_value, [{"label": t, "value": t} for t in types], type_value, ""
        except ValueError as exc:
            return no_update, no_update, no_update, no_update, no_update, no_update, str(exc)
        except Exception as exc:
            return [], [], [], None, [], None, f"Cannot load trace: {exc}"

    @app.callback(
        Output("transition-table", "selected_rows"), Output("transition-table", "selected_row_ids"),
        Output("selection-state", "data"),
        Input("select-all", "n_clicks"), Input("remove-all", "n_clicks"), Input("transition-table", "data"),
        State("transition-table", "selected_row_ids"), State("trace-1", "value"),
        State("trace-2", "value"), State("selection-state", "data"),
    )
    def select_all(clicks, remove_clicks, data, selected_ids, path1, path2, state):
        if not data or not path1 or not path2:
            return [], [], None
        previous = state or {}
        paths = [path1, path2]
        if previous.get("paths") != paths:
            rows = list(range(len(data)))
        elif previous.get("remove_clicks", 0) != remove_clicks:
            rows = []
        elif previous.get("clicks", 0) != clicks:
            rows = list(range(len(data)))
        else:
            selected = set(selected_ids or [])
            rows = [i for i, row in enumerate(data) if row["id"] in selected]
        return rows, [data[i]["id"] for i in rows], {
            "paths": paths, "clicks": clicks, "remove_clicks": remove_clicks,
        }

    @app.callback(Output("rank-table", "data"), Output("rank-table", "columns"),
                  Input("trace-1", "value"), Input("trace-2", "value"), Input("active-trace", "value"),
                  Input("phase-table", "selected_row_ids"), Input("transition-table", "selected_row_ids"), Input("step-range", "value"))
    def rank_rows(path1, path2, active, selected_phases, selected_names, step_range):
        path = path1 if active == 1 else path2
        if not path:
            return [], []
        try:
            all_events = get_trace(path)
            ranks = pd.DataFrame({"Rank": sorted(int(r) for r in all_events["rank"].unique())})
            events, phase_data = scope_steps(all_events, get_phases(path), step_range)
            calls = events.loc[(events.label == "apply_total") & events.caller.isin(selected_names or []), "transition_nr"].unique()
            rows = phase_data[phase_data.phase.isin(selected_phases or []) & phase_data.transition_nr.isin(calls)]
            summary = rows.groupby("rank", as_index=False).agg(
                _duration=("duration_ms", "sum"), _gc=("gc_ms", lambda s: s.sum(min_count=1)),
                _calls=("transition_nr", "nunique"),
            ).rename(columns={"rank": "Rank"})
            data = ranks.merge(summary, on="Rank", how="left")
            data["_mean"] = data["_duration"] / data["_calls"].replace(0, float("nan"))
            data["Mean phase (ms)"] = data["_mean"].round(2)
            data["GC share (%)"] = (100 * data["_gc"] / data["_duration"].replace(0, float("nan"))).round(2)
            data["id"] = data.Rank
            data = data.astype(object).where(pd.notna(data), None)
            columns = ["Rank", "Mean phase (ms)", "GC share (%)"]
            return data.to_dict("records"), [{"name": c, "id": c} for c in columns]
        except ValueError:
            return no_update, no_update
        except Exception:
            return [], []

    @app.callback(Output("rank-table", "selected_rows"), Output("rank-table", "selected_row_ids"),
                  Output("rank-selection-state", "data"), Input("rank-all", "n_clicks"),
                  Input("rank-none", "n_clicks"), Input("rank-table", "data"),
                  State("rank-table", "selected_row_ids"), State("trace-1", "value"),
                  State("trace-2", "value"), State("active-trace", "value"), State("rank-selection-state", "data"))
    def select_ranks(all_clicks, none_clicks, data, selected, path1, path2, active, state):
        if not data:
            return [], [], None
        previous = state or {}
        path = path1 if active == 1 else path2
        if previous.get("path") != path or previous.get("all", 0) != all_clicks:
            indices = list(range(len(data)))
        elif previous.get("none", 0) != none_clicks:
            indices = []
        else:
            chosen = set(selected or [])
            indices = [i for i, row in enumerate(data) if row["id"] in chosen]
        return indices, [data[i]["id"] for i in indices], {"path": path, "all": all_clicks, "none": none_clicks}

    @app.callback(Output("rank-total", "children"), Input("rank-table", "data"), Input("rank-table", "selected_row_ids"))
    def rank_total(data, selected):
        rows = [row for row in (data or []) if row["id"] in (selected or [])]
        measured = [row for row in rows if row.get("_duration") is not None]
        if not measured:
            return html.P("Selected ranks: no phase data.")
        duration = sum(row["_duration"] for row in measured)
        gc = [row["_gc"] for row in measured if row.get("_gc") is not None]
        share = round(100 * sum(gc) / duration, 2) if duration and len(gc) == len(measured) else None
        mean = sum(row["_mean"] for row in measured) / len(measured)
        return total_table("Selected ranks (mean)", {"Mean phase (ms)": round(mean, 2), "GC share (%)": share},
                           ["Rank", "Mean phase (ms)", "GC share (%)"])

    @app.callback(Output("step-plot", "figure"), Output("phase-plot", "figure"), Input("trace-1", "value"), Input("trace-2", "value"), Input("active-trace", "value"), Input("phase-table", "selected_row_ids"), Input("step-range", "value"), Input("duration-mode", "value"), Input("transition-table", "selected_row_ids"), Input("rank-table", "selected_row_ids"))
    def plots(path1, path2, active, phase, step_range, mode, selected, ranks):
        import plotly.graph_objects as go
        if not path1 or not path2:
            return go.Figure(), go.Figure()
        try:
            first, second = get_trace(path1), get_trace(path2)
            path = path1 if active == 1 else path2
            events = first if active == 1 else second
            selected_phases = phase or []
            all_phases = set(selected_phases) == set(PHASES)
            run_phases = None if all_phases else [get_phases(path1), get_phases(path2)]
            overview = step_figure(
                [(f"Trace 1: {Path(path1).name}", first), (f"Trace 2: {Path(path2).name}", second)],
                selected or [], mode == "cumulative",
                selected_phases=None if all_phases else selected_phases, phase_data=run_phases,
            )
            try:
                bounds = parse_step_range(step_range)
            except ValueError as exc:
                return overview, go.Figure().add_annotation(text=str(exc), showarrow=False)
            selected_steps = None if bounds is None else sorted(
                int(s) for s in events["step"].dropna().unique() if bounds[0] <= s <= bounds[1]
            )
            return overview, phase_figure(events, get_phases(path), selected or [], phase or [], selected_steps, Path(path).name, ranks or [])
        except Exception as exc:
            error = go.Figure().add_annotation(text=f"Unable to render plots: {exc}", showarrow=False)
            return error, error

    def filtered_view(path1, path2, active, names, phase_names, ranks, step_range):
        path = path1 if active == 1 else path2
        if not path:
            raise ValueError("Select an active trace.")
        events = get_trace(path)
        filtered = filter_events(events, names or [], phase_names or [], ranks or [], parse_step_range(step_range))
        keys = filtered[["transition_nr", "rank"]].drop_duplicates()
        apply_events = events[events.label == "apply_total"].merge(keys, on=["transition_nr", "rank"], how="inner")
        return filtered, apply_events

    @app.callback(Output("timeline-event-plot", "figure"), Output("timeline-event-plot-2", "figure"),
                  Input("trace-1", "value"), Input("trace-2", "value"),
                  Input("transition-table", "selected_row_ids"), Input("phase-table", "selected_row_ids"),
                  Input("rank-table", "selected_row_ids"), Input("step-range", "value"))
    def timelines(path1, path2, names, selected_phases, ranks, step_range):
        import plotly.graph_objects as go
        def render(path):
            if not path:
                return go.Figure().add_annotation(text="Select a trace.", showarrow=False)
            try:
                filtered, apply_events = filtered_view(path, None, 1, names, selected_phases, ranks, step_range)
                return event_timeline(filtered, apply_events, Path(path).name)
            except Exception as exc:
                return go.Figure().add_annotation(text=f"Unable to render timeline: {exc}", showarrow=False)
        return render(path1), render(path2)

    @app.callback(Output("events-table", "page_current"),
                  Input("trace-1", "value"), Input("trace-2", "value"), Input("active-trace", "value"),
                  Input("transition-table", "selected_row_ids"), Input("phase-table", "selected_row_ids"),
                  Input("rank-table", "selected_row_ids"), Input("step-range", "value"), prevent_initial_call=True)
    def reset_event_page(*_):
        return 0

    @app.callback(Output("events-table", "data"), Output("events-table", "columns"), Output("events-table", "page_count"), Output("events-status", "children"),
                  Input("trace-1", "value"), Input("trace-2", "value"), Input("active-trace", "value"),
                  Input("transition-table", "selected_row_ids"), Input("phase-table", "selected_row_ids"),
                  Input("rank-table", "selected_row_ids"), Input("step-range", "value"),
                  Input("events-table", "page_current"), Input("events-table", "page_size"), Input("events-table", "sort_by"))
    def events_page(path1, path2, active, names, selected_phases, ranks, step_range, page, size, sort_by):
        try:
            events, _ = filtered_view(path1, path2, active, names, selected_phases, ranks, step_range)
            data = event_table_frame(events)
            count = len(data)
            if sort_by and sort_by[0].get("column_id") in data.columns:
                data = data.sort_values(sort_by[0]["column_id"], ascending=sort_by[0].get("direction") != "desc", kind="stable")
            else:
                data = data.sort_values(["transition_nr", "rank", "start_rel_ms"], kind="stable")
            numeric_columns = {c for c in data.columns if pd.api.types.is_numeric_dtype(data[c])}
            exact_text_columns = {
                c for c in data.columns
                if c.endswith("_ns") or c in ("agent_sum_sq_hi", "agent_sum_sq_lo")
                or (pd.api.types.is_integer_dtype(data[c]) and
                    (data[c].gt(2**53 - 1).any() or data[c].lt(-(2**53 - 1)).any()))
            }
            size = max(1, int(size or 50))
            data = data.iloc[max(0, int(page or 0)) * size : (max(0, int(page or 0)) + 1) * size].copy()
            for column in exact_text_columns:
                data[column] = data[column].map(lambda v: f"{int(v):,}" if pd.notna(v) else None)
            data = data.astype(object).where(pd.notna(data), None)
            columns = []
            for column in data.columns:
                config = {"name": column, "id": column}
                if column in numeric_columns and column not in exact_text_columns:
                    fmt = Format(group=Group.yes, group_delimiter=",")
                    if column in ("start_rel_ms", "end_rel_ms", "duration_ms", "gc_pause_ms"):
                        fmt = Format(group=Group.yes, group_delimiter=",", precision=6, scheme=Scheme.fixed)
                    config.update(type="numeric", format=fmt)
                columns.append(config)
            return data.to_dict("records"), columns, (count + size - 1) // size, f"{count:,} matching events"
        except Exception as exc:
            return [], [], 0, f"Unable to load events: {exc}"

    @app.callback(Output("agent-plot", "figure"), Input("trace-1", "value"), Input("trace-2", "value"), Input("active-trace", "value"), Input("agent-transition", "value"), Input("agent-type", "value"), Input("rank-table", "selected_row_ids"))
    def agent_plot(path1, path2, active, transition, agent_type, ranks):
        import plotly.graph_objects as go
        path = path1 if active == 1 else path2
        if not path:
            return go.Figure()
        try:
            events = get_trace(path)
            rows = events[(events.label == "transition") & (events.transition_nr == transition) & (events["type"] == agent_type) & events["rank"].isin(ranks or [])].sort_values("rank")
            fig = go.Figure(go.Bar(x=rows["rank"], y=rows["items"], name="Live agents"))
            fig.update_layout(title=f"Live agents — {agent_type} — {Path(path).name}", xaxis_title="Rank", yaxis_title="Live agents")
            return fig
        except Exception as exc:
            return go.Figure().add_annotation(text=f"Unable to render agent distribution: {exc}", showarrow=False)
    return app
