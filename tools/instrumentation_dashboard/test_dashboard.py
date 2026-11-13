"""Regression tests for multi-run dashboard and vectorized reader."""
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest
from unittest.mock import patch

import h5py
import numpy as np
import pandas as pd

from analysis import PHASES, PRE_PHASES, POST_PHASES, phases, step_figure, phase_figure, selected_summary, transition_summary, transition_totals
from dashboard import create_dashboard, get_trace, parse_step_range, scan_directory, scope_steps
from event_views import event_table_frame, event_timeline, filter_events
from fast_reader import load_trace
from hdf5_reader import load_instrumentation_hdf5
from dash import no_update

SAMPLE = Path(__file__).resolve().parents[2] / "h5" / "InstrumentationExample.h5"


class DashboardTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.events = load_trace(SAMPLE)

    def test_vector_reader_matches_legacy(self):
        old = load_instrumentation_hdf5(SAMPLE)
        for column in ["label", "caller", "type", "context", "kind", "rank", "transition_nr", "dur_ns", "items", "send_bytes"]:
            self.assertEqual(self.events[column].tolist(), old[column].tolist(), column)

    def test_directory_scan(self):
        folders, files, resolved = scan_directory(str(SAMPLE.parent))
        self.assertIn(str(SAMPLE.resolve()), [f["value"] for f in files])
        self.assertTrue(folders)
        self.assertEqual(resolved, str(SAMPLE.parent.resolve()))

    def test_cache_reuses_data(self):
        self.assertIs(get_trace(str(SAMPLE)), get_trace(str(SAMPLE)))

    def test_grouped_rank_files_and_cache_invalidation(self):
        with tempfile.TemporaryDirectory() as temporary:
            base = Path(temporary) / "run"
            with h5py.File(SAMPLE, "r") as source:
                source_group = source["instrumentation"]
                data = source_group["events"][:]
                for rank in (0, 1, 2):
                    part = data[data["rank"] == rank]
                    with h5py.File(f"{base}_{rank}.h5", "w") as target:
                        group = target.create_group("instrumentation")
                        for key, value in source_group.attrs.items():
                            group.attrs[key] = value
                        group.attrs["n_events_per_rank"] = [len(part)]
                        group.attrs["displace"] = [0]
                        group.create_dataset("events", data=part)
            _, options, _ = scan_directory(temporary)
            self.assertIn(str(base), [item["value"] for item in options])
            first = get_trace(str(base))
            self.assertEqual(len(first), len(self.events))
            with h5py.File(f"{base}_2.h5", "a") as target:
                target.attrs["changed"] = True
            self.assertIsNot(first, get_trace(str(base)))

    def test_phases_and_summary(self):
        data = phases(self.events)
        self.assertIn("transition", set(data.phase))
        names = sorted(self.events.loc[self.events.label == "transition", "caller"].unique())
        summary = transition_summary(self.events, names, "transition", data)
        self.assertEqual(summary.Transition.tolist(), names)
        self.assertTrue((summary.Calls > 0).all())

    def test_selected_summary_recomputes_metrics(self):
        phase_data = phases(self.events)
        names = self.events.loc[self.events.label == "apply_total", "caller"].unique().tolist()
        selected = ["transition", "prepare_read"]
        total = selected_summary(self.events, names, selected, phase_data)
        ranked = phase_data[phase_data.phase.isin(selected)].groupby(["transition_nr", "rank"]).duration_ms.sum()
        calls = ranked.groupby("transition_nr").max()
        self.assertEqual(total["Calls"], len(calls))
        self.assertAlmostEqual(total["Mean phase (ms)"], round(calls.mean(), 2))
        self.assertAlmostEqual(total["Max phase (ms)"], round(calls.max(), 2))
        self.assertIsNone(selected_summary(self.events, [], selected, phase_data))
        self.assertIsNone(selected_summary(self.events, names, [], phase_data))

    def test_transition_mean_apply_barrier(self):
        events = self.events.copy()
        phase_data = phases(events)
        names = sorted(events.loc[events.label == "apply_total", "caller"].unique())
        summary = transition_summary(events, names, ["transition"], phase_data).set_index("Transition")
        calls = events.loc[events.label == "apply_total", ["transition_nr", "rank", "caller"]].drop_duplicates()
        barriers = events[(events.context == "apply") & events.label.str.startswith("barrier")]
        per_rank = barriers.groupby(["transition_nr", "rank"], as_index=False).duration_ms.sum()
        expected = calls.merge(per_rank, on=["transition_nr", "rank"], how="left").fillna({"duration_ms": 0})
        for name, group in expected.groupby("caller"):
            self.assertEqual(summary.loc[name, "Mean apply barrier (ms)"], round(group.duration_ms.mean(), 2))
        self.assertIsNotNone(transition_summary(events, names, [], phase_data).iloc[0]["Mean apply barrier (ms)"])
        without_barriers = events[~events.label.str.startswith("barrier")]
        zero = transition_summary(without_barriers, names, ["transition"], phase_data)
        self.assertTrue((zero["Mean apply barrier (ms)"] == 0).all())

    def test_transition_totals_sum_displayed_times(self):
        original = self.events.copy()
        duplicate = original.copy()
        duplicate["transition_nr"] += 100
        duplicate["caller"] = "other transition"
        events = pd.concat([original, duplicate], ignore_index=True)
        phase_data = phases(events)
        names = events.loc[events.label == "transition", "caller"].unique().tolist()
        rows = transition_summary(events, names, ["transition"], phase_data)
        total = transition_totals(events, names, ["transition"], phase_data)
        self.assertAlmostEqual(total["Mean phase (ms)"], rows["Mean phase (ms)"].sum())
        self.assertAlmostEqual(total["Max phase (ms)"], rows["Max phase (ms)"].sum())
        self.assertAlmostEqual(total["Mean apply barrier (ms)"], rows["Mean apply barrier (ms)"].sum())
        self.assertIsNone(transition_totals(events, [], ["transition"], phase_data))

    def test_step_scope_and_table_callbacks(self):
        events = self.events.copy()
        events["step"] = events["transition_nr"]
        data = phases(events)
        scoped, scoped_phases = scope_steps(events, data, "1")
        self.assertEqual(set(scoped.transition_nr), {1})
        self.assertEqual(set(scoped_phases.transition_nr), {1})
        self.assertEqual(len(scope_steps(events, data, "all")[0]), len(events))
        empty, empty_phases = scope_steps(events, data, "999-1000")
        self.assertTrue(empty.empty and empty_phases.empty)
        app = create_dashboard(SAMPLE.parent, SAMPLE)
        callbacks = {k: v["callback"].__wrapped__ for k, v in app.callback_map.items()}
        controls = next(v for k, v in callbacks.items() if "transition-table.data" in k)
        phase_rows = next(v for k, v in callbacks.items() if "phase-table.data" in k)
        rank_rows = next(v for k, v in callbacks.items() if "rank-table.data" in k)
        totals = next(v for k, v in callbacks.items() if "transition-total.children" in k)
        names = sorted(events.loc[events.label == "transition", "caller"].unique())
        with patch("dashboard.get_trace", return_value=events), patch("dashboard.get_phases", return_value=data):
            args = (str(SAMPLE), str(SAMPLE), 1)
            all_rows = controls(*args, ["transition"], "all")[0]
            step_rows = controls(*args, ["transition"], "1")[0]
            self.assertEqual(sum(r["Calls"] for r in all_rows), 2)
            self.assertEqual(sum(r["Calls"] for r in step_rows), 1)
            phase_all = phase_rows(*args, "all")[0]
            phase_one = phase_rows(*args, "1")[0]
            self.assertEqual(next(r for r in phase_all if r["Phase"] == "transition")["Calls"], 2)
            self.assertEqual(next(r for r in phase_one if r["Phase"] == "transition")["Calls"], 1)
            rank_all = rank_rows(*args, ["transition"], names, "all")[0]
            rank_one = rank_rows(*args, ["transition"], names, "1")[0]
            self.assertTrue(all(a["_duration"] >= b["_duration"] for a, b in zip(rank_all, rank_one)))
            for row in rank_all:
                self.assertEqual(row["_calls"], 2)
                self.assertAlmostEqual(row["_mean"], row["_duration"] / 2)
                self.assertEqual(row["Mean phase (ms)"], round(row["_mean"], 2))
            rank_total = callbacks["rank-total.children"]
            summary = rank_total(rank_all, [0, 1, 2])
            cells = summary.children[1].children.children
            self.assertEqual(cells[1].children, round(sum(r["_mean"] for r in rank_all) / 3, 2))
            phase_total, transition_total = totals(*args, ["transition"], names, "1")
            self.assertEqual(phase_total.__class__.__name__, "Table")
            self.assertEqual(transition_total.__class__.__name__, "Table")
            no_matches = controls(*args, ["transition"], "999")[0]
            self.assertTrue(all(r["Calls"] is None or pd.isna(r["Calls"]) for r in no_matches))
            self.assertIs(phase_rows(*args, "bad")[0], no_update)
            self.assertIs(rank_rows(*args, ["transition"], names, "bad")[0], no_update)
            self.assertIs(controls(*args, ["transition"], "bad")[0], no_update)
            preserved = controls(*args, ["transition"], "1", 2, "A1")
            self.assertEqual(preserved[3], 2)
            self.assertEqual(preserved[5], "A1")

    def test_two_run_step_plot_and_cumulative(self):
        data = self.events.copy()
        data.loc[:, "step"] = data.transition_nr
        names = sorted(data.loc[data.label == "transition", "caller"].unique())
        per = step_figure([("one", data), ("two", data)], names, False)
        cumulative = step_figure([("one", data), ("two", data)], names, True)
        self.assertEqual(len(per.data), 2)
        np.testing.assert_allclose(np.cumsum(per.data[0].y), cumulative.data[0].y)
        self.assertEqual(len(step_figure([("one", data)], [], False).data[0].x), 0)

    def test_step_plot_phase_selection_uses_max_after_rank_sum(self):
        events = self.events.copy()
        events["step"] = events.transition_nr
        phase_data = phases(events)
        names = sorted(events.loc[events.label == "transition", "caller"].unique())
        chosen = ["transition", "prepare_read"]
        fig = step_figure([("one", events), ("two", events)], names, False, chosen, [phase_data, phase_data])
        selected = phase_data[phase_data.phase.isin(chosen)]
        ranked = selected.groupby(["transition_nr", "rank"]).duration_ms.sum()
        expected = ranked.groupby("transition_nr").max().sort_index().to_numpy()
        np.testing.assert_allclose(fig.data[0].y, expected)
        self.assertIn("Selected phase duration", fig.layout.title.text)
        cumulative = step_figure([("one", events)], names, True, chosen, [phase_data])
        np.testing.assert_allclose(cumulative.data[0].y, np.cumsum(expected))
        empty = step_figure([("one", events)], names, False, [], [phase_data])
        self.assertEqual(len(empty.data[0].x), 0)

    def test_step_range_validation_and_filter(self):
        self.assertIsNone(parse_step_range("  "))
        self.assertIsNone(parse_step_range("all"))
        self.assertIsNone(parse_step_range(" ALL "))
        self.assertEqual(parse_step_range("5-7"), (5, 7))
        self.assertEqual(parse_step_range(" 5 - 7 "), (5, 7))
        self.assertEqual(parse_step_range("5"), (5, 5))
        self.assertEqual(parse_step_range("-2--1"), (-2, -1))
        for invalid in ("7-5", "5-", "5,7", "hello"):
            with self.assertRaises(ValueError):
                parse_step_range(invalid)
        data = self.events.copy()
        data.loc[:, "step"] = data.transition_nr
        names = sorted(data.loc[data.label == "transition", "caller"].unique())
        fig = phase_figure(data, phases(data), names, "transition", [1, 2], "test")
        self.assertGreater(len(fig.layout.xaxis.ticktext), 0)
        self.assertTrue(all(str(x).startswith(("1 · ", "2 · ")) for x in fig.layout.xaxis.ticktext))

    def test_step_filter_labels(self):
        data = self.events.copy()
        data.loc[:, "step"] = data.transition_nr
        names = sorted(data.loc[data.label == "transition", "caller"].unique())
        fig = phase_figure(data, phases(data), names, "transition", 1, "test")
        self.assertEqual(fig.layout.xaxis.tickangle, -45)
        self.assertTrue(fig.layout.xaxis.ticktext)

    def test_event_table_columns_and_relative_start(self):
        events = self.events.iloc[:8].copy()
        events["start_ns"] = np.arange(len(events), dtype=np.int64) * 1_000_000 + 8_000_000_000_000_000_000
        frame = event_table_frame(events)
        self.assertEqual(frame.columns[0], "rank")
        self.assertNotIn("source_file", frame.columns)
        self.assertNotIn("dur_ns", frame.columns)
        self.assertNotIn("start_ns", frame.columns)
        self.assertEqual(frame.columns.get_loc("end_rel_ms"), frame.columns.get_loc("start_rel_ms") + 1)
        self.assertEqual(frame.columns.get_loc("items"), frame.columns.get_loc("duration_ms") + 1)
        self.assertEqual(frame.start_rel_ms.tolist(), list(range(8)))
        np.testing.assert_allclose(frame.end_rel_ms, frame.start_rel_ms + frame.duration_ms)
        agent_columns = [c for c in frame if c.startswith("agent_")]
        self.assertEqual(frame.columns[-len(agent_columns):].tolist(), agent_columns)
        self.assertIn("gc_pause_ms", frame.columns)
        self.assertEqual(event_table_frame(events.iloc[0:0]).columns.tolist(), frame.columns.tolist())

    def test_event_filter_selected_phase_includes_nested_events(self):
        def row(label, rank, start, duration, step=1, caller="move", nr=1):
            return {"label": label, "rank": rank, "start_ns": start, "dur_ns": duration,
                    "step": step, "caller": caller, "transition_nr": nr,
                    "context": "apply", "duration_ms": duration / 1e6}
        events = pd.DataFrame([
            row("apply_total", 0, 0, 100), row("transition", 0, 10, 40),
            row("nested", 0, 15, 5), row("barrier_post_apply", 0, 70, 10),
            row("apply_total", 1, 0, 100), row("transition", 1, 30, 30),
            row("nested", 1, 15, 5), row("apply_total", 0, 200, 100, step=2, caller="other", nr=2),
            row("transition", 0, 210, 30, step=2, caller="other", nr=2),
            {**row("distribute", 0, -10, 5, step=0, caller="", nr=0), "context": "init"},
        ])
        selected = filter_events(events, ["move"], ["transition"], [0, 1], (1, 1))
        self.assertEqual(selected.label.tolist(), ["transition", "nested", "transition"])
        self.assertEqual(selected['rank'].tolist(), [0, 0, 1])
        self.assertEqual(len(filter_events(events, ["move"], PHASES, [0], None)), 4)
        self.assertTrue(filter_events(events, ["move"], [], [0], None).empty)
        self.assertTrue(filter_events(events, ["move"], PHASES, [], None).empty)
        self.assertTrue(filter_events(events, ["move"], PHASES, [0], (2, 2)).empty)
        all_events = filter_events(events, ["move", "other"], PHASES, [0, 1], None)
        self.assertEqual(len(all_events), len(events))
        self.assertIn("distribute", all_events.label.tolist())
        self.assertNotIn("distribute", filter_events(events, ["move", "other"], ["transition"], [0, 1], None).label.tolist())
        applies = events[(events.label == "apply_total") & (events.transition_nr == 1)]
        timeline = event_timeline(selected, applies)
        self.assertTrue(any(trace.name == "apply_total (context)" for trace in timeline.data))
        self.assertTrue(any(trace.name == "nested" for trace in timeline.data))
        self.assertFalse(any(trace.name == "barrier_post_apply" for trace in timeline.data))

    def test_timeline_nested_intervals(self):
        def row(label, rank, start, duration):
            return {"label": label, "rank": rank, "start_ns": start, "dur_ns": duration,
                    "duration_ms": duration / 1e6, "transition_nr": 1, "step": 1,
                    "caller": "move", "context": "apply"}

        applies = pd.DataFrame([row("apply_total", rank, 0, 1000) for rank in (0, 1)])
        events = pd.DataFrame([
            row("outer", 0, 100, 800), row("inner", 0, 200, 500),
            row("leaf", 0, 300, 1), row("sibling", 0, 700, 100),
            row("inner", 1, 200, 500), row("outer", 1, 100, 800),
        ])
        events["send_bytes"] = 0
        events["send_items"] = 0
        events.loc[events.label == "leaf", ["send_bytes", "send_items"]] = [2048, 16]
        fig = event_timeline(events, applies)
        traces = {trace.name: trace for trace in fig.data}
        self.assertGreater(traces["outer"].width, traces["inner"].width)
        self.assertGreater(traces["inner"].width, traces["leaf"].width)
        self.assertEqual(traces["inner"].width, traces["sibling"].width)
        self.assertEqual(traces["leaf"].type, "bar")
        self.assertEqual(traces["leaf"].hovertext[0], "<br>GC: unavailable<br>Sent: 2,048 bytes (16 items)")
        self.assertEqual(traces["outer"].hovertext[0], "<br>GC: unavailable")
        self.assertIn("%{hovertext}", traces["leaf"].hovertemplate)
        self.assertEqual(list(traces["inner"].y), ["Rank 0", "Rank 1"])
        self.assertLess(list(traces).index("outer"), list(traces).index("inner"))
        legend = sorted(fig.data, key=lambda trace: trace.legendrank)
        self.assertEqual([trace.name for trace in legend],
                         ["apply_total (context)", "outer", "inner", "leaf", "sibling"])

    def test_timeline_gc_overlay(self):
        def row(label, start, duration, gc):
            return {"label": label, "rank": 0, "start_ns": start, "dur_ns": duration,
                    "duration_ms": duration / 1e6, "gc_pause_ns": gc,
                    "transition_nr": 1, "step": 1, "caller": "move", "context": "apply"}
        applies = pd.DataFrame([row("apply_total", 0, 10_000_000, 2_000_000)])
        events = pd.DataFrame([row("outer", 1_000_000, 8_000_000, 3_000_000),
                               row("inner", 2_000_000, 2_000_000, 1_000_000),
                               row("missing", 5_000_000, 1_000_000, np.nan),
                               row("zero", 6_000_000, 1_000_000, 0)])
        fig = event_timeline(events, applies)
        overlays = [trace for trace in fig.data if trace.meta == "gc_overlay"]
        self.assertEqual([trace.name for trace in overlays], ["apply_total (context) GC", "outer GC", "inner GC"])
        self.assertEqual([trace.x[0] for trace in overlays], [2, 3, 1])
        self.assertEqual([trace.base[0] for trace in overlays], [8, 6, 3])
        self.assertTrue(all(trace.marker.pattern.shape == "/" for trace in overlays))
        traces = {trace.name: trace for trace in fig.data if trace.meta != "gc_overlay"}
        self.assertIsNone(traces["missing"].customdata[0][8])
        self.assertEqual(traces["zero"].customdata[0][8], 0)
        self.assertEqual(traces["inner"].customdata[0][8], 1)
        self.assertLess(list(fig.data).index(traces["outer"]), list(fig.data).index(traces["inner"]))

    def test_timeline_multiple_transitions(self):
        def row(label, rank, start, duration, step, nr):
            return {"label": label, "rank": rank, "start_ns": start, "dur_ns": duration,
                    "duration_ms": duration / 1e6, "transition_nr": nr, "step": step,
                    "caller": f"call{nr}", "context": "apply"}
        applies = pd.DataFrame([row("apply_total", 0, 100, 100, 1, 1),
                                row("apply_total", 0, 400, 100, 2, 2),
                                row("apply_total", 1, 900, 100, 2, 2)])
        events = pd.DataFrame([row("work", 0, 110, 20, 1, 1),
                               row("work", 0, 410, 20, 2, 2),
                               row("work", 1, 910, 20, 2, 2),
                               row("work", 0, 800, 10, 3, 3)])
        fig = event_timeline(events, applies)
        context, work = fig.data
        self.assertEqual(list(context.base), [0.0, 0.0003, 0.0])
        self.assertEqual(list(work.base), [0.00001, 0.00031, 0.00001])
        self.assertEqual([row[6] for row in work.customdata], [1, 2, 2])
        self.assertEqual([row[5] for row in work.customdata], [1, 2, 2])
        self.assertIn("first selected apply", fig.layout.xaxis.title.text)
        self.assertFalse(event_timeline(events, applies.iloc[0:0]).data)

    @unittest.skipUnless(shutil.which("node"), "Node.js is required for the JavaScript hit test")
    def test_timeline_pointer_hit_testing(self):
        script = r'''
            const assert = require("assert");
            const hit = require("./assets/timeline_hover.js");
            const x = {_offset: 0, d2p: v => v};
            const y = {_offset: 0, d2c: () => 0, l2p: v => v * 100};
            const traces = [
                {type:"bar", name:"apply", y:["Rank 0"], x:[100], base:0, width:.85},
                {type:"bar", name:"outer", y:["Rank 0"], x:[80], base:[10], width:.65, customdata:[[10,80,1,0,0]]},
                {type:"bar", name:"inner", y:["Rank 0"], x:[20], base:[20], width:.4, marker:{color:"#abcdef"}, customdata:[[20,20,2,128,2]]}
            ];
            assert.equal(hit(traces,x,y,25,0).name, "inner");
            assert.equal(hit(traces,x,y,25,0).color, "#abcdef");
            traces.push({type:"bar", name:"overlay", meta:"gc_overlay", y:["Rank 0"], x:[20], base:[20], width:.4, customdata:[[20,20,99,0,0]]});
            assert.equal(hit(traces,x,y,25,0).name, "inner");
            assert.deepEqual(hit(traces,x,y,25,0).path, ["apply", "outer", "inner"]);
            assert.equal(hit(traces,x,y,25,25).name, "outer");
            assert.equal(hit(traces,x,y,5,0).name, "apply");
            assert.equal(hit(traces,x,y,101,0), null);
            traces[2].visible = "legendonly";
            assert.equal(hit(traces,x,y,25,0).name, "outer");
            assert.deepEqual(hit(traces,x,y,25,0).path, ["apply", "outer"]);
            traces.push({type:"bar", name:"tiny", y:["Rank 0"], x:[.01], base:[60], width:.4, customdata:[[60,.01,2,0,0]]});
            assert.equal(hit(traces,x,y,61,0).name, "tiny");
        '''
        subprocess.run(["node", "-e", script], cwd=Path(__file__).parent, check=True)

    def test_app_endpoints_and_navigation(self):
        app = create_dashboard(SAMPLE.parent, SAMPLE)
        client = app.server.test_client()
        self.assertEqual(client.get('/').status_code, 200)
        response = client.get('/_dash-layout')
        self.assertEqual(response.status_code, 200)
        body = response.get_data(as_text=True)
        layout = response.get_json()
        def components(node):
            if isinstance(node, dict):
                yield node
                for value in node.values():
                    yield from components(value)
            elif isinstance(node, list):
                for value in node:
                    yield from components(value)
        props = {n["props"]["id"]: n["props"] for n in components(layout) if isinstance(n.get("props"), dict) and "id" in n["props"]}
        for component in ("directory", "trace-1", "trace-2", "active-trace"):
            self.assertTrue(props[component]["persistence"])
            self.assertEqual(props[component]["persistence_type"], "local")
        for text in ["Filters", "Plots", "Timelines", "Events", "Agents", "Trace 1", "Trace 2", "Select all", "Duration over steps"]:
            self.assertIn(text, body)
        self.assertGreaterEqual(len(app.callback_map), 6)
        callbacks = {key: value["callback"].__wrapped__ for key, value in app.callback_map.items()}
        totals = next(v for k, v in callbacks.items() if "transition-total.children" in k)
        names_for_total = sorted(self.events.loc[self.events.label == "transition", "caller"].unique())
        _, transition_total = totals(str(SAMPLE), str(SAMPLE), 1, ["transition"], names_for_total, "all")
        self.assertNotIn("Calls", str(transition_total.to_plotly_json()))
        page = next(v for k, v in callbacks.items() if "filters-page.style" in k)
        self.assertEqual([style["display"] for style in page("filters")], ["block", "none", "none", "none", "none"])
        self.assertEqual([style["display"] for style in page("plots")], ["none", "block", "none", "none", "none"])
        self.assertEqual([style["display"] for style in page("timelines")], ["none", "none", "block", "none", "none"])
        self.assertEqual([style["display"] for style in page("events")], ["none", "none", "none", "block", "none"])
        self.assertEqual([style["display"] for style in page("agents")], ["none", "none", "none", "none", "block"])
        scan = next(v for k, v in callbacks.items() if "directories.options" in k)
        result = scan(str(SAMPLE.parent), 0, str(SAMPLE), str(SAMPLE), None, None, False)
        self.assertEqual(result[3:5], (str(SAMPLE), str(SAMPLE)))
        saved = {"directory": str(SAMPLE.parent), "trace1": str(SAMPLE), "trace2": str(SAMPLE)}
        result = scan(str(SAMPLE.parent), 0, None, None, None, saved, False)
        self.assertEqual(result[3:5], (str(SAMPLE), str(SAMPLE)))
        with tempfile.TemporaryDirectory() as temporary:
            result = scan(temporary, 0, str(SAMPLE), str(SAMPLE), None, None, False)
            self.assertIs(result[3], no_update)
            self.assertIs(result[4], no_update)
            first_path, second_path = Path(temporary) / "first.h5", Path(temporary) / "second.h5"
            shutil.copyfile(SAMPLE, first_path)
            shutil.copyfile(SAMPLE, second_path)
            preferences = {"directory": temporary, "trace1": str(second_path), "trace2": str(first_path)}
            result = scan(temporary, 0, str(first_path), str(first_path), str(first_path), preferences, False)
            self.assertEqual(result[3:5], (str(second_path), str(first_path)))
            result = scan(temporary, 1, str(first_path), str(second_path), str(first_path), preferences, True)
            self.assertEqual(result[3:5], (str(first_path), str(second_path)))
            remember = callbacks["trace-preferences.data"]
            self.assertEqual(remember(temporary, str(second_path), str(first_path), True), preferences)
        phase_rows = next(v for k, v in callbacks.items() if "phase-table.data" in k)
        phase_data, phase_columns = phase_rows(str(SAMPLE), str(SAMPLE), 1, "all")
        self.assertEqual([r["Phase"] for r in phase_data], PHASES)
        self.assertNotIn("pre_transition", [r["Phase"] for r in phase_data])
        phase_select = next(v for k, v in callbacks.items() if "phase-table.selected_rows" in k)
        indices, selected, state = phase_select(0, 0, 0, 0, phase_data, [], None)
        self.assertEqual(selected, ["transition"])
        indices, selected, state = phase_select(0, 0, 1, 0, phase_data, selected, state)
        self.assertEqual(selected, PRE_PHASES)
        indices, selected, state = phase_select(0, 0, 1, 1, phase_data, selected, state)
        self.assertEqual(selected, POST_PHASES)
        indices, selected, state = phase_select(1, 0, 1, 1, phase_data, selected, state)
        self.assertEqual(selected, PHASES)
        indices, selected, state = phase_select(1, 1, 1, 1, phase_data, selected, state)
        self.assertEqual(selected, [])
        controls = next(v for k, v in callbacks.items() if "transition-table.data" in k)
        rows, columns, transitions, nr, types, agent_type, error = controls(str(SAMPLE), str(SAMPLE), 1, ["transition"], "all")
        self.assertFalse(error)
        self.assertTrue(rows)
        select = next(v for k, v in callbacks.items() if "transition-table.selected_rows" in k)
        selected_rows, names, state = select(0, 0, rows, [], str(SAMPLE), str(SAMPLE), None)
        self.assertEqual(selected_rows, list(range(len(rows))))
        self.assertEqual(set(names), {row["Transition"] for row in rows})
        subset = names[:1]
        preserved, preserved_ids, state = select(0, 0, rows, subset, str(SAMPLE), str(SAMPLE), state)
        self.assertEqual(preserved, [0])
        self.assertEqual(preserved_ids, subset)
        all_rows, all_ids, state = select(1, 0, rows, subset, str(SAMPLE), str(SAMPLE), state)
        self.assertEqual(all_rows, list(range(len(rows))))
        self.assertEqual(all_ids, names)
        cleared, cleared_ids, state = select(1, 1, rows, all_ids, str(SAMPLE), str(SAMPLE), state)
        self.assertEqual(cleared, [])
        self.assertEqual(cleared_ids, [])
        restored, restored_ids, _ = select(2, 1, rows, [], str(SAMPLE), str(SAMPLE), state)
        self.assertEqual(restored, list(range(len(rows))))
        self.assertEqual(restored_ids, names)
        plot = next(v for k, v in callbacks.items() if "step-plot.figure" in k)
        first, second = plot(str(SAMPLE), str(SAMPLE), 1, ["transition"], "all", "step", names, [0, 1, 2])
        self.assertEqual(len(first.data), 2)
        self.assertIn("Selected phase duration", first.layout.title.text)
        apply_plot, _ = plot(str(SAMPLE), str(SAMPLE), 1, PHASES, "all", "step", names, [0, 1, 2])
        self.assertIn("Apply duration", apply_plot.layout.title.text)
        expected_apply = step_figure([("one", self.events)], names, False)
        np.testing.assert_allclose(apply_plot.data[0].y, expected_apply.data[0].y)
        self.assertTrue(second.data)
        agent = callbacks["agent-plot.figure"]
        self.assertTrue(agent(str(SAMPLE), str(SAMPLE), 1, nr, agent_type, [0, 1, 2]).data)
        events_page = next(v for k, v in callbacks.items() if "events-table.data" in k)
        table, event_columns, pages, status = events_page(str(SAMPLE), str(SAMPLE), 1, names, PHASES, [0, 1, 2], "all", 0, 10, [])
        self.assertEqual(len(table), 10)
        self.assertEqual(pages, 15)
        self.assertIn("144", status)
        self.assertIsInstance(table[0]["start_rel_ms"], float)
        self.assertIsInstance(table[0]["agent_sum_ns"], str)
        self.assertAlmostEqual(table[0]["end_rel_ms"], table[0]["start_rel_ms"] + table[0]["duration_ms"])
        self.assertEqual([c["id"] for c in event_columns].index("end_rel_ms"), [c["id"] for c in event_columns].index("start_rel_ms") + 1)
        items_column = next(c for c in event_columns if c["id"] == "items")
        self.assertEqual(items_column["type"], "numeric")
        self.assertEqual(items_column["format"].to_plotly_json()["locale"]["group"], ",")
        self.assertNotIn("source_file", table[0])
        self.assertNotIn("dur_ns", table[0])
        ordered, _, _, _ = events_page(str(SAMPLE), str(SAMPLE), 1, names, PHASES, [0, 1, 2], "all", 0, 10, [{"column_id": "duration_ms", "direction": "desc"}])
        self.assertGreaterEqual(ordered[0]["duration_ms"], ordered[-1]["duration_ms"])
        earliest, _, _, _ = events_page(str(SAMPLE), str(SAMPLE), 1, names, PHASES, [0, 1, 2], "all", 0, 10, [{"column_id": "start_rel_ms", "direction": "asc"}])
        self.assertEqual(earliest[0]["start_rel_ms"], 0.0)
        next_page, _, _, _ = events_page(str(SAMPLE), str(SAMPLE), 1, names, PHASES, [0, 1, 2], "all", 1, 10, [])
        self.assertNotEqual(table[0], next_page[0])
        large = self.events.copy()
        large["agent_sum_ns"] = 1_000_000_000_000_001
        large["items"] = 1_000_000
        with patch("dashboard.get_trace", return_value=large):
            formatted, _, _, _ = events_page(str(SAMPLE), str(SAMPLE), 1, names, PHASES, [0, 1, 2], "all", 0, 10, [])
            self.assertEqual(formatted[0]["agent_sum_ns"], "1,000,000,000,000,001")
            self.assertEqual(formatted[0]["items"], 1_000_000)
        subset, _, _, _ = events_page(str(SAMPLE), str(SAMPLE), 1, names, ["transition"], [0], "all", 0, 50, [])
        self.assertTrue(subset)
        self.assertTrue(all(row["rank"] == 0 and row["label"] == "transition" for row in subset))
        self.assertFalse(any("timeline-step" in k or "timeline-transition" in k for k in callbacks))
        timelines = next(v for k, v in callbacks.items() if "timeline-event-plot.figure" in k)
        event_fig, second_fig = timelines(str(SAMPLE), str(SAMPLE), names, PHASES, [0, 1, 2], "all")
        self.assertTrue(event_fig.data)
        self.assertTrue(second_fig.data)
        self.assertIn(SAMPLE.name, second_fig.layout.title.text)
        first_only, missing = timelines(str(SAMPLE), None, names, PHASES, [0, 1, 2], "all")
        self.assertTrue(first_only.data)
        self.assertIn("Select a trace", missing.layout.annotations[0].text)
        first_ok, second_error = timelines(str(SAMPLE), "/missing.h5", names, PHASES, [0, 1, 2], "all")
        self.assertTrue(first_ok.data)
        self.assertIn("Unable to render", second_error.layout.annotations[0].text)
        rank_rows = next(v for k, v in callbacks.items() if "rank-table.data" in k)
        rank_data, rank_columns = rank_rows(str(SAMPLE), str(SAMPLE), 1, ["transition"], names, "all")
        self.assertEqual([row["Rank"] for row in rank_data], [0, 1, 2])
        rank_select = next(v for k, v in callbacks.items() if "rank-table.selected_rows" in k)
        indexes, selected_ranks, state = rank_select(0, 0, rank_data, [], str(SAMPLE), str(SAMPLE), 1, None)
        self.assertEqual(selected_ranks, [0, 1, 2])
        indexes, selected_ranks, state = rank_select(0, 1, rank_data, selected_ranks, str(SAMPLE), str(SAMPLE), 1, state)
        self.assertEqual(selected_ranks, [])
        indexes, selected_ranks, state = rank_select(1, 1, rank_data, [], str(SAMPLE), str(SAMPLE), 1, state)
        self.assertEqual(selected_ranks, [0, 1, 2])
        _, filtered = plot(str(SAMPLE), str(SAMPLE), 1, ["transition"], "all", "step", names, [1])
        self.assertTrue(filtered.data)
        self.assertTrue(all(t.name.startswith("Rank 1") for t in filtered.data))
        unfiltered_steps, empty_ranks = plot(str(SAMPLE), str(SAMPLE), 1, ["transition"], "all", "step", names, [])
        self.assertFalse(empty_ranks.data)
        np.testing.assert_allclose(first.data[0].y, unfiltered_steps.data[0].y)
        overview, invalid_phase = plot(str(SAMPLE), str(SAMPLE), 1, ["transition"], "7-5", "step", names, [1])
        self.assertEqual(len(overview.data), 2)
        self.assertIn("first step", invalid_phase.layout.annotations[0].text)
        agent_fig = agent(str(SAMPLE), str(SAMPLE), 1, nr, agent_type, [1])
        self.assertTrue(all(int(r) == 1 for r in agent_fig.data[0].x))


if __name__ == '__main__':
    unittest.main()
