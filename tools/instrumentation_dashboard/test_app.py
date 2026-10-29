"""Dash smoke tests; run in the dashboard virtual environment."""
from pathlib import Path
import unittest

from app import create_app, make_gantt_figure
from hdf5_reader import load_instrumentation_hdf5


ROOT = Path(__file__).resolve().parents[2]
SAMPLE = ROOT / "h5" / "InstrumentationExample.h5"


class DashAppTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.events = load_instrumentation_hdf5(SAMPLE)

    def test_gantt_uses_time_as_horizontal_interval_and_rank_as_lane(self):
        figure = make_gantt_figure(self.events, 1)
        self.assertGreater(len(figure.data), 0)
        self.assertTrue(any(trace.orientation == "h" for trace in figure.data))
        total = next(trace for trace in figure.data if trace.name.startswith("apply_total"))
        self.assertGreater(max(float(x) for x in total.x), 800.0)
        self.assertEqual(total.base, 0)

        bars = [trace for trace in figure.data if trace.type == "bar" and not trace.name.startswith("apply_total")]
        self.assertTrue(bars)
        for trace in bars:
            self.assertTrue(all(float(x) > 0 for x in trace.x))
            self.assertTrue(all(str(y).startswith("Rank ") for y in trace.y))
            self.assertTrue(all(float(x) >= 0 for x in trace.base))
        short_events = [trace for trace in figure.data if trace.type == "scatter"]
        self.assertTrue(any("Berechnung" in trace.name for trace in short_events))
        self.assertIn("nicht separat instrumentiert", figure.layout.title.text)

    def test_dash_serves_page_and_serialized_layout_from_real_hdf5(self):
        app = create_app(self.events, SAMPLE)
        client = app.server.test_client()
        page = client.get("/")
        self.assertEqual(page.status_code, 200)
        layout = client.get("/_dash-layout")
        self.assertEqual(layout.status_code, 200)
        body = layout.get_data(as_text=True)
        self.assertIn("InstrumentationExample.h5", body)
        self.assertIn("apply! duration by transition", body)

        callback_key = next(iter(app.callback_map))
        update = client.post("/_dash-update-component", json={
            "output": callback_key,
            "outputs": [
                {"id": "gantt", "property": "figure"},
                {"id": "agent-distribution", "property": "figure"},
                {"id": "diagnostic-table", "property": "children"},
            ],
            "changedPropIds": ["transition-select.value"],
            "inputs": [
                {"id": "transition-select", "property": "value", "value": 1},
                {"id": "agent-type-select", "property": "value", "value": "A1"},
            ],
            "state": [],
        })
        self.assertEqual(update.status_code, 200, update.get_data(as_text=True))
        self.assertIn("Rank 0", update.get_data(as_text=True))


if __name__ == "__main__":
    unittest.main()
