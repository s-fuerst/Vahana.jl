"""Reader tests against an actual Vahana instrumentation HDF5 example."""
from pathlib import Path
import tempfile
import unittest

import h5py
import numpy as np

from hdf5_reader import load_instrumentation_hdf5


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
SAMPLE_FILE = REPOSITORY_ROOT / "h5" / "InstrumentationExample.h5"


class HDF5ReaderTests(unittest.TestCase):
    def test_reads_real_vahana_file(self):
        frame = load_instrumentation_hdf5(SAMPLE_FILE)
        self.assertEqual(len(frame), 144)
        self.assertEqual(sorted(frame["rank"].unique().tolist()), [0, 1, 2])
        self.assertEqual(sorted(frame["transition_nr"].unique().tolist()), [1, 2])
        self.assertEqual(int((frame["label"] == "apply_total").sum()), 6)
        self.assertEqual(int((frame["label"] == "transition").sum()), 6)
        self.assertEqual(int((frame["label"] == "transition_stats").sum()), 0)
        self.assertEqual(frame.loc[frame["label"] == "transition", "type"].unique().tolist(), ["A1"])
        self.assertIn("anon@instrumentation_dataframe.j", frame["caller"].iloc[0])
        self.assertEqual(frame.attrs["mpi_size"], 3)

    def test_accepts_base_filename_without_h5_suffix(self):
        base = SAMPLE_FILE.with_suffix("")
        frame = load_instrumentation_hdf5(base)
        self.assertEqual(len(frame), 144)

    def test_reads_per_rank_hdf5_files_from_base_name(self):
        with tempfile.TemporaryDirectory() as temporary:
            base = Path(temporary) / "parallel_run"
            with h5py.File(SAMPLE_FILE, "r") as source:
                dataset = source["instrumentation/events"]
                for rank in (0, 1, 2):
                    target = Path(f"{base}_{rank}.h5")
                    mask = dataset["rank"][:] == rank
                    with h5py.File(target, "w") as output:
                        group = output.create_group("instrumentation")
                        group.attrs["mpi_size"] = 3
                        group.attrs["n_events_per_rank"] = [int(mask.sum())]
                        group.attrs["displace"] = [0]
                        group.create_dataset("events", data=dataset[mask])

            frame = load_instrumentation_hdf5(base)
            self.assertEqual(len(frame), 144)
            self.assertEqual(len(frame.attrs["source_files"]), 3)
            self.assertEqual(frame["rank"].value_counts().to_dict(), {0: 48, 1: 48, 2: 48})

    def test_infers_rank_for_legacy_compound_without_rank_field(self):
        with tempfile.TemporaryDirectory() as temporary:
            target = Path(temporary) / "legacy.h5"
            with h5py.File(SAMPLE_FILE, "r") as source, h5py.File(target, "w") as output:
                source_dataset = source["instrumentation/events"]
                names = [name for name in source_dataset.dtype.names if name != "rank"]
                dtype = np.dtype([(name, source_dataset.dtype.fields[name][0]) for name in names])
                records = np.empty(len(source_dataset), dtype=dtype)
                for name in names:
                    records[name] = source_dataset[name][:]
                group = output.create_group("instrumentation")
                for name, value in source["instrumentation"].attrs.items():
                    group.attrs[name] = value
                group.create_dataset("events", data=records)

            frame = load_instrumentation_hdf5(target)
            self.assertEqual(frame["rank"].value_counts().to_dict(), {0: 48, 1: 48, 2: 48})


if __name__ == "__main__":
    unittest.main()
