"""Direct reader for Vahana's /instrumentation/events HDF5 dataset.

This adapter reads the event compound dataset in-place with h5py. It does not
require a Julia process or an intermediate JSON/Arrow export.
"""
from __future__ import annotations

from pathlib import Path
import re
from typing import Any

import h5py
import numpy as np
import pandas as pd


# Current EventLabel enum IDs from src/Instrumentation.jl. HDF5 stores enum
# fields as their integer representation, so keep this table in sync with the
# Julia enum until event-name metadata is added to the file format.
EVENT_LABELS = (
    "distribute",
    "barrier_pre_distribute",
    "barrier_post_distribute",
    "raster_bcast",
    "agent_init_alltoall",
    "agent_init_alltoallv",
    "ready_wait_agent_init_alltoallv",
    "agent_alltoall_ids",
    "agent_alltoallv_ids",
    "ready_wait_agent_alltoallv_ids",
    "agent_alltoall_state",
    "agent_alltoallv_state",
    "ready_wait_agent_alltoallv_state",
    "edge_add_alltoall",
    "edge_add_alltoallv",
    "ready_wait_edge_add_alltoallv",
    "edge_remove_alltoall",
    "edge_remove_alltoallv",
    "ready_wait_edge_remove_alltoallv",
    "apply_total",
    "prepare_read",
    "prepare_write",
    "barrier_pre_apply",
    "transition",
    "barrier_post_apply",
    "edges_add",
    "edges_remove",
    "finish_read",
    "finish_write",
    "finish_spatial",
    "memcpy_state",
    "win_shmstate",
    "win_shmdied",
    "spatial_collect",
    "spatial_kdtree",
    "spatial_cached",
    "allreduce_mustcopy",
    "allreduce_networkchanged",
    "sendagents",
    "sendedges",
    "mapreduce",
    "mapreduce_local",
    "mapreduce_allreduce",
    "join_died",
    "join_sizes",
    "ready_wait_join_sizes",
    "join_values",
    "ready_wait_join_values",
    "join_idmapping_keys",
    "join_idmapping_values",
    "join_spatial_poss",
    "join_spatial_edges",
    "transition_stats",
)
EVENT_LABEL_BY_ID = dict(enumerate(EVENT_LABELS))
CONTEXT_BY_ID = {0: "none", 1: "apply", 2: "mapreduce", 3: "init"}
KIND_BY_ID = {
    0: "phase",
    1: "barrier",
    2: "alltoall",
    3: "alltoallv",
    4: "allreduce",
    5: "win_fence",
}

_REQUIRED_FIELDS = {
    "label", "caller", "type", "context", "kind", "transition_nr", "items",
    "diag", "start_ns", "dur_ns", "send_items", "recv_items", "send_bytes",
    "recv_bytes", "agent_min_ns", "agent_max_ns", "agent_sum_ns",
    "agent_sum_sq_hi", "agent_sum_sq_lo",
}
_RANK_FILE_RE = re.compile(r"_(\d+)\.h5$")


def _decode_fixed_string(value: Any) -> str:
    """Decode Vahana's nested HDF5 compound representation of VString{32}."""
    while isinstance(value, np.void) and value.dtype.names:
        names = value.dtype.names
        if "data" in names:
            value = value["data"]
        else:
            value = [value[name] for name in names]
    raw = np.asarray(value, dtype=np.uint8).reshape(-1).tobytes()
    return raw.split(b"\0", 1)[0].decode("utf-8", errors="replace").rstrip()


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


def _legacy_rank(index: int, attrs: h5py.AttributeManager, rank_from_filename: int | None) -> int:
    if rank_from_filename is not None:
        return rank_from_filename
    counts = np.asarray(attrs.get("n_events_per_rank", []), dtype=np.int64).reshape(-1)
    displacements = np.asarray(attrs.get("displace", []), dtype=np.int64).reshape(-1)
    for rank, (start, count) in enumerate(zip(displacements, counts)):
        if start <= index < start + count:
            return rank
    if len(counts) == 1:
        return 0
    raise ValueError(f"Cannot infer MPI rank for event row {index}; file has no rank field or usable partition attributes.")


def _enum_name(value: Any, mapping: dict[int, str], field: str) -> str:
    number = int(value)
    return mapping.get(number, f"{field}#{number}")


def _int128_from_halves(hi: Any, lo: Any) -> int:
    return (int(hi) << 64) + (int(lo) & ((1 << 64) - 1))


def load_instrumentation_hdf5(filename: str | Path) -> pd.DataFrame:
    """Load a Vahana instrumentation trace directly from HDF5.

    Accepts a concrete ``.h5`` file, a base filename (with or without
    ``.h5``), or a base filename whose data is stored in ``_<rank>.h5`` files.
    Returns one normalized row per event, including rank and readable enum
    names. No Vahana-specific HDF5-to-JSON conversion is performed.
    """
    rows: list[dict[str, Any]] = []
    file_metadata: list[dict[str, Any]] = []
    for path, rank_from_filename in _resolve_files(filename):
        with h5py.File(path, "r") as h5:
            if "instrumentation/events" not in h5:
                raise ValueError(f"{path} does not contain /instrumentation/events")
            group = h5["instrumentation"]
            dataset = group["events"]
            fields = set(dataset.dtype.names or ())
            missing = _REQUIRED_FIELDS - fields
            if missing:
                raise ValueError(f"{path} has an unsupported instrumentation event schema; missing fields: {sorted(missing)}")
            file_metadata.append({
                "path": str(path),
                "mpi_size": int(group.attrs.get("mpi_size", 1)),
                "n_events": int(len(dataset)),
            })
            has_rank = "rank" in fields
            for index, raw in enumerate(dataset):
                rank = int(raw["rank"]) if has_rank else _legacy_rank(index, group.attrs, rank_from_filename)
                hi = int(raw["agent_sum_sq_hi"])
                lo = int(raw["agent_sum_sq_lo"])
                rows.append({
                    "label": _enum_name(raw["label"], EVENT_LABEL_BY_ID, "label"),
                    "label_id": int(raw["label"]),
                    "caller": _decode_fixed_string(raw["caller"]),
                    "type": _decode_fixed_string(raw["type"]),
                    "context": _enum_name(raw["context"], CONTEXT_BY_ID, "context"),
                    "kind": _enum_name(raw["kind"], KIND_BY_ID, "kind"),
                    "rank": rank,
                    "transition_nr": int(raw["transition_nr"]),
                    "items": int(raw["items"]),
                    "diag": bool(raw["diag"]),
                    "start_ns": int(raw["start_ns"]),
                    "dur_ns": int(raw["dur_ns"]),
                    "send_items": int(raw["send_items"]),
                    "recv_items": int(raw["recv_items"]),
                    "send_bytes": int(raw["send_bytes"]),
                    "recv_bytes": int(raw["recv_bytes"]),
                    "agent_min_ns": int(raw["agent_min_ns"]),
                    "agent_max_ns": int(raw["agent_max_ns"]),
                    "agent_sum_ns": int(raw["agent_sum_ns"]),
                    "agent_sum_sq_ns": _int128_from_halves(hi, lo),
                    "source_file": str(path),
                })

    if not rows:
        raise ValueError(f"No instrumentation events were found in {filename!s}.")
    frame = pd.DataFrame.from_records(rows)
    frame["duration_ms"] = frame["dur_ns"] / 1_000_000.0
    frame.sort_values(["transition_nr", "rank", "start_ns", "dur_ns"], inplace=True, ignore_index=True)
    frame.attrs["source_files"] = [m["path"] for m in file_metadata]
    frame.attrs["mpi_size"] = max(m["mpi_size"] for m in file_metadata)
    frame.attrs["n_events_per_file"] = [m["n_events"] for m in file_metadata]
    return frame
