"""Chunked, vectorized reader for Vahana instrumentation datasets."""
from __future__ import annotations

from pathlib import Path
import numpy as np
import pandas as pd
import h5py

from hdf5_reader import _resolve_files, EVENT_LABELS, CONTEXT_BY_ID, KIND_BY_ID

CHUNK_SIZE = 250_000


def _strings(bulk: np.ndarray, field: str) -> np.ndarray:
    column = bulk[field]
    while column.dtype.names:
        if column.dtype.names == ("data",):
            column = column["data"]
        else:
            column = np.column_stack([column[name] for name in column.dtype.names])
    raw = np.ascontiguousarray(column, dtype=np.uint8).reshape(-1, 32).view("S32").reshape(-1)
    values, inverse = np.unique(raw, return_inverse=True)
    decoded = np.array([bytes(v).rstrip(b"\0").decode("utf-8", errors="replace").rstrip() for v in values], dtype=object)
    return decoded[inverse]


def _names(group: h5py.Group, key: str, fallback: list[str]) -> list[str]:
    if key not in group.attrs:
        return fallback
    return [x.decode("utf-8") if isinstance(x, bytes) else str(x)
            for x in np.asarray(group.attrs[key]).reshape(-1)]


def _decode_ids(values: np.ndarray, labels: list[str], field: str) -> np.ndarray:
    ids = values.astype(np.int64)
    unique, inverse = np.unique(ids, return_inverse=True)
    decoded = np.array([labels[i] if 0 <= i < len(labels) else f"{field}#{i}" for i in unique], dtype=object)
    return decoded[inverse]


def load_trace(filename: str | Path) -> pd.DataFrame:
    parts = []
    sources = []
    numeric = ["transition_nr", "items", "start_ns", "dur_ns", "send_items", "recv_items",
               "send_bytes", "recv_bytes", "agent_min_ns", "agent_max_ns", "agent_sum_ns",
               "agent_sum_sq_hi", "agent_sum_sq_lo"]
    for path, file_rank in _resolve_files(filename):
        sources.append(str(path))
        with h5py.File(path, "r") as h5:
            if "instrumentation/events" not in h5:
                raise ValueError(f"{path} has no instrumentation events")
            group = h5["instrumentation"]
            ds = group["events"]
            fields = set(ds.dtype.names or ())
            missing = set(numeric + ["label", "caller", "type", "context", "kind"]) - fields
            if missing:
                raise ValueError(f"{path} has unsupported event schema; missing {sorted(missing)}")
            labels = _names(group, "event_labels", list(EVENT_LABELS))
            contexts = _names(group, "contexts", list(CONTEXT_BY_ID.values()))
            kinds = _names(group, "kinds", list(KIND_BY_ID.values()))
            counts = np.asarray(group.attrs.get("n_events_per_rank", []), dtype=np.int64).reshape(-1)
            offsets = np.asarray(group.attrs.get("displace", []), dtype=np.int64).reshape(-1)
            for start in range(0, len(ds), CHUNK_SIZE):
                bulk = ds[start:start + CHUNK_SIZE]
                n = len(bulk)
                if "rank" in fields:
                    rank = bulk["rank"].astype(np.int64)
                elif file_rank is not None:
                    rank = np.full(n, file_rank, dtype=np.int64)
                elif len(counts) and len(counts) == len(offsets):
                    indices = np.arange(start, start+n)
                    rank = np.searchsorted(offsets, indices, side="right") - 1
                    if np.any(rank < 0) or np.any(indices >= offsets[rank] + counts[rank]):
                        raise ValueError(f"Invalid rank partition in {path}")
                elif len(counts) == 1:
                    rank = np.zeros(n, dtype=np.int64)
                else:
                    raise ValueError(f"Cannot infer rank for {path}")
                data = {k: bulk[k].astype(np.int64) for k in numeric}
                data.update({
                    "rank": rank,
                    "step": bulk["step"].astype(np.int64) if "step" in fields else np.full(n, -1, dtype=np.int64),
                    "gc_pause_ns": bulk["gc_pause_ns"].astype(np.int64) if "gc_pause_ns" in fields else np.full(n, np.nan),
                    "label": _decode_ids(bulk["label"], labels, "label"),
                    "caller": _strings(bulk, "caller"),
                    "type": _strings(bulk, "type"),
                    "context": _decode_ids(bulk["context"], contexts, "context"),
                    "kind": _decode_ids(bulk["kind"], kinds, "kind"),
                    "source_file": str(path),
                })
                parts.append(pd.DataFrame(data))
    if not parts:
        raise ValueError(f"No instrumentation events found in {filename}")
    frame = pd.concat(parts, ignore_index=True)
    frame["duration_ms"] = frame["dur_ns"] / 1_000_000.0
    frame.sort_values(["transition_nr", "rank", "start_ns", "dur_ns"], inplace=True, ignore_index=True)
    frame.attrs["source_files"] = sources
    return frame
