#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd -- "$SCRIPT_DIR/../.." && pwd)"
DEFAULT_FILE="$REPO_ROOT/h5/InstrumentationExample.h5"
DATA_FILE="$DEFAULT_FILE"
DEBUG=1
APP_ARGS=()

usage() {
  cat <<'EOF'
Start the Vahana HDF5 instrumentation dashboard using uv.

Usage:
  tools/instrumentation_dashboard/start.sh [HDF5_FILE_OR_BASE] [--no-debug] [DASH_OPTIONS...]

Examples:
  tools/instrumentation_dashboard/start.sh
  tools/instrumentation_dashboard/start.sh h5/InstrumentationExample.h5
  tools/instrumentation_dashboard/start.sh results/run --port 8060
  tools/instrumentation_dashboard/start.sh results/run --no-debug

If no file is supplied, the repository sample h5/InstrumentationExample.h5 is used.
Hot reload is on by default; --no-debug disables Dash development mode.
EOF
}

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
  usage
  exit 0
fi

if [[ $# -gt 0 && "$1" != -* ]]; then
  DATA_FILE="$1"
  shift
fi

while (($#)); do
  case "$1" in
    --debug)
      DEBUG=1
      shift
      ;;
    --no-debug)
      DEBUG=0
      shift
      ;;
    *)
      APP_ARGS+=("$1")
      shift
      ;;
  esac
done

if ! command -v uv >/dev/null 2>&1; then
  echo "Error: uv is required. Install uv and retry." >&2
  exit 127
fi

# Resolve user-relative file/base paths from the repository root, independent
# of the caller's current working directory.
if [[ "$DATA_FILE" != /* ]]; then
  DATA_FILE="$REPO_ROOT/$DATA_FILE"
fi
BASE_FILE="$DATA_FILE"
if [[ "$BASE_FILE" == *.h5 ]]; then
  BASE_FILE="${BASE_FILE%.h5}"
fi
if [[ ! -f "$DATA_FILE" && ! -f "${DATA_FILE}.h5" && ! -f "${BASE_FILE}_0.h5" ]]; then
  echo "Error: no HDF5 file found for '$DATA_FILE'." >&2
  echo "Expected the file, '${DATA_FILE}.h5', or per-rank files like '${BASE_FILE}_0.h5'." >&2
  exit 2
fi

if ((DEBUG)); then
  APP_ARGS+=(--debug)
fi

cd "$REPO_ROOT"
exec uv run --project "$SCRIPT_DIR" python "$SCRIPT_DIR/app.py" "$DATA_FILE" "${APP_ARGS[@]}"
