#!/usr/bin/env bash
# Run the r-c-typing type checker on one extracted package directory.
# Usage: run_one_package.sh <package-dir> <output-dir>
#
# Produces:
#   <output-dir>/<pkg>.out  — stdout+stderr
#   <output-dir>/<pkg>.time — wall-clock seconds
#   <output-dir>/<pkg>.exit — exit code

set -u

PKG_DIR="$(realpath "$1")"
OUT_DIR="$(realpath -m "$2")"

PKG_NAME="$(basename "$PKG_DIR")"

CHECKER_DIR="${CHECKER_DIR:-/home/pierre/Documents/Rlanguage/r-c-typing}"
CHECKER="${CHECKER:-$CHECKER_DIR/_build/default/bin/main.exe}"
TS_LIB_DIR="${TS_LIB_DIR:-/home/pierre/Documents/Rlanguage/r-parser/core/tree-sitter/lib}"
export LD_LIBRARY_PATH="${TS_LIB_DIR}${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"

mkdir -p "$OUT_DIR"

cd "$CHECKER_DIR"

TIMEOUT_OPT=()
if [ -n "${FUN_TIMEOUT:-}" ]; then
    TIMEOUT_OPT=(--timeout "$FUN_TIMEOUT")
fi

# Optional: wrap with `perf record` when PERF=1. `--quiet` keeps perf's own
# chatter out of the .out file; only the checker's stdout/stderr land there.
# Output: <pkg>.perf.data, viewable with `perf report -i <pkg>.perf.data`.
PERF_PREFIX=()
if [ "${PERF:-0}" = "1" ]; then
    PERF_BIN="${PERF_BIN:-perf}"
    if ! command -v "$PERF_BIN" >/dev/null 2>&1; then
        echo "PERF=1 but '$PERF_BIN' not found in PATH; skipping profiling for $PKG_NAME" >&2
    else
        PERF_PREFIX=("$PERF_BIN" record ${PERF_OPTS:--F 99 --call-graph dwarf} \
            --quiet -o "$OUT_DIR/$PKG_NAME.perf.data" --)
    fi
fi

START=$(date +%s.%N)
"${PERF_PREFIX[@]}" "$CHECKER" ${CHECKER_OPTS-} "${TIMEOUT_OPT[@]}" "$PKG_DIR" \
    > "$OUT_DIR/$PKG_NAME.out" 2>&1
EXIT_CODE=$?
END=$(date +%s.%N)

ELAPSED=$(echo "$END - $START" | bc)

echo "$EXIT_CODE" > "$OUT_DIR/$PKG_NAME.exit"
echo "$ELAPSED" > "$OUT_DIR/$PKG_NAME.time"
