#!/usr/bin/env bash
# Run TypR on one extracted package directory, in one mode.
# Usage: run_one_package.sh <package-dir> <output-root> [strict|gradual]
#
# Produces, under <output-root>/<mode>/:
#   <pkg>.jsonl — TypR's report (one JSON record per line); the measurements
#   <pkg>.out   — stdout+stderr, kept for debugging
#   <pkg>.time  — wall-clock seconds
#   <pkg>.exit  — exit code
#   fuzz/<pkg>/ — with FUZZ=1, strict mode only: the fuzzing's samples, the
#                 harness's results and log (its verdicts are in the report)
#
# The mode is a directory rather than part of the file name because R package
# names may contain dots (data.table, R.utils).

set -u

PKG_DIR="$(realpath "$1")"
MODE="${3:-strict}"
OUT_DIR="$(realpath -m "$2")/$MODE"

PKG_NAME="$(basename "$PKG_DIR")"
# Before the cd below: the paths the fuzzing gets must be absolute.
SCRIPTS_DIR="$(dirname "$(realpath "$0")")"
LIBRARY="$(realpath -m "${LIBRARY:-work/library}")"

CHECKER_DIR="${CHECKER_DIR:-/checker}"
CHECKER="${CHECKER:-$CHECKER_DIR/_build/default/bin/main.exe}"
TS_LIB_DIR="${TS_LIB_DIR:-/home/pierre/Documents/RLanguage/r-parser/core/tree-sitter/lib}"
export LD_LIBRARY_PATH="${TS_LIB_DIR}${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"

mkdir -p "$OUT_DIR"

# NativeSem looks for types/ among the ancestors of the working directory.
cd "$CHECKER_DIR"

# TypR parses its flags with OCaml's Arg: values go space-separated, never as
# --flag=value, and the package directory comes last.
OPTS=(--report "$OUT_DIR/$PKG_NAME.jsonl")
case "$MODE" in
    strict) ;;
    gradual) OPTS+=(--gradual) ;;
    *) echo "unknown mode '$MODE' (expected strict or gradual)" >&2; exit 2 ;;
esac
if [ -n "${FUN_TIMEOUT:-}" ]; then
    OPTS+=(--timeout "$FUN_TIMEOUT")
fi
# Without a prelude every base R function is unbound; skip it rather than fail
# when running against a checkout that does not have one.
if [ -n "${PRELUDE:-}" ] && [ -f "$PRELUDE" ]; then
    OPTS+=(--prelude "$PRELUDE")
fi

# Fuzzing (FUZZ=1): the strict run only (dyn has no values to draw), and only
# packages that load: `make install` put them in LIBRARY, but their
# dependencies may have gone to the system library of another container (cf.
# the README). The harness loads the package through the sandbox of
# fuzz_sandbox.sh.
if [ "${FUZZ:-0}" = 1 ] && [ "$MODE" = strict ]; then
    export R_LIBS="$LIBRARY${R_LIBS:+:$R_LIBS}"
    if Rscript -e 'loadNamespace(commandArgs(TRUE)[1])' "$PKG_NAME" >/dev/null 2>&1; then
        OPTS+=(--fuzz-runner "$SCRIPTS_DIR/fuzz_sandbox.sh"
               --fuzz-dir "$OUT_DIR/fuzz/$PKG_NAME"
               --fuzz-samples "${FUZZ_SAMPLES:-8}" --fuzz-seed "${FUZZ_SEED:-1}"
               --fuzz-call-timeout "${FUZZ_CALL_TIMEOUT:-5}")
        if [ -n "${FUZZ_HARNESS:-}" ]; then
            OPTS+=(--fuzz-harness "$FUZZ_HARNESS")
        fi
        mkdir -p "$OUT_DIR/fuzz"
    else
        echo "$PKG_NAME: does not load (see $LIBRARY/install_status.csv), not fuzzed" >&2
    fi
fi

# Optional: wrap with `perf record` when PERF=1. `--quiet` keeps perf's own
# chatter out of the .out file; only the checker's stdout/stderr land there.
# Output: <mode>/<pkg>.perf.data, viewable with `perf report -i <file>`.
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

# Caps on one run, applied here rather than by the caller so that a run that
# hits one still gets its .exit/.time files and keeps the records its report
# streamed before dying. RUN_TIMEOUT (seconds): the run is killed and exits
# 124. MEM_LIMIT_MB: the address-space limit of the checker process.
LIMIT_PREFIX=()
if [ -n "${RUN_TIMEOUT:-}" ] && [ "$RUN_TIMEOUT" != 0 ]; then
    LIMIT_PREFIX=(timeout --kill-after=30 "$RUN_TIMEOUT")
fi

# Stale files from an earlier run would be read as this run's, should this
# one be killed before writing its own.
rm -f "$OUT_DIR/$PKG_NAME".{jsonl,exit,time}

START=$(date +%s.%N)
(
    if [ -n "${MEM_LIMIT_MB:-}" ] && [ "$MEM_LIMIT_MB" != 0 ]; then
        ulimit -v $((MEM_LIMIT_MB * 1024))
    fi
    exec "${LIMIT_PREFIX[@]}" "${PERF_PREFIX[@]}" "$CHECKER" ${CHECKER_OPTS-} "${OPTS[@]}" "$PKG_DIR"
) > "$OUT_DIR/$PKG_NAME.out" 2>&1
EXIT_CODE=$?
END=$(date +%s.%N)

ELAPSED=$(echo "$END - $START" | bc)

echo "$EXIT_CODE" > "$OUT_DIR/$PKG_NAME.exit"
echo "$ELAPSED" > "$OUT_DIR/$PKG_NAME.time"
