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

START=$(date +%s.%N)
"$CHECKER" ${CHECKER_OPTS-} "$PKG_DIR" > "$OUT_DIR/$PKG_NAME.out" 2>&1
EXIT_CODE=$?
END=$(date +%s.%N)

ELAPSED=$(echo "$END - $START" | bc)

echo "$EXIT_CODE" > "$OUT_DIR/$PKG_NAME.exit"
echo "$ELAPSED" > "$OUT_DIR/$PKG_NAME.time"
