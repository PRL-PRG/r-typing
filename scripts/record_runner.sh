#!/usr/bin/env bash
# Snapshot the machine the type checker runs on, so that timing trends can
# tell a slower checker from a slower runner (GitHub's ubuntu-latest jobs land
# on different CPU models).
# Usage: record_runner.sh <out_dir>
#
# Writes <out_dir>/runner.json: {cpu_model, nproc, mem_total_kb, runner_name,
# run_url, run_number}; the last two link the dashboard to its GitHub Actions
# run. Always created; a field it cannot read is "unknown" (or null, or "").

set -u

OUT_DIR="${1:?out_dir required}"
mkdir -p "$OUT_DIR"

cpu=$(sed -n 's/^model name[[:space:]]*:[[:space:]]*//p' /proc/cpuinfo 2>/dev/null | head -1)
[ -n "$cpu" ] || cpu=unknown
ncpu=$(nproc 2>/dev/null || echo null)
mem=$(sed -n 's/^MemTotal:[[:space:]]*\([0-9]*\).*/\1/p' /proc/meminfo 2>/dev/null)
[ -n "$mem" ] || mem=null
name=${RUNNER_NAME:-unknown}
run_url=""
if [ -n "${GITHUB_RUN_ID:-}" ] && [ -n "${GITHUB_REPOSITORY:-}" ]; then
    run_url="${GITHUB_SERVER_URL:-https://github.com}/$GITHUB_REPOSITORY/actions/runs/$GITHUB_RUN_ID"
fi
run_number=${GITHUB_RUN_NUMBER:-null}

# Strip what would break the JSON string.
esc() { printf '%s' "$1" | tr -d '"\\' | tr -s ' '; }
printf '{"cpu_model":"%s","nproc":%s,"mem_total_kb":%s,"runner_name":"%s","run_url":"%s","run_number":%s}\n' \
    "$(esc "$cpu")" "$ncpu" "$mem" "$(esc "$name")" "$(esc "$run_url")" "$run_number" \
    > "$OUT_DIR/runner.json"
