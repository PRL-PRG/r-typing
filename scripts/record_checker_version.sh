#!/usr/bin/env bash
# Snapshot the r-c-typing checker's git state at typecheck time.
# Usage: record_checker_version.sh <checker_dir> <out_dir>
#
# Writes three files under <out_dir>:
#   checker_version.txt  — short SHA, with "-dirty" suffix if the worktree has
#                          any modified tracked files OR untracked files
#   checker_status.txt   — `git status --short` (empty when clean)
#   checker_subject.txt  — subject of HEAD commit
#
# All three files are always created (empty/"unknown" on failure) so downstream
# steps don't have to special-case missing files.

set -u

CHECKER_DIR="${1:?checker_dir required}"
OUT_DIR="${2:?out_dir required}"

mkdir -p "$OUT_DIR"

ver_file="$OUT_DIR/checker_version.txt"
status_file="$OUT_DIR/checker_status.txt"
subject_file="$OUT_DIR/checker_subject.txt"

# Default outputs in case git is unavailable or the dir isn't a repo.
: > "$status_file"
echo "unknown" > "$ver_file"
: > "$subject_file"

if ! command -v git >/dev/null 2>&1; then exit 0; fi
if ! git -C "$CHECKER_DIR" rev-parse --git-dir >/dev/null 2>&1; then exit 0; fi

sha=$(git -C "$CHECKER_DIR" rev-parse --short=8 HEAD 2>/dev/null || echo "unknown")
status=$(git -C "$CHECKER_DIR" status --porcelain 2>/dev/null || true)
subject=$(git -C "$CHECKER_DIR" log -1 --pretty=%s 2>/dev/null || true)

if [ -n "$status" ]; then
    printf '%s-dirty\n' "$sha" > "$ver_file"
else
    printf '%s\n' "$sha" > "$ver_file"
fi
printf '%s' "$status" > "$status_file"
printf '%s\n' "$subject" > "$subject_file"
