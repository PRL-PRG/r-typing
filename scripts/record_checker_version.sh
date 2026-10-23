#!/usr/bin/env bash
# Snapshot the checker's version at typecheck time.
# Usage: record_checker_version.sh <checker_dir> <out_dir>
#
# Writes under <out_dir>:
#   provenance.json      — {typr, rsem, nativesem, rstt, pinned, subject,
#                          built_at}: the four commits the checker was built
#                          from, and whether the submodules were at the
#                          commits typr pins
#   checker_version.txt  — short typr SHA, with "-dirty" suffix if the worktree
#                          has any modified tracked files OR untracked files
#   checker_status.txt   — `git status --short` (empty when clean)
#   checker_subject.txt  — subject of HEAD commit
#
# In the TypR image, /checker is not a git repository: provenance.json was
# written when the image was built and is copied as is. Locally the checker
# directory lies inside a typr checkout, and the same file is derived from git.
#
# All files are always created (empty/"unknown" on failure) so downstream
# steps don't have to special-case missing files.

set -u

CHECKER_DIR="${1:?checker_dir required}"
OUT_DIR="${2:?out_dir required}"

mkdir -p "$OUT_DIR"

prov_file="$OUT_DIR/provenance.json"
ver_file="$OUT_DIR/checker_version.txt"
status_file="$OUT_DIR/checker_status.txt"
subject_file="$OUT_DIR/checker_subject.txt"

# Default outputs in case git is unavailable or the dir isn't a repo.
rm -f "$prov_file"
: > "$status_file"
echo "unknown" > "$ver_file"
: > "$subject_file"

if [ -f "$CHECKER_DIR/provenance.json" ]; then
    cp "$CHECKER_DIR/provenance.json" "$prov_file"
    sha=$(sed -n 's/.*"typr" *: *"\([0-9a-f]*\)".*/\1/p' "$prov_file" | cut -c1-8)
    [ -n "$sha" ] && printf '%s\n' "$sha" > "$ver_file"
    exit 0
fi

if ! command -v git >/dev/null 2>&1; then exit 0; fi
if ! git -C "$CHECKER_DIR" rev-parse --git-dir >/dev/null 2>&1; then exit 0; fi

top=$(git -C "$CHECKER_DIR" rev-parse --show-toplevel)
sha=$(git -C "$top" rev-parse --short=8 HEAD 2>/dev/null || echo "unknown")
status=$(git -C "$top" status --porcelain 2>/dev/null || true)
subject=$(git -C "$top" log -1 --pretty=%s 2>/dev/null || true)

if [ -n "$status" ]; then
    printf '%s-dirty\n' "$sha" > "$ver_file"
else
    printf '%s\n' "$sha" > "$ver_file"
fi
printf '%s' "$status" > "$status_file"
printf '%s\n' "$subject" > "$subject_file"

# The checked-out commit of each submodule, and whether it is the one typr
# records. Empty when the submodule is missing.
pinned=true
sub_sha() {
    local cur rec
    cur=$(git -C "$top/$1" rev-parse HEAD 2>/dev/null || true)
    rec=$(git -C "$top" rev-parse "HEAD:$1" 2>/dev/null || true)
    [ -n "$cur" ] && [ "$cur" != "$rec" ] && pinned=false
    printf '%s' "$cur"
}
rsem=$(sub_sha Rsem)
nativesem=$(sub_sha nativesem)
rstt=$(sub_sha rstt)
printf '{"typr":"%s","rsem":"%s","nativesem":"%s","rstt":"%s","pinned":%s,"subject":"local","built_at":null}\n' \
    "$(git -C "$top" rev-parse HEAD)" "$rsem" "$nativesem" "$rstt" "$pinned" > "$prov_file"
