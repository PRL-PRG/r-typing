#!/usr/bin/env bash
# The command TypR runs its fuzzing harness with (--fuzz-runner): Rscript, in
# a sandbox, since the calls it makes to the package's functions may have any
# side effect.
# Usage (by TypR): fuzz_sandbox.sh HARNESS.R DESCRIBE.R PKG SAMPLES RESULTS TIMEOUT
#
# FUZZ_SANDBOX selects the sandbox:
#   bwrap (default)  bubblewrap: the whole file system read-only but for the
#                    directory of RESULTS, a private /tmp (also $HOME), no
#                    network, nor any other namespace shared. In a container,
#                    bwrap needs it started with --cap-add SYS_ADMIN
#                    --security-opt seccomp=unconfined --security-opt
#                    apparmor=unconfined. A new /proc also needs
#                    --security-opt systempaths=unconfined; without it, the
#                    container's /proc is bound read-only instead (the
#                    calls can then see its processes).
#   none             no sandbox: the container the pipeline runs in must be
#                    the boundary (no network, the inputs mounted read-only;
#                    cf. the README). Refused unless FUZZ_UNSAFE=1. Nothing
#                    then keeps a function from writing into the experiment's
#                    outputs: the samples with file system effects say which
#                    packages to suspect.
# Either way: FUZZ_MEM_MB (default 4096) caps the address space of R, and
# FUZZ_RUN_TIMEOUT (seconds, default 600) one run of the harness, after
# which TypR resumes it after the sample it was stuck on.

set -u

RESULTS="$5"
OUT="$(dirname "$(realpath -m "$RESULTS")")"
MODE="${FUZZ_SANDBOX:-bwrap}"
LIMITS=(timeout --kill-after=10 "${FUZZ_RUN_TIMEOUT:-600}")
ulimit -v $(( ${FUZZ_MEM_MB:-4096} * 1024 ))

case "$MODE" in
    bwrap)
        BWRAP=(bwrap --ro-bind / / --dev /dev --tmpfs /tmp
               --bind "$OUT" "$OUT" --unshare-all --die-with-parent --new-session
               --setenv HOME /tmp --chdir /tmp)
        if "${BWRAP[@]}" --proc /proc true 2>/dev/null; then
            BWRAP+=(--proc /proc)
        else
            BWRAP+=(--ro-bind /proc /proc)
        fi
        if ! "${BWRAP[@]}" true 2>/dev/null; then
            echo "fuzz_sandbox: bwrap cannot create its sandbox here:" >&2
            "${BWRAP[@]}" true >&2
            echo "fuzz_sandbox: in a container, start it with --cap-add SYS_ADMIN" \
                 "--security-opt seccomp=unconfined --security-opt apparmor=unconfined;" \
                 "or use FUZZ_SANDBOX=none FUZZ_UNSAFE=1 inside a container without network" >&2
            exit 3
        fi
        exec "${LIMITS[@]}" "${BWRAP[@]}" Rscript "$@"
        ;;
    none)
        if [ "${FUZZ_UNSAFE:-0}" != 1 ]; then
            echo "fuzz_sandbox: FUZZ_SANDBOX=none runs the package's functions unconfined;" \
                 "set FUZZ_UNSAFE=1 if the container around the pipeline is the sandbox" >&2
            exit 3
        fi
        HOME_DIR="$(mktemp -d)"
        trap 'rm -rf "$HOME_DIR"' EXIT
        cd "$HOME_DIR" && HOME="$HOME_DIR" "${LIMITS[@]}" Rscript "$@"
        ;;
    *)
        echo "fuzz_sandbox: unknown FUZZ_SANDBOX '$MODE' (expected bwrap or none)" >&2
        exit 2
        ;;
esac
