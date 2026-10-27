# r-typing

Pipeline to download CRAN package sources, run the **TypR** type checker on them, and collect results into CSVs and an HTML dashboard ("R we type yet?"). TypR checks a whole package: NativeSem types the C code under `src/`, those types are bound under their R names, then Rsem checks the R code under `R/`. The checker is a separate project ([PRL-PRG/typr](https://github.com/PRL-PRG/typr)); this repo just orchestrates it.

Every package is checked twice: **strict** reports every name TypR cannot resolve, **gradual** (`--gradual`) gives such names the dynamic type instead. The dashboard shows the pair.

## Prerequisites

- **TypR** built (`make build` from its checkout). By default the pipeline uses a sibling checkout's build tree, `../typr/_build/default`, which holds `bin/main.exe`, `types/` and `prelude/` side by side. Point `TYPR_DIR` at another checkout, or `CHECKER_DIR`/`CHECKER` at another build. In CI all of this comes from the `ghcr.io/prl-prg/typr` image.
- **tree-sitter** shared library — its directory is passed via `TS_LIB_DIR` and appended to `LD_LIBRARY_PATH` when invoking the checker.
- **R** with the **jsonlite** package (`parse_output.R` reads TypR's JSONL reports with it); otherwise only the built-in `parallel` and `utils` packages.
- **GNU parallel** — drives `make typecheck`.
- **bc** — wall-clock arithmetic in `scripts/run_one_package.sh`.
- **Universal Ctags** — used by `make webpage` to locate C functions for the source snippets on the per-package pages (R definitions are located from TypR's own positions). The legacy Exuberant Ctags will not work; the script relies on `--fields=+ne` (line + end fields) and `--kinds-C=f`. If `ctags` is not on `PATH`, the dashboard still renders but without inline C source.
- **tar**, **find**, **realpath** — standard POSIX tools used by the Makefile and shell scripts.
- **xdg-open** *(optional)* — only needed by `make dashboard` to open the rendered HTML.

## Quick start

```bash
# With a built TypR checkout next to this one (../typr):
# edit packages.txt to list the packages you want, then:
make
```

This runs the full pipeline: **download → extract → typecheck → results**.

### Environment variables / Makefile options

| Variable | Default | Description |
|----------|---------|-------------|
| `TYPR_DIR` | `../typr` | TypR checkout, built by `make build-checker` |
| `CHECKER_DIR` | `$(TYPR_DIR)/_build/default` | Directory the checker runs from; NativeSem looks for `types/` among its ancestors |
| `CHECKER` | `$(CHECKER_DIR)/bin/main.exe` | Checker binary |
| `PRELUDE` | `$(CHECKER_DIR)/prelude/base.R` | R signatures loaded before each package (`--prelude`); skipped if the file does not exist |
| `MODES` | `strict gradual` | Modes each package is checked in |
| `TS_LIB_DIR` | tree-sitter lib path | Appended to `LD_LIBRARY_PATH` |
| `FUN_TIMEOUT` | `20` | Per-definition timeout, native and R (seconds) |
| `MEMFREE` | `4G` | GNU parallel starts no new run while less than this is free, and kills the youngest run when free memory drops below half of it. Big runs (vctrs peaks near 14 GB) then run one after the other. |
| `RETRIES` | `3` | How many times a run killed that way is started again. A run that never finishes is recorded with exit code 137; `work/typecheck.joblog` lists every run. |
| `RUN_TIMEOUT` | `0` (off) | Wall-clock limit on one run, in seconds; a run that hits it is recorded with exit code 124 and the records it reported |
| `MEM_LIMIT_MB` | `0` (off) | Address-space limit on one run, in MB |
| `CHECKER_OPTS` | *(empty)* | Extra flags passed to the checker |
| `FALLBACK` | `1` | When `FALLBACK=1`, appends `--fallback-c-signature` to `CHECKER_OPTS`. Binds native functions that fail body inference at their declared C signature, so their R callers don't cascade as "unbound variable". Such functions are reported as `fallback`, not `typed`. |
| `LOG_TIMINGS` | `0` | When `LOG_TIMINGS=1`, appends `--log-times`: NativeSem's per-function timing lines in the `.out` files. The measurements come from the JSONL report either way. |
| `LIBRARY` | `work/library` | Where `make install` installs the packages, for fuzzing |
| `BASELINE` | auto-detects `results.prev/` | Directory whose CSVs are diffed against the current run when generating the dashboard. Set `BASELINE=none` to disable, or `BASELINE=results.something` to compare against a specific snapshot. |
| `PERF` | `0` | When `PERF=1`, each package run is wrapped with `perf record` (see [Profiling](#profiling) below). |
| `PERF_OPTS` | `-F 99 --call-graph dwarf` | Flags passed to `perf record`. Override to change sample rate or unwind mode. |
| `PERF_BIN` | `perf` | Profiler binary. Override if `perf` is not on `PATH` (e.g. a kernel-specific build under `/usr/lib/linux-tools-*/`). |

Examples:

```bash
make FALLBACK=0                  # no C-signature fallback
make FUN_TIMEOUT=60              # longer per-definition timeout
make MODES=strict                # strict runs only
make CHECKER_OPTS='--debug'      # pass arbitrary flags
```

TypR parses its flags with OCaml's `Arg`: an unknown flag fails the run with exit code 2, and values must be passed space-separated (`--timeout 20`, not `--timeout=20`).

## Makefile targets

| Target | Description |
|--------|-------------|
| `make` / `make all` | Run the full pipeline |
| `make download` | Download source tarballs from CRAN |
| `make extract` | Extract tarballs into `work/sources/` |
| `make install` | Install each package from its tarball into `LIBRARY`, for fuzzing (see [Fuzzing](#fuzzing)) |
| `make typecheck` | Run TypR on each package, in each mode (via GNU parallel) |
| `make snapshot` | If `results/summary.csv` exists, move `results/` to `results.prev/` so the next run can use it as a baseline. Run automatically as a prerequisite of `make results`. |
| `make results` | Parse the JSONL reports into CSV files (snapshots first) |
| `make webpage` | Render `results/index.html` and per-package pages under `results/pkg/`, diffing against `BASELINE` when present |
| `make dashboard` | `make webpage` then open the dashboard via `xdg-open` |
| `make discover` | Generate `packages.txt` from CRAN (packages with compiled code) |
| `make build-checker` | Build TypR (`make build` in `TYPR_DIR`) |
| `make trends` | Render `results/trends.html`, time series over past CI runs (needs `gh`) |
| `make perf-report PKG=<name> [MODE=<mode>]` | Open `perf report` for a package profiled with `PERF=1` (see [Profiling](#profiling)) |
| `make clean` | Remove all generated files |
| `make clean-results` | Remove results and re-run typecheck + parse |

In CI, the previous successful run's `results-<n>` artifact is downloaded into `results.prev/` before the pipeline runs, so the published dashboard shows changes since that run.

## Fuzzing

With `FUZZ=1`, the strict runs also check the inferred types by running the packages' functions. For each typed function (R definition or `.Call` entry point), TypR draws calls inside its inferred domain and checks that each result has the type predicted for its arguments; it also draws calls just outside, and records which the function accepts. The verdicts are in the JSONL report (`fuzz_def` and `fuzz_sample` records); the samples, the harness's results and its log in `work/raw_output/strict/fuzz/<pkg>/`.

The packages must be installed (`make install`, a prerequisite of `make typecheck` with `FUZZ=1`); those that do not load are not fuzzed. In the image, the dependencies are installed as binaries into the *system* library of the container, so installing and fuzzing must happen in the same container. The calls may have any side effect, so they run in a sandbox, `scripts/fuzz_sandbox.sh`:

- `FUZZ_SANDBOX=bwrap` (default): bubblewrap, with the whole file system read-only but for the package's fuzz directory, a private `/tmp`, and no network. In a container, bwrap needs it started with privileges:

  ```bash
  docker run --rm --cap-add SYS_ADMIN --security-opt seccomp=unconfined \
      --security-opt apparmor=unconfined -v $PWD:/rt -w /rt ghcr.io/prl-prg/typr \
      make FUZZ=1
  ```

- `FUZZ_SANDBOX=none FUZZ_UNSAFE=1`: no sandbox; the container is the boundary. Install with network, then take the network away from the container before fuzzing:

  ```bash
  docker run -d --name rt-fuzz -v $PWD:/rt -w /rt ghcr.io/prl-prg/typr sleep infinity
  docker exec rt-fuzz make install
  docker network disconnect bridge rt-fuzz
  docker exec rt-fuzz make FUZZ=1 FUZZ_SANDBOX=none FUZZ_UNSAFE=1
  docker rm -f rt-fuzz
  ```

  A function can then write anywhere in the checkout, into `work/` (the sources, the library, the outputs of the other packages) and `results/`, and nothing would show it but the file system effects the harness records for each call: prefer bwrap.

| Variable | Default | Description |
|----------|---------|-------------|
| `FUZZ` | `0` | `1` fuzzes the strict runs |
| `FUZZ_SAMPLES` | `8` | Calls per function inside its domain (more for functions with more overloads, up to 4×), and as many outside |
| `FUZZ_SEED` | `1` | Seed; the calls of a function only depend on it, the package and the function's name |
| `FUZZ_CALL_TIMEOUT` | `5` | Seconds per call (R code only: the C code is bounded by `FUZZ_RUN_TIMEOUT`) |
| `FUZZ_RUN_TIMEOUT` | `600` | Seconds per run of the harness; after it, the harness resumes after the call it was stuck on |
| `FUZZ_MEM_MB` | `4096` | Address-space limit of R, in MB |
| `FUZZ_SANDBOX` | `bwrap` | `bwrap` or `none` (see above) |
| `FUZZ_UNSAFE` | `0` | Must be `1` for `FUZZ_SANDBOX=none` |
| `FUZZ_HARNESS` | the image's, or `$(TYPR_DIR)/fuzz` | Directory of TypR's `harness.R` and `describe.R` |

## Profiling

Run any package (or the full pipeline) under `perf record` to get a sampling profile of the checker. Each run gets its own `.perf.data` file alongside the `.jsonl` / `.out` / `.time` / `.exit` outputs, so you can inspect them independently.

```bash
make typecheck PERF=1                     # profile every package
make typecheck PERF=1 -j1                 # serialize if you want one sample at a time
make perf-report PKG=clue                 # open `perf report` for one package (strict run)
make perf-report PKG=clue MODE=gradual    # ... or its gradual run
```

Outputs land in `work/raw_output/<mode>/<pkg>.perf.data`. View them with `make perf-report PKG=<pkg>` or directly:

```bash
perf report -i work/raw_output/strict/clue.perf.data
perf script -i work/raw_output/strict/clue.perf.data | flamegraph.pl > clue.svg
```

**Prerequisites:**

- `perf` on `PATH` (Debian/Ubuntu: `linux-tools-generic`; the resulting binary often lives under `/usr/lib/linux-tools-<kernel>/perf` — set `PERF_BIN` if it's not symlinked).
- `kernel.perf_event_paranoid <= 1` (check with `sysctl kernel.perf_event_paranoid`). To temporarily relax it: `sudo sysctl kernel.perf_event_paranoid=1`. Otherwise per-package runs skip profiling with a warning on stderr.
- For DWARF call-graphs (the default `--call-graph dwarf`), the checker must be built with frame pointers / debug info — the default `dune build` is fine; a stripped release build loses symbol resolution.

**Customizing the profile:** override `PERF_OPTS` to change sample rate, unwind mode, or events:

```bash
make typecheck PERF=1 PERF_OPTS='-F 999 --call-graph fp'  # higher rate, frame-pointer unwind
make typecheck PERF=1 PERF_OPTS='-e cache-misses -F 99'   # hardware event instead
```

## Output

`make typecheck` writes `work/raw_output/<mode>/<pkg>.{jsonl,out,time,exit}`: TypR's report (one JSON record per line, `--report`), its stdout+stderr, the wall-clock time and the exit code. `make results` recomputes every number from the individual records of the reports, so a run that was killed still contributes what it reported before dying.

The CSV schema only ever grows: columns that predate TypR keep their names and their meaning (native), so older results and CI artifacts still read correctly. `schema_version` is 2 for TypR results.

### `results/summary.csv`

One row per package (never one per mode).

| Column | Description |
|--------|-------------|
| `package` | Package name |
| `schema_version` | 2 (absent before TypR) |
| `ep_call`, `ep_c`, `ep_fortran`, `ep_external` | Entry points per calling convention |
| `n_ep` | Distinct entry-point symbols the R code reaches |
| `n_ep_reached` | Entry points defined in `src/` |
| `n_ep_typed` | Entry points typed |
| `n_ep_linked` | Entry points bound in R (their native type converted to an R one) |
| `n_functions` | Native functions reachable from an entry point |
| `n_typed` | Native functions typed |
| `n_untypeable`, `n_timeout`, `n_internal` | Native functions that failed, timed out, or hit an internal error |
| `n_from_c_sig` | Native functions bound at their declared C signature after inference failed (`FALLBACK=1`) |
| `n_empty_ret` | Native functions typed with an `empty` result |
| `pct_typed` | Percentage of native functions typed |
| `n_timed_meas`, `total_typing_sec`, `mean_typing_sec`, `median_typing_sec`, `sd_typing_sec`, `min_typing_sec`, `max_typing_sec` | Native per-function inference times |
| `phase_load_ty_sec`, `phase_parsing_sec`, `phase_fundef_pass_sec` | Native phases: loading `types/`, parsing C, typing the functions |
| `n_c_files`, `n_r_files` | Source files found |
| `r_n_defs` | Named top-level R definitions (the R denominator) |
| `r_n_stmts` | Anonymous top-level statements, never in a denominator |
| `r_n_typed`, `r_n_untypeable`, `r_n_timeout`, `r_n_skipped`, `r_n_unknown` | R definitions by outcome (`skipped`: TypR caught an exception; `unknown`: neither a type nor an error block) |
| `r_n_unbound` | R definitions failing on an unbound name, split into `r_n_unbound_prelude` (a name the prelude lacks), `r_n_unbound_package` (one of the package's own names, itself untyped) and `r_n_unbound_native` (a native symbol TypR could not bind) |
| `r_n_other_err` | R definitions untypeable for another reason |
| `r_n_empty_ret`, `r_n_annotated` | R definitions typed with an `empty` result; checked against a `#\|` signature |
| `r_n_files`, `r_n_files_unparsed` | R files, and those that did not parse |
| `r_pct_typed`, `r_typing_sec` | Percentage of R definitions typed; total time spent on them |
| `rg_*` | The same R columns for the gradual run |
| `elapsed_sec`, `exit_code`, `crashed`, `truncated` | The strict run: wall-clock time, exit code, 1 if it did not exit 0, 1 if its report has no `summary` record |
| `rg_elapsed_sec`, `rg_exit_code`, `rg_crashed`, `rg_truncated` | The same for the gradual run |

The native columns come from the strict run (the native side does not depend on the mode), or from the gradual run if the strict one left no report.

### `results/functions.csv`

One row per native function or R definition. Native functions appear once (from the strict run); R definitions once per mode.

| Column | Description |
|--------|-------------|
| `package` | Package name |
| `function_name` | C function or R definition name (`<statement>` for an anonymous one) |
| `status` | `typed`, `untypeable`, `timeout`; native also `fallback`, `internal`; R also `skipped`, `unknown` |
| `type_sig` | Inferred type (if typed, or the C signature for `fallback`) |
| `error_title` | Error category (if not typed) |
| `error_detail` | Error details, or the exception for `skipped` |
| `timing_sec` | Time spent on it |
| `side` | `native` or `r` (`native` for every row before TypR) |
| `mode` | `strict` or `gradual` |
| `file`, `line`, `col` | Where it is defined, relative to the package root (no line for native functions) |
| `degenerate` | Typed with an `empty` result |
| `is_entry_point`, `convention` | Native: reached directly from R, and how |
| `annotated`, `anonymous` | R: checked against a `#\|` signature; an anonymous top-level statement |
| `error_name`, `unbound_kind`, `error_line` | R: the unbound name, its kind (`prelude`, `package`, `native`), and the line of the error |
| `natives`, `calls` | R: native symbols it calls; native: package functions it calls (`;`-separated) |

### `results/links.csv`

One row per entry point: how its native type became (or did not become) an R binding.

| Column | Description |
|--------|-------------|
| `package`, `r_name`, `symbol` | Package, name in the R code, C symbol |
| `native_typed`, `native_type` | Whether NativeSem typed it, and its type |
| `convertible`, `bound`, `r_type` | Whether the type converts to an R calling convention, was bound, and as what |
| `reject_reason` | Why not: `no native type`, `empty native type`, `not a function type`, `empty after conversion` |
| `degenerate` | The R type has an `empty` result |

### `results/entry_points.csv`

One row per entry point: `package`, `r_name`, `symbol`, `convention`, and `native_status` (the C function's status, empty when `src/` does not define it).

### `results/crashes.csv`

One row per run that did not exit 0:

| Column | Description |
|--------|-------------|
| `package` | Package name |
| `exit_code` | Exit code (2 = uncaught exception or bad flag, 124/137 = killed) |
| `error_message` | The exception from the report's `crash` record, else the last line of output |
| `mode` | `strict` or `gradual` |

### `results/provenance.json`

The four commits the checker was built from — TypR, Rsem, NativeSem, rstt — and whether the submodules were at the commits TypR pins (`pinned`). Copied from the image in CI, derived from git for a local checkout. `checker_version.txt`, `checker_status.txt` and `checker_subject.txt` are still written alongside it.

### `results/index.html` and `results/pkg/<pkg>.html`

Produced by `make webpage`. The index page leads with the strict/gradual pair and **prelude debt** (definitions blocked only by a name missing from the prelude), then a native → R funnel, a per-package table, the missing prelude names ranked by definitions blocked, why entry points failed to bind, and error categories, `empty` returns and timings per side. Each detail page has one card per native function and per R definition, with its type or error, its source, and filters by side, mode, status, entry point, and name.

The page renders NativeSem-era results too (no R side, no `links.csv`): every R section is skipped.

## File layout

```
packages.txt              # One CRAN package name per line
Makefile                   # Pipeline orchestration
scripts/
  download_packages.R      # Download source tarballs
  discover_packages.R      # Query CRAN for packages with native code
  record_checker_version.sh # Record the checker's provenance
  run_one_package.sh       # Run TypR on one package in one mode
  parse_output.R           # Parse the JSONL reports into CSV
  generate_webpage.R       # Render results/index.html + results/pkg/*.html
  trends.Rmd               # Time series over past CI runs
assets/
  prism*.{css,js}          # Vendored Prism assets for C and R syntax highlighting
work/                      # Generated (gitignored)
  tarballs/                # Downloaded .tar.gz files
  sources/                 # Extracted package directories
  raw_output/<mode>/       # Per-package .jsonl, .out, .time, .exit files
results/                   # Generated (gitignored)
  summary.csv
  functions.csv
  links.csv
  entry_points.csv
  crashes.csv
  provenance.json
  index.html               # Dashboard (from `make webpage`)
  pkg/<pkg>.html           # Per-package detail pages
results.prev/              # Auto-snapshot of the previous results/, used as baseline
```
