# r-typing

Pipeline to download CRAN package sources, run the **r-c-typing** type checker on their C code, and collect results into CSVs and an HTML dashboard. The checker is a separate project; this repo just orchestrates it.

## Prerequisites

- **r-c-typing** built (`dune build` from its checkout). The checker can live anywhere — point `CHECKER_DIR` at the repo root, or override `CHECKER` directly with the path to the binary. Builds against OCaml + dune.
- **tree-sitter** shared library — its directory is passed via `TS_LIB_DIR` and appended to `LD_LIBRARY_PATH` when invoking the checker.
- **R** (base only — uses the built-in `parallel` and `utils` packages; no CRAN packages required).
- **GNU parallel** — drives `make typecheck`.
- **bc** — wall-clock arithmetic in `scripts/run_one_package.sh`.
- **Universal Ctags** — used by `make webpage` to extract source snippets for the per-package detail pages. The legacy Exuberant Ctags will not work; the script relies on `--fields=+ne` (line + end fields) and `--kinds-C=f`. If `ctags` is not on `PATH`, the dashboard still renders but without inline source.
- **tar**, **find**, **realpath** — standard POSIX tools used by the Makefile and shell scripts.
- **xdg-open** *(optional)* — only needed by `make dashboard` to open the rendered HTML.

## Quick start

```bash
export CHECKER_DIR=/path/to/checker
# Edit packages.txt to list the packages you want, then:
make
```

This runs the full pipeline: **download → extract → typecheck → results**.

### Environment variables / Makefile options

| Variable | Default | Description |
|----------|---------|-------------|
| `CHECKER_DIR` | `/home/…/r-c-typing` | Path to the r-c-typing checkout |
| `CHECKER` | `$(CHECKER_DIR)/_build/default/bin/main.exe` | Checker binary |
| `TS_LIB_DIR` | tree-sitter lib path | Appended to `LD_LIBRARY_PATH` |
| `FUN_TIMEOUT` | `20` | Per-function inference timeout (seconds) |
| `CHECKER_OPTS` | *(empty)* | Extra flags passed to the checker |
| `FALLBACK` | *(unset)* | When `FALLBACK=1`, appends `--fallback-c-signature` to `CHECKER_OPTS`. Binds functions that fail body inference at their declared C signature so callers don't cascade as "unbound variable". |
| `BASELINE` | auto-detects `results.prev/` | Directory whose CSVs are diffed against the current run when generating the dashboard. Set `BASELINE=none` to disable, or `BASELINE=results.something` to compare against a specific snapshot. |

Examples:

```bash
make FALLBACK=1                  # enable the C-signature fallback
make FUN_TIMEOUT=60              # longer per-function timeout
make CHECKER_OPTS='--debug'      # pass arbitrary flags
```

## Makefile targets

| Target | Description |
|--------|-------------|
| `make` / `make all` | Run the full pipeline |
| `make download` | Download source tarballs from CRAN |
| `make extract` | Extract tarballs into `work/sources/` |
| `make typecheck` | Run r-c-typing on each package (via GNU parallel) |
| `make snapshot` | If `results/summary.csv` exists, move `results/` to `results.prev/` so the next run can use it as a baseline. Run automatically as a prerequisite of `make results`. |
| `make results` | Parse raw output into CSV files (snapshots first) |
| `make webpage` | Render `results/index.html` and per-package pages under `results/pkg/`, diffing against `BASELINE` when present |
| `make dashboard` | `make webpage` then open the dashboard via `xdg-open` |
| `make discover` | Generate `packages.txt` from CRAN (packages with compiled code) |
| `make build-checker` | Build the type checker via `dune build` |
| `make clean` | Remove all generated files |
| `make clean-results` | Remove results and re-run typecheck + parse |

## Output

### `results/summary.csv`

One row per package:

| Column | Description |
|--------|-------------|
| `package` | Package name |
| `ep_call`, `ep_c`, `ep_fortran`, `ep_external` | Entry-point counts per calling convention |
| `n_functions` | Total C functions processed |
| `n_typed` | Successfully typed functions |
| `n_ep_typed` | Typed among entry points |
| `n_untypeable` | Functions that failed type inference |
| `n_timeout` | Functions whose inference hit the per-function timeout |
| `pct_typed` | Percentage typed |
| `elapsed_sec` | Wall-clock time (seconds) |
| `exit_code` | Type checker exit code |
| `crashed` | 1 if the checker crashed |

### `results/functions.csv`

One row per function:

| Column | Description |
|--------|-------------|
| `package` | Package name |
| `function_name` | C function name |
| `status` | `typed`, `untypeable`, or `timeout` |
| `type_sig` | Inferred type signature (if typed) |
| `error_title` | Error category (if untypeable/timeout) |
| `error_detail` | Error details (if untypeable/timeout) |

### `results/crashes.csv`

Packages where the type checker crashed:

| Column | Description |
|--------|-------------|
| `package` | Package name |
| `exit_code` | Exit code (125 = unsupported construct, 137 = timeout) |
| `error_message` | Extracted error message |

### `results/index.html` and `results/pkg/<pkg>.html`

Produced by `make webpage`. The index page shows per-package counts, a
"Change" column vs. the baseline, and links to per-package detail pages.
Each detail page lists every function processed for the package with its
inferred type or error, an "entry point" badge for entry points, and an
inline source snippet (from `ctags`) when available. A client-side
filter toolbar lets you narrow by status, entry-point-only, and name.

## File layout

```
packages.txt              # One CRAN package name per line
Makefile                   # Pipeline orchestration
scripts/
  download_packages.R      # Download source tarballs
  discover_packages.R      # Query CRAN for packages with native code
  run_one_package.sh       # Run r-c-typing on one package
  parse_output.R           # Parse raw output into CSV
  generate_webpage.R       # Render results/index.html + results/pkg/*.html
assets/
  prism*.{css,js}           # Vendored Prism assets for C syntax highlighting
work/                      # Generated (gitignored)
  tarballs/                # Downloaded .tar.gz files
  sources/                 # Extracted package directories
  raw_output/              # Per-package .out, .time, .exit files
results/                   # Generated (gitignored)
  summary.csv
  functions.csv
  crashes.csv
  index.html               # Dashboard (from `make webpage`)
  pkg/<pkg>.html           # Per-package detail pages
results.prev/              # Auto-snapshot of the previous results/, used as baseline
```
