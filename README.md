# r-typing

Pipeline to download CRAN package sources, run the [r-c-typing](../r-c-typing) type checker on their C code, and collect results into CSV.

## Prerequisites

- **r-c-typing** built (`dune build` in `../r-c-typing`)
- **R** with `utils::download.packages`
- **GNU parallel**
- **bc**

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
| `make results` | Parse raw output into CSV files |
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

## File layout

```
packages.txt              # One CRAN package name per line
Makefile                   # Pipeline orchestration
scripts/
  download_packages.R      # Download source tarballs
  discover_packages.R      # Query CRAN for packages with native code
  run_one_package.sh       # Run r-c-typing on one package
  parse_output.R           # Parse raw output into CSV
work/                      # Generated (gitignored)
  tarballs/                # Downloaded .tar.gz files
  sources/                 # Extracted package directories
  raw_output/              # Per-package .out, .time, .exit files
results/                   # Generated (gitignored)
  summary.csv
  functions.csv
  crashes.csv
```
