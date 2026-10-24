# The checker is TypR. In the CI image CHECKER_DIR/CHECKER come from the
# environment (/checker); locally they default to a sibling typr checkout's
# build tree, which holds types/ and prelude/ next to bin/.
TYPR_DIR    ?= $(abspath ../typr)
CHECKER_DIR ?= $(TYPR_DIR)/_build/default
CHECKER     ?= $(CHECKER_DIR)/bin/main.exe
PRELUDE     ?= $(CHECKER_DIR)/prelude/base.R
NPROC       := $(shell nproc)
TIMEOUT     := 6000
FUN_TIMEOUT ?= 20

# Memory. vctrs alone peaks near 14 GB, and its strict and gradual runs start
# together. GNU parallel starts no new run while less than MEMFREE is free,
# and when free memory falls below half of it, kills the youngest run; with
# --retries that run is queued again. Big runs end up one after the other
# instead of exhausting the machine (CI runners have 16 GB). A run killed
# more than RETRIES times is recorded as exit 137 below, so the dashboard
# shows it rather than losing it.
MEMFREE ?= 4G
RETRIES ?= 3
# Optional caps on a single run, applied by run_one_package.sh so a capped run
# is still recorded: RUN_TIMEOUT in seconds (exit 124), MEM_LIMIT_MB on the
# checker's address space. 0 or empty disables them.
RUN_TIMEOUT  ?= 0
MEM_LIMIT_MB ?= 0

TS_LIB_DIR  ?= /home/pierre/Documents/RLanguage/r-parser/core/tree-sitter/lib

# Every package is checked once per mode: strict reports every unbound name,
# gradual gives it the dyn type instead.
MODES ?= strict gradual

# Pass arbitrary flags to the checker. Set FALLBACK=1 to add
# --fallback-c-signature: when full-body inference fails, bind the function at
# its declared C signature so callers don't cascade as "unbound variable".
# LOG_TIMINGS=1 adds NativeSem's per-function timing lines to the .out files;
# the measurements come from the JSONL report either way.
FALLBACK ?= 1
LOG_TIMINGS ?= 0
CHECKER_OPTS ?=
ifeq ($(FALLBACK),1)
CHECKER_OPTS += --fallback-c-signature
endif
ifeq ($(LOG_TIMINGS),1)
CHECKER_OPTS += --log-times
endif

# Optional: profile each package with `perf record`. Produces
# work/raw_output/<mode>/<pkg>.perf.data alongside the existing .out/.time/.exit
# files. Override PERF_OPTS to change sample rate or unwind mode.
# Requires kernel.perf_event_paranoid <= 1 (or sudo).
PERF ?= 0
PERF_OPTS ?= -F 99 --call-graph dwarf

# Baseline for the dashboard. Auto-picks results.prev/ when present; users can
# override with `make webpage BASELINE=results.true_baseline_fb` or disable
# entirely with `make webpage BASELINE=none`.
BASELINE ?= $(shell test -f results.prev/summary.csv && echo results.prev)

export CHECKER_DIR CHECKER PRELUDE TS_LIB_DIR FUN_TIMEOUT CHECKER_OPTS PERF PERF_OPTS RUN_TIMEOUT MEM_LIMIT_MB

.PHONY: all download extract typecheck results snapshot webpage dashboard discover build-checker trends clean clean-results perf-report

all: results

# --- Download source tarballs ---
work/.download_done: packages.txt scripts/download_packages.R
	Rscript scripts/download_packages.R packages.txt work/tarballs
	touch $@

download: work/.download_done

# --- Extract tarballs ---
work/.extract_done: work/.download_done
	@mkdir -p work/sources
	@for tarball in work/tarballs/*.tar.gz; do \
		[ -f "$$tarball" ] || continue; \
		echo "Extracting $$(basename $$tarball)"; \
		tar xzf "$$tarball" -C work/sources; \
	done
	touch $@

extract: work/.extract_done

# --- Run type checker ---
work/.typecheck_done: work/.extract_done $(CHECKER)
	@mkdir -p work/raw_output
	@bash scripts/record_checker_version.sh "$(CHECKER_DIR)" work
	find work/sources -mindepth 1 -maxdepth 1 -type d \
		| parallel -j$(NPROC) --timeout $(TIMEOUT) --memfree $(MEMFREE) --retries $(RETRIES) \
			--joblog work/typecheck.joblog --bar \
			bash scripts/run_one_package.sh {1} work/raw_output {2} :::: - ::: $(MODES) \
		|| { echo "Some runs did not finish; see work/typecheck.joblog:"; \
		     awk 'NR == 1 || $$7 != 0' work/typecheck.joblog; }
	@for d in work/sources/*/; do p=$$(basename "$$d"); for m in $(MODES); do \
		[ -f "work/raw_output/$$m/$$p.exit" ] && continue; \
		echo "$$p ($$m): killed before finishing, recorded as exit 137"; \
		mkdir -p "work/raw_output/$$m"; echo 137 > "work/raw_output/$$m/$$p.exit"; \
	done; done
	touch $@

typecheck: work/.typecheck_done

# --- Snapshot the previous run before regenerating CSVs ---
# Manual target: unconditionally moves results/ to results.prev/. Useful before
# a known-disruptive checker change ("make snapshot" then iterate).
snapshot:
	@if [ -d results ] && [ -f results/summary.csv ]; then \
	    rm -rf results.prev; \
	    mv results results.prev; \
	    echo "Snapshotted previous results -> results.prev/"; \
	fi

# --- Parse output into CSV ---
# Auto-snapshot fires only when typecheck has produced fresher raw_output than
# the current results/. That way `make webpage` (which depends on results) is a
# no-op when nothing has changed, and doesn't clobber results.prev/ with a copy
# of the current run.
results: work/.typecheck_done scripts/parse_output.R
	@if [ -f results/summary.csv ] && [ work/.typecheck_done -nt results/summary.csv ]; then \
	    rm -rf results.prev; \
	    mv results results.prev; \
	    echo "Snapshotted previous results -> results.prev/"; \
	fi
	Rscript scripts/parse_output.R work/raw_output results

# --- Generate summary webpage ---
webpage: results
	Rscript scripts/generate_webpage.R results results/index.html $(BASELINE)

# --- Open dashboard in browser ---
dashboard: webpage
	xdg-open results/index.html

# --- Optional: discover packages with native code ---
discover:
	Rscript scripts/discover_packages.R > packages.txt

# --- Build the type checker ---
build-checker:
	$(MAKE) -C $(TYPR_DIR) build

# --- Trend report across CI runs ---
# Downloads results/ artifacts from past successful CI runs (cached under
# work/artifacts/) and renders time-series plots. Requires `gh` CLI auth'd
# against this repo.
trends:
	@mkdir -p results
	Rscript -e 'rmarkdown::render("scripts/trends.Rmd", output_file = "../results/trends.html", knit_root_dir = normalizePath("."))'

# --- Open perf report for a single package ---
# Usage: make perf-report PKG=clue [MODE=gradual]
# Requires `make typecheck PERF=1` to have been run.
MODE ?= strict
perf-report:
	@if [ -z "$(PKG)" ]; then \
	    echo "Usage: make perf-report PKG=<package-name>"; exit 2; \
	fi
	@perf_data="work/raw_output/$(MODE)/$(PKG).perf.data"; \
	if [ ! -f "$$perf_data" ]; then \
	    echo "No perf data at $$perf_data — run 'make typecheck PERF=1' first."; exit 1; \
	fi; \
	perf report -i "$$perf_data"

# --- Clean targets ---
clean:
	rm -rf work results

clean-results:
	rm -rf results work/.typecheck_done
