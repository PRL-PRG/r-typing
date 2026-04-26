CHECKER_DIR ?= /home/pierre/Documents/Rlanguage/r-c-typing
CHECKER     ?= $(CHECKER_DIR)/_build/default/bin/main.exe
NPROC       := $(shell nproc)
TIMEOUT     := 6000
FUN_TIMEOUT ?= 20

TS_LIB_DIR  ?= /home/pierre/Documents/Rlanguage/r-parser/core/tree-sitter/lib

# Pass arbitrary flags to the checker. Set FALLBACK=1 to add
# --fallback-c-signature: when full-body inference fails, bind the function at
# its declared C signature so callers don't cascade as "unbound variable".
CHECKER_OPTS ?=
ifeq ($(FALLBACK),1)
CHECKER_OPTS += --fallback-c-signature
endif

# Baseline for the dashboard. Auto-picks results.prev/ when present; users can
# override with `make webpage BASELINE=results.true_baseline_fb` or disable
# entirely with `make webpage BASELINE=none`.
BASELINE ?= $(shell test -f results.prev/summary.csv && echo results.prev)

export CHECKER_DIR CHECKER TS_LIB_DIR FUN_TIMEOUT CHECKER_OPTS

.PHONY: all download extract typecheck results snapshot webpage dashboard discover build-checker clean clean-results

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
	find work/sources -mindepth 1 -maxdepth 1 -type d \
		| parallel -j$(NPROC) --timeout $(TIMEOUT) --bar \
			bash scripts/run_one_package.sh {} work/raw_output
	touch $@

typecheck: work/.typecheck_done

# --- Snapshot the previous run before regenerating CSVs ---
# Moves results/ to results.prev/ so the dashboard can pick it up as a baseline.
# Only runs when results/summary.csv exists (so a fresh checkout is unaffected).
snapshot:
	@if [ -d results ] && [ -f results/summary.csv ]; then \
	    rm -rf results.prev; \
	    mv results results.prev; \
	    echo "Snapshotted previous results -> results.prev/"; \
	fi

# --- Parse output into CSV ---
results: work/.typecheck_done scripts/parse_output.R snapshot
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
	cd $(CHECKER_DIR) && dune build

# --- Clean targets ---
clean:
	rm -rf work results

clean-results:
	rm -rf results work/.typecheck_done
