CHECKER_DIR := /home/pierre/Documents/Rlanguage/r-c-typing
CHECKER     := $(CHECKER_DIR)/_build/default/bin/main.exe
NPROC       := $(shell nproc)
TIMEOUT     := 120

TS_LIB_DIR  := /home/pierre/Documents/Rlanguage/r-parser/core/tree-sitter/lib

export CHECKER_DIR CHECKER TS_LIB_DIR

.PHONY: all download extract typecheck results discover build-checker clean clean-results

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

# --- Parse output into CSV ---
results: work/.typecheck_done scripts/parse_output.R
	Rscript scripts/parse_output.R work/raw_output results

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
