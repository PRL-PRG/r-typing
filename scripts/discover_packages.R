#!/usr/bin/env Rscript
# Query CRAN for packages that have native (C/C++) code.
# Writes package names to stdout (one per line), suitable for redirecting to packages.txt.

args <- commandArgs(trailingOnly = TRUE)
max_packages <- if (length(args) >= 1) as.integer(args[1]) else 100L

cat("Querying CRAN for packages with compiled code...\n", file = stderr())

ap <- available.packages(repos = "https://cloud.r-project.org")

# NeedsCompilation field indicates packages with C/C++/Fortran code
has_native <- ap[!is.na(ap[, "NeedsCompilation"]) & ap[, "NeedsCompilation"] == "yes", ]

pkg_names <- sort(rownames(has_native))
cat(sprintf("Found %d packages with compiled code\n", length(pkg_names)), file = stderr())

if (length(pkg_names) > max_packages) {
  pkg_names <- pkg_names[seq_len(max_packages)]
  cat(sprintf("Limiting to first %d packages\n", max_packages), file = stderr())
}

cat(pkg_names, sep = "\n")
cat("\n")
