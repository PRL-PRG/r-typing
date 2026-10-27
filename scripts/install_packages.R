#!/usr/bin/env Rscript
# Install the downloaded packages, for fuzzing their inferred types: each
# from its own tarball in work/tarballs (the very version that is
# type-checked), into a library of its own.
#
# Usage: Rscript install_packages.R [TARBALLS_DIR] [LIBRARY]
#
# Their dependencies (Depends, Imports, LinkingTo) come from CRAN: as
# binaries through bspm when it is installed (the TypR image has it, with
# r2u), into the system library; otherwise built from source into LIBRARY.
# Building from source needs the headers of the system libraries a package
# links to, which r2u's binaries do not bring: when the build fails, with
# bspm, the package's own binary is installed instead, if it is the very
# version of the tarball. A package that fails to install is recorded, not
# fatal: the fuzzing skips it. LIBRARY/install_status.csv has one row per package, and
# LIBRARY/logs/<package>.log what R CMD INSTALL said.

args <- commandArgs(trailingOnly = TRUE)
tarballs_dir <- if (length(args) >= 1) args[1] else "work/tarballs"
lib <- if (length(args) >= 2) args[2] else "work/library"

repos <- c(CRAN = "https://cloud.r-project.org")
ncpus <- parallel::detectCores()
dir.create(file.path(lib, "logs"), recursive = TRUE, showWarnings = FALSE)
lib <- normalizePath(lib)
.libPaths(c(lib, .libPaths()))
Sys.setenv(MAKEFLAGS = paste0("-j", ncpus))

use_bspm <- requireNamespace("bspm", quietly = TRUE)
if (use_bspm) {
  options(bspm.version.check = FALSE)
  suppressMessages(bspm::enable())
}
cat(sprintf("Installing into %s; dependencies %s\n", lib,
            if (use_bspm) "as binaries (bspm)" else "from source"))

status_file <- file.path(tarballs_dir, "download_status.csv")
if (!file.exists(status_file)) stop("No download status: run `make download` first")
downloaded <- read.csv(status_file, stringsAsFactors = FALSE)
downloaded <- downloaded[downloaded$status == "ok", ]

base_pkgs <- rownames(installed.packages(priority = "base"))

# The packages a package needs installed to be installed and loaded.
dependencies <- function(tarball, pkg) {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  untar(tarball, files = file.path(pkg, "DESCRIPTION"), exdir = tmp)
  desc <- read.dcf(file.path(tmp, pkg, "DESCRIPTION"))
  fields <- intersect(c("Depends", "Imports", "LinkingTo"), colnames(desc))
  deps <- unlist(strsplit(desc[1, fields], ","))
  deps <- trimws(sub("\\(.*", "", deps))
  setdiff(deps[nzchar(deps)], c("R", base_pkgs))
}

missing_pkgs <- function(pkgs)
  setdiff(pkgs, rownames(installed.packages(lib.loc = .libPaths())))

# The first error R CMD INSTALL reported, for the status file.
first_error <- function(log) {
  lines <- readLines(log, warn = FALSE)
  err <- grep("^ERROR|[Ee]rror", lines, value = TRUE)
  if (length(err)) trimws(err[[1]]) else NA_character_
}

rows <- list()
for (i in seq_len(nrow(downloaded))) {
  pkg <- downloaded$package[[i]]
  tarball <- file.path(tarballs_dir, downloaded$file[[i]])
  cat(sprintf("  %s ... ", pkg))
  start <- Sys.time()

  deps <- tryCatch(dependencies(tarball, pkg), error = function(e) character())
  need <- missing_pkgs(deps)
  if (length(need)) {
    # bspm installs into the system library; without it, into ours.
    tryCatch(
      if (use_bspm) install.packages(need, repos = repos, Ncpus = ncpus, quiet = TRUE)
      else install.packages(need, lib = lib, repos = repos, Ncpus = ncpus, quiet = TRUE),
      error = function(e) NULL)
  }
  deps_missing <- missing_pkgs(deps)

  log <- file.path(lib, "logs", paste0(pkg, ".log"))
  code <- system2(file.path(R.home("bin"), "R"),
                  c("CMD", "INSTALL", "--no-test-load", paste0("--library=", shQuote(lib)),
                    shQuote(tarball)),
                  stdout = log, stderr = log)
  loads <- code == 0 && tryCatch({
    # In a fresh R: loading it here would keep it loaded for the next ones.
    system2(file.path(R.home("bin"), "Rscript"),
            c("-e", shQuote(sprintf("loadNamespace('%s', lib.loc = c('%s', .libPaths()))", pkg, lib))),
            stdout = FALSE, stderr = FALSE) == 0
  }, error = function(e) FALSE)

  status <- if (loads) "ok" else if (code == 0) "load_failed" else "install_failed"
  if (status == "install_failed" && use_bspm) {
    version <- sub(".*_(.*)\\.tar\\.gz$", "\\1", downloaded$file[[i]])
    tryCatch(install.packages(pkg, repos = repos, quiet = TRUE), error = function(e) NULL)
    got <- tryCatch(as.character(packageVersion(pkg)), error = function(e) NA_character_)
    if (identical(got, version)) status <- "ok_binary"
  }
  elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  cat(sprintf("%s (%.0fs)\n", status, elapsed))
  rows[[length(rows) + 1]] <- data.frame(
    package = pkg, file = downloaded$file[[i]], status = status,
    error = if (code != 0) first_error(log) else NA_character_,
    deps_missing = paste(deps_missing, collapse = ";"),
    elapsed_sec = round(elapsed, 1), stringsAsFactors = FALSE)
}

status <- if (length(rows)) do.call(rbind, rows) else
  data.frame(package = character(), file = character(), status = character(),
             error = character(), deps_missing = character(), elapsed_sec = numeric())
out <- file.path(lib, "install_status.csv")
write.csv(status, out, row.names = FALSE)
cat(sprintf("\nInstall status written to %s\n", out))
ok <- status$status %in% c("ok", "ok_binary")
cat(sprintf("  OK: %d (%d as binaries), failed: %d\n", sum(ok),
            sum(status$status == "ok_binary"), sum(!ok)))
