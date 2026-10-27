#!/usr/bin/env Rscript
# Parse TypR's JSONL reports into the dashboard's CSVs.
# Usage: Rscript scripts/parse_output.R [raw_dir] [results_dir]
#
# raw_dir holds one subdirectory per mode (strict, gradual), each with the
# <pkg>.{jsonl,out,exit,time} files of run_one_package.sh. Writes:
#   summary.csv       one row per package. The native columns keep their
#                     NativeSem-era names; r_* describe the strict R run and
#                     rg_* the gradual one.
#   functions.csv     one row per native function or R definition, with its
#                     side and mode. The native side does not depend on the
#                     mode, so its rows come from the strict run only.
#   links.csv         one row per entry point: its native type and whether it
#                     could be bound on the R side.
#   entry_points.csv  one row per entry point symbol the R code reaches.
#   crashes.csv       one row per (package, mode) whose run did not exit 0.
#   fuzz.csv          with FUZZ=1: one row per fuzzed definition, with its
#                     sample counts per direction (in_* inside the domain,
#                     out_* outside) and verdict.
#   fuzz_samples.csv  with FUZZ=1: the fuzzing's samples. Of the expected
#                     ones (`ok` inside, `rejected` outside), only the first
#                     FUZZ_KEEP_OK per definition and direction are kept,
#                     and those with side effects.
#
# Every count is recomputed from the individual records: a run that was killed
# has no `summary` record, and its partial report is still worth reading. The
# `summary` record only sets the `truncated` flag.

suppressPackageStartupMessages({
  library(parallel)
  library(jsonlite)
})

args <- commandArgs(trailingOnly = TRUE)
raw_dir <- if (length(args) >= 1) args[1] else "work/raw_output"
results_dir <- if (length(args) >= 2) args[2] else "results"

SCHEMA_VERSION <- 3L
MODES <- c("strict", "gradual")

# Verdicts of the fuzzing, per direction (see typr's lib/fuzz.ml).
FUZZ_IN  <- c("ok", "unsound", "unknown", "error_pkg", "error_type",
              "timeout", "crash", "invalid", "not_run")
FUZZ_OUT <- c("rejected", "accepted", "not_forced", "timeout", "crash", "invalid", "not_run")
FUZZ_KEEP_OK <- as.integer(Sys.getenv("FUZZ_KEEP_OK", "2"))

dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

# A package is anything with an .exit file in some mode directory.
exit_files <- list.files(raw_dir, pattern = "\\.exit$", recursive = TRUE)
exit_files <- exit_files[basename(dirname(exit_files)) %in% MODES]
packages <- sort(unique(sub("\\.exit$", "", basename(exit_files))))

if (length(packages) == 0) {
  cat("No output files found in", file.path(raw_dir, "<mode>"), "\n")
  quit(status = 1)
}

raw_path <- function(mode, pkg, ext) file.path(raw_dir, mode, paste0(pkg, ".", ext))

read_first <- function(path, conv) {
  if (!file.exists(path)) return(conv(NA))
  v <- readLines(path, n = 1L, warn = FALSE)
  if (length(v) == 0L) conv(NA) else conv(v)
}

# A report, split by record kind. NULL when the run left no report at all.
read_report <- function(path) {
  if (!file.exists(path)) return(NULL)
  lines <- readLines(path, warn = FALSE)
  lines <- lines[nzchar(lines)]
  # A run killed while writing leaves a partial last line.
  if (length(lines) > 0L && !validate(lines[length(lines)])) lines <- lines[-length(lines)]
  if (length(lines) == 0L) return(NULL)
  recs <- stream_in(textConnection(lines), verbose = FALSE)
  split(recs, recs$k)
}

# A field of a record table, or `default` for each row when no record of that
# kind carried it.
fld <- function(df, name, default = NA) {
  if (!is.null(df) && name %in% names(df)) df[[name]] else rep(default, NROW(df))
}
nrec <- function(df) NROW(df)

# Paths relative to the package root, as the dashboard and ctags see them.
rel_path <- function(path, root) {
  if (is.na(root)) return(path)
  prefix <- paste0(root, "/")
  ifelse(!is.na(path) & startsWith(path, prefix), substring(path, nchar(prefix) + 1L), path)
}

timing_stats <- function(t) {
  t <- t[!is.na(t)]
  n <- length(t)
  list(
    n_timed_meas      = n,
    total_typing_sec  = if (n > 0L) sum(t) else NA_real_,
    mean_typing_sec   = if (n > 0L) mean(t) else NA_real_,
    median_typing_sec = if (n > 0L) median(t) else NA_real_,
    sd_typing_sec     = if (n >= 2L) sd(t) else NA_real_,
    min_typing_sec    = if (n > 0L) min(t) else NA_real_,
    max_typing_sec    = if (n > 0L) max(t) else NA_real_
  )
}

phase_sec <- function(rep, name) {
  ph <- rep$phase
  v <- fld(ph, "elapsed_sec")[fld(ph, "name") == name]
  if (length(v) == 0L) NA_real_ else v[1L]
}

# The package's native functions (globals, #defines and type declarations are
# reported too, but are not what the native side is measured on).
native_functions <- function(rep) {
  nd <- rep$native_def
  if (is.null(nd)) return(NULL)
  nd[fld(nd, "kind") == "function", , drop = FALSE]
}

native_summary <- function(rep) {
  fn <- native_functions(rep)
  ep <- rep$entry_point
  status <- fld(fn, "status")
  conv <- fld(ep, "convention")
  symbols <- fld(ep, "symbol")
  c(
    list(
      ep_call     = sum(conv == "Call"),
      ep_c        = sum(conv == "C"),
      ep_fortran  = sum(conv == "Fortran"),
      ep_external = sum(conv == "External"),
      # Distinct symbols (the per-convention counts add up to it: TypR
      # reports one convention per symbol).
      n_ep         = length(unique(symbols)),
      # Defined in src/, so typed at all.
      n_ep_reached = sum(unique(symbols) %in% fld(fn, "name")),
      n_ep_typed   = sum(unique(symbols) %in% fld(fn, "name")[status == "typed"]),
      n_ep_linked  = sum(fld(rep$link, "bound", FALSE), na.rm = TRUE),
      n_functions  = nrec(fn),
      n_typed      = sum(status == "typed"),
      n_untypeable = sum(status == "untypeable"),
      n_timeout    = sum(status == "timeout"),
      # Bound at the declared C signature after inference failed.
      n_from_c_sig = sum(status == "fallback"),
      n_internal   = sum(status == "internal"),
      n_empty_ret  = sum(status == "typed" & fld(fn, "degenerate", FALSE), na.rm = TRUE),
      n_c_files    = fld(rep$run, "n_c_files", NA_integer_)[1L]
    ),
    timing_stats(fld(fn, "elapsed_sec", NA_real_)),
    list(
      phase_load_ty_sec        = fld(rep$run, "load_ty_sec", NA_real_)[1L],
      phase_parsing_sec        = phase_sec(rep, "c_parsing"),
      phase_fundef_pass_sec    = phase_sec(rep, "native_functions")
    )
  )
}

# The R side of one run, prefixed r_ (strict) or rg_ (gradual). Only the
# package's own definitions count: the prelude is checked in the same run.
# Anonymous top-level statements are counted apart and are never part of a
# denominator.
r_summary <- function(rep, prefix) {
  rd <- rep$r_def
  rd <- rd[fld(rd, "role") == "package", , drop = FALSE]
  named <- !fld(rd, "anonymous", FALSE)
  rd <- rd[named, , drop = FALSE]
  status <- fld(rd, "status")
  unbound <- fld(rd, "unbound_kind")
  files <- rep$file
  files <- files[fld(files, "side") == "r" & fld(files, "role") == "package", , drop = FALSE]
  n_defs <- nrec(rd)
  s <- list(
    n_defs           = n_defs,
    n_stmts          = sum(!named),
    n_typed          = sum(status == "typed"),
    n_untypeable     = sum(status == "untypeable"),
    n_timeout        = sum(status == "timeout"),
    n_skipped        = sum(status == "skipped"),
    # No error block and no type either: a failure Rsem printed in a shape
    # TypR does not recognise.
    n_unknown        = sum(status == "unknown"),
    n_unbound        = sum(!is.na(unbound)),
    n_unbound_prelude = sum(unbound == "prelude", na.rm = TRUE),
    n_unbound_package = sum(unbound == "package", na.rm = TRUE),
    n_unbound_native  = sum(unbound == "native", na.rm = TRUE),
    n_other_err      = sum(status == "untypeable" & is.na(unbound)),
    n_empty_ret      = sum(status == "typed" & fld(rd, "degenerate", FALSE), na.rm = TRUE),
    n_annotated      = sum(fld(rd, "annotated", FALSE), na.rm = TRUE),
    n_files          = nrec(files),
    n_files_unparsed = sum(!fld(files, "parsed", TRUE), na.rm = TRUE),
    pct_typed        = if (n_defs > 0L) round(100 * sum(status == "typed") / n_defs, 1) else NA_real_,
    typing_sec       = if (n_defs > 0L) sum(fld(rd, "elapsed_sec", 0), na.rm = TRUE) else NA_real_
  )
  setNames(s, paste0(prefix, names(s)))
}

# The same columns, all NA, for a mode that left no report.
na_like <- function(x) lapply(x, function(v) v[NA_integer_])

# Each mode's run as a whole: exit, wall time, whether its report is complete.
run_info <- function(rep, exit_code, elapsed, prefix) {
  s <- list(
    elapsed_sec = elapsed,
    exit_code   = exit_code,
    crashed     = as.integer(is.na(exit_code) || exit_code != 0L),
    truncated   = as.integer(is.null(rep$summary) || !isTRUE(rep$summary$complete[1L]))
  )
  setNames(s, paste0(prefix, names(s)))
}

crash_message <- function(rep, exit_code, out_path) {
  exn <- fld(rep$crash, "exn")
  if (length(exn) > 0L && !is.na(exn[1L])) return(exn[1L])
  if (is.na(exit_code)) return("no exit code recorded")
  if (exit_code == 137L) return("killed (SIGKILL, likely timeout or OOM)")
  if (exit_code == 124L) return("killed (timeout)")
  lines <- if (file.exists(out_path)) readLines(out_path, warn = FALSE) else character()
  lines <- trimws(lines[nzchar(trimws(lines))])
  if (length(lines) > 0L) lines[length(lines)] else NA_character_
}

FUNCTION_COLS <- c("package", "function_name", "status", "type_sig", "error_title",
  "error_detail", "timing_sec", "side", "mode", "file", "line", "col", "degenerate",
  "is_entry_point", "convention", "annotated", "anonymous", "error_name",
  "unbound_kind", "error_line", "natives", "calls")

native_rows <- function(pkg, rep, root) {
  fn <- native_functions(rep)
  if (nrec(fn) == 0L) return(NULL)
  data.frame(
    package = pkg, function_name = fn$name, status = fn$status,
    type_sig = fld(fn, "type"), error_title = fld(fn, "error_title"),
    error_detail = fld(fn, "error_detail"), timing_sec = fld(fn, "elapsed_sec"),
    side = "native", mode = "strict", file = rel_path(fld(fn, "file"), root),
    line = fld(fn, "line"), col = fld(fn, "col"), degenerate = fld(fn, "degenerate"),
    is_entry_point = fld(fn, "is_entry_point"), convention = fld(fn, "convention"),
    annotated = NA, anonymous = FALSE, error_name = NA, unbound_kind = NA,
    error_line = NA, natives = NA, calls = fld(fn, "calls"),
    stringsAsFactors = FALSE)
}

r_rows <- function(pkg, rep, root, mode) {
  rd <- rep$r_def
  rd <- rd[fld(rd, "role") == "package", , drop = FALSE]
  if (nrec(rd) == 0L) return(NULL)
  data.frame(
    package = pkg, function_name = rd$name, status = rd$status,
    type_sig = fld(rd, "type"), error_title = fld(rd, "error_title"),
    # A skipped definition has no error block, only the exception TypR caught.
    error_detail = ifelse(is.na(fld(rd, "error_detail")), fld(rd, "skipped_exn"), fld(rd, "error_detail")),
    timing_sec = fld(rd, "elapsed_sec"),
    side = "r", mode = mode, file = rel_path(fld(rd, "file"), root),
    line = fld(rd, "line"), col = fld(rd, "col"), degenerate = fld(rd, "degenerate"),
    is_entry_point = NA, convention = NA, annotated = fld(rd, "annotated"),
    anonymous = fld(rd, "anonymous"), error_name = fld(rd, "error_name"),
    unbound_kind = fld(rd, "unbound_kind"), error_line = fld(rd, "error_line"),
    natives = fld(rd, "natives"), calls = NA,
    stringsAsFactors = FALSE)
}

# ===== Fuzzing =====
#
# TypR writes the fuzzing's records once the harness is done, so a run killed
# while fuzzing has none. The fuzz directory of run_one_package.sh tells a
# fuzzed package from one that was not.

fuzz_dir <- function(pkg) file.path(raw_dir, "strict", "fuzz", pkg)

fuzz_key <- function(df) paste(fld(df, "kind"), fld(df, "def"), sep = "\r")

# One row per fuzzed definition. The counts are recomputed from the samples
# per direction: those of `fuzz_def` add up both directions for the verdicts
# they share (timeout, crash, invalid, not_run).
fuzz_rows <- function(pkg, rep) {
  fs <- rep$fuzz_sample
  fd <- rep$fuzz_def
  if (nrec(fs) == 0L && nrec(fd) == 0L) return(NULL)
  keys <- unique(c(fuzz_key(fd), fuzz_key(fs)))
  d <- match(keys, fuzz_key(fd))
  s_key <- fuzz_key(fs)
  # A definition with samples but no `fuzz_def` record still gets its name.
  first <- match(keys, s_key)
  pick <- function(name, default = NA) {
    v <- fld(fd, name, default)[d]
    if (name %in% c("def", "kind")) ifelse(is.na(v), fld(fs, name)[first], v) else v
  }
  count <- function(dir, verdict) vapply(keys, function(k)
    sum(s_key == k & fld(fs, "dir") == dir & fld(fs, "verdict") == verdict), integer(1),
    USE.NAMES = FALSE)
  in_counts  <- setNames(lapply(FUZZ_IN,  function(v) count("in", v)),  paste0("in_", FUZZ_IN))
  out_counts <- setNames(lapply(FUZZ_OUT, function(v) count("out", v)), paste0("out_", FUZZ_OUT))
  data.frame(
    c(list(package = pkg, def = pick("def"), kind = pick("kind"), type = pick("type"),
           exported = pick("exported"), n_arrows = pick("n_arrows"),
           n_arrows_covered = pick("n_arrows_covered"), ungeneratable = pick("ungeneratable"),
           lb_ne_ub = pick("lb_ne_ub"), effects = pick("effects"),
           n_in = vapply(keys, function(k) sum(s_key == k & fld(fs, "dir") == "in"), integer(1),
                         USE.NAMES = FALSE),
           n_out = vapply(keys, function(k) sum(s_key == k & fld(fs, "dir") == "out"), integer(1),
                          USE.NAMES = FALSE)),
      in_counts, out_counts),
    stringsAsFactors = FALSE)
}

FUZZ_SAMPLE_COLS <- c("package", "def", "kind", "id", "dir", "arrow", "verdict", "args",
  "param", "op", "expected", "result", "error_class", "error_message", "error_fun",
  "error_ns", "forced", "effects", "warnings", "elapsed_sec")

# The samples worth showing: every unexpected one, and a few expected ones
# per definition and direction. One with effects is never dropped.
fuzz_sample_rows <- function(pkg, rep) {
  fs <- rep$fuzz_sample
  if (nrec(fs) == 0L) return(NULL)
  fs <- fs[order(fld(fs, "id")), , drop = FALSE]
  expected <- fld(fs, "verdict") %in% c("ok", "rejected")
  quiet <- expected & !(nzchar(fld(fs, "effects", "")) %in% TRUE)
  rank <- ave(seq_len(nrec(fs)), fuzz_key(fs), fld(fs, "dir"), quiet, FUN = seq_along)
  keep <- !quiet | rank <= FUZZ_KEEP_OK
  fs <- fs[keep, , drop = FALSE]
  as.data.frame(c(list(package = rep(pkg, nrec(fs))),
                  setNames(lapply(FUZZ_SAMPLE_COLS[-1], function(c) fld(fs, c)), FUZZ_SAMPLE_COLS[-1])),
                stringsAsFactors = FALSE)
}

# The fz_* columns of summary.csv: NA for a package that was not fuzzed, or
# whose run was killed before its fuzzing reported.
fuzz_summary <- function(pkg, rep, rows) {
  fuzzed <- dir.exists(fuzz_dir(pkg))
  s <- list(
    n_defs          = nrec(rows),
    n_ungeneratable = sum(fld(rows, "n_in", 0L) == 0L),
    n_in            = sum(fld(rows, "n_in", 0L)),
    n_out           = sum(fld(rows, "n_out", 0L)),
    n_ok            = sum(fld(rows, "in_ok", 0L)),
    n_unsound       = sum(fld(rows, "in_unsound", 0L) > 0L),
    n_unsound_samples = sum(fld(rows, "in_unsound", 0L)),
    n_unknown       = sum(fld(rows, "in_unknown", 0L)),
    n_error_pkg     = sum(fld(rows, "in_error_pkg", 0L)),
    n_error_type    = sum(fld(rows, "in_error_type", 0L)),
    n_rejected      = sum(fld(rows, "out_rejected", 0L)),
    n_accepted      = sum(fld(rows, "out_accepted", 0L) > 0L),
    n_accepted_samples = sum(fld(rows, "out_accepted", 0L)),
    n_crash         = sum(fld(rows, "in_crash", 0L) + fld(rows, "out_crash", 0L) > 0L),
    n_effects       = sum(nzchar(fld(rows, "effects", "")) %in% TRUE),
    n_not_run       = sum(fld(rows, "in_not_run", 0L) + fld(rows, "out_not_run", 0L))
  )
  reported <- !is.null(rep$summary) || nrec(rows) > 0L
  if (!fuzzed || !reported) s <- na_like(s)
  setNames(c(list(fuzzed = as.integer(fuzzed)), s), paste0("fz_", c("fuzzed", names(s))))
}

process_package <- function(pkg) {
  runs <- lapply(setNames(MODES, MODES), function(mode) list(
    rep     = read_report(raw_path(mode, pkg, "jsonl")),
    exit    = read_first(raw_path(mode, pkg, "exit"), as.integer),
    elapsed = read_first(raw_path(mode, pkg, "time"), as.numeric)))
  have <- vapply(MODES, function(m) file.exists(raw_path(m, pkg, "exit")), logical(1))

  # The native side is the same in both modes: read it from the strict run,
  # or from the gradual one when the strict run left nothing.
  native_mode <- if (!is.null(runs$strict$rep$run)) "strict" else "gradual"
  nrep <- runs[[native_mode]]$rep
  root_of <- function(rep) fld(rep$run, "root", NA_character_)[1L]

  strict_r  <- r_summary(runs$strict$rep, "r_")
  gradual_r <- r_summary(runs$gradual$rep, "rg_")
  if (is.null(runs$strict$rep))  strict_r  <- na_like(strict_r)
  if (is.null(runs$gradual$rep)) gradual_r <- na_like(gradual_r)

  native <- native_summary(nrep)
  if (is.null(nrep)) native <- na_like(native)

  # Only the strict run is fuzzed.
  fuzz <- fuzz_rows(pkg, runs$strict$rep)

  summary_row <- as.data.frame(c(
    list(package = pkg, schema_version = SCHEMA_VERSION),
    native,
    list(pct_typed = if (isTRUE(native$n_functions > 0L))
      round(100 * native$n_typed / native$n_functions, 1) else NA_real_),
    strict_r, gradual_r,
    list(n_r_files = fld(nrep$run, "n_r_files", NA_integer_)[1L]),
    if (have["strict"]) run_info(runs$strict$rep, runs$strict$exit, runs$strict$elapsed, "")
    else na_like(run_info(NULL, NA, NA, "")),
    if (have["gradual"]) run_info(runs$gradual$rep, runs$gradual$exit, runs$gradual$elapsed, "rg_")
    else na_like(run_info(NULL, NA, NA, "rg_")),
    fuzz_summary(pkg, runs$strict$rep, fuzz)
  ), stringsAsFactors = FALSE)

  funcs <- rbind(
    native_rows(pkg, nrep, root_of(nrep)),
    r_rows(pkg, runs$strict$rep, root_of(runs$strict$rep), "strict"),
    r_rows(pkg, runs$gradual$rep, root_of(runs$gradual$rep), "gradual"))

  link <- nrep$link
  links <- if (nrec(link) > 0L) data.frame(
    package = pkg, r_name = link$r_name, symbol = link$symbol,
    native_typed = fld(link, "native_typed"), native_type = fld(link, "native_type"),
    convertible = fld(link, "convertible"), reject_reason = fld(link, "reject_reason"),
    bound = fld(link, "bound"), r_type = fld(link, "r_type"),
    degenerate = fld(link, "degenerate"), stringsAsFactors = FALSE) else NULL

  ep <- nrep$entry_point
  entry_points <- if (nrec(ep) > 0L) {
    fn <- native_functions(nrep)
    data.frame(
      package = pkg, r_name = ep$r_name, symbol = ep$symbol, convention = ep$convention,
      native_status = fld(fn, "status")[match(ep$symbol, fld(fn, "name"))],
      stringsAsFactors = FALSE)
  } else NULL

  crashes <- do.call(rbind, lapply(MODES[have], function(mode) {
    r <- runs[[mode]]
    if (!is.na(r$exit) && r$exit == 0L) return(NULL)
    data.frame(package = pkg, exit_code = r$exit,
      error_message = crash_message(r$rep, r$exit, raw_path(mode, pkg, "out")),
      mode = mode, stringsAsFactors = FALSE)
  }))

  list(summary = summary_row, functions = funcs, links = links,
       entry_points = entry_points, crash = crashes, fuzz = fuzz,
       fuzz_samples = fuzz_sample_rows(pkg, runs$strict$rep))
}

# Process all packages in parallel. mc.preschedule=FALSE so uneven workloads
# (like vctrs against tiny packages) spread across cores.
n_cores <- max(1L, min(length(packages), parallel::detectCores(logical = FALSE)))
safe_process <- function(pkg) {
  tryCatch(process_package(pkg), error = function(e) {
    list(error = sprintf("%s: %s", pkg, conditionMessage(e)))
  })
}
results <- if (n_cores > 1L && .Platform$OS.type != "windows") {
  mclapply(packages, safe_process, mc.cores = n_cores, mc.preschedule = FALSE)
} else {
  lapply(packages, safe_process)
}

# Surface any per-package errors swallowed by mclapply.
errs <- Filter(Negate(is.null), lapply(results, `[[`, "error"))
if (length(errs) > 0L) {
  for (e in errs) message("parse error: ", e)
  quit(status = 1)
}

collect <- function(part) do.call(rbind, Filter(Negate(is.null), lapply(results, `[[`, part)))

write_table <- function(df, empty, name, what) {
  if (is.null(df)) df <- empty
  write.csv(df, file.path(results_dir, name), row.names = FALSE)
  cat(sprintf("%s: %d %s\n", name, nrow(df), what))
}

write_table(collect("summary"), NULL, "summary.csv", "packages")

empty_functions <- as.data.frame(setNames(rep(list(character()), length(FUNCTION_COLS)), FUNCTION_COLS))
functions_df <- collect("functions")
if (!is.null(functions_df)) functions_df <- functions_df[, FUNCTION_COLS]
write_table(functions_df, empty_functions, "functions.csv", "functions and definitions")

write_table(collect("links"),
  data.frame(package = character(), r_name = character(), symbol = character(),
             native_typed = logical(), native_type = character(), convertible = logical(),
             reject_reason = character(), bound = logical(), r_type = character(),
             degenerate = logical()),
  "links.csv", "links")

write_table(collect("entry_points"),
  data.frame(package = character(), r_name = character(), symbol = character(),
             convention = character(), native_status = character()),
  "entry_points.csv", "entry points")

write_table(collect("crash"),
  data.frame(package = character(), exit_code = integer(), error_message = character(),
             mode = character()),
  "crashes.csv", "crashed runs")

# The fuzzing's tables exist only when something was fuzzed: their absence is
# how the dashboard knows a run did not fuzz.
fuzz_files <- file.path(results_dir, c("fuzz.csv", "fuzz_samples.csv"))
if (any(dir.exists(fuzz_dir(packages)))) {
  fuzz_df <- collect("fuzz")
  write_table(fuzz_df,
    as.data.frame(c(
      list(package = character(), def = character(), kind = character(), type = character(),
           exported = logical(), n_arrows = integer(), n_arrows_covered = integer(),
           ungeneratable = logical(), lb_ne_ub = logical(), effects = character(),
           n_in = integer(), n_out = integer()),
      setNames(rep(list(integer()), length(FUZZ_IN) + length(FUZZ_OUT)),
               c(paste0("in_", FUZZ_IN), paste0("out_", FUZZ_OUT))))),
    "fuzz.csv", "fuzzed definitions")
  write_table(collect("fuzz_samples"),
    as.data.frame(setNames(rep(list(character()), length(FUZZ_SAMPLE_COLS)), FUZZ_SAMPLE_COLS)),
    "fuzz_samples.csv", "fuzzing samples")
} else {
  unlink(fuzz_files)
}

# Carry the checker-version snapshot from work/ into results/ so it survives
# results.prev/ promotion and CI artifact upload.
for (f in c("provenance.json", "checker_version.txt", "checker_status.txt", "checker_subject.txt")) {
  src <- file.path(dirname(raw_dir), f)
  if (file.exists(src)) file.copy(src, file.path(results_dir, f), overwrite = TRUE)
}
