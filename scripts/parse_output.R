#!/usr/bin/env Rscript
# Parse raw type-checker output into summary.csv, functions.csv, and crashes.csv

suppressPackageStartupMessages(library(parallel))

args <- commandArgs(trailingOnly = TRUE)
raw_dir <- if (length(args) >= 1) args[1] else "work/raw_output"
results_dir <- if (length(args) >= 2) args[2] else "results"

dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

# Collect all packages from .exit files
exit_files <- list.files(raw_dir, pattern = "\\.exit$", full.names = TRUE)

if (length(exit_files) == 0) {
  cat("No output files found in", raw_dir, "\n")
  quit(status = 1)
}

# Single-pass noise filter — one compiled regex instead of ten per-line greps.
noise_re <- paste0(
  "^(?:Typing (?:package|file):",
  "|Not supported yet:",
  "|Missing:",
  "|Prep_incl",
  "|Entry points detected:",
  "|Entry points for ",
  "|Native calls found",
  "|C source files found",
  "|Creating fresh variable:",
  "|r-c-typing:",
  "|\\s+(?:Called from|Raised at|Failure)",
  "|\\s*$)"
)

is_noise_vec <- function(lines) grepl(noise_re, lines, perl = TRUE)

# Parse one package's .out file into a data.frame of function records.
# Collect parallel character vectors rather than a data.frame per row (the
# latter is O(n^2) under do.call(rbind, ...) on long packages like vctrs).
parse_output <- function(lines) {
  n <- length(lines)
  if (n == 0) {
    return(data.frame(
      function_name = character(), status = character(),
      type_sig = character(), error_title = character(),
      error_detail = character(), stringsAsFactors = FALSE
    ))
  }

  # Preallocate generously; trim at the end.
  cap <- 1024L
  fname <- character(cap); status <- character(cap)
  type_sig <- character(cap); err_title <- character(cap)
  err_detail <- character(cap)
  k <- 0L

  push <- function(fn, st, ts, et, ed) {
    k <<- k + 1L
    if (k > cap) {
      cap <<- cap * 2L
      length(fname) <<- cap; length(status) <<- cap
      length(type_sig) <<- cap; length(err_title) <<- cap
      length(err_detail) <<- cap
    }
    fname[k] <<- fn; status[k] <<- st; type_sig[k] <<- ts
    err_title[k] <<- et; err_detail[k] <<- ed
  }

  noise <- is_noise_vec(lines)

  # Header regexes: func:  or  .C(func):
  # (one with trailing signature, one bare header followed by untypeable/timeout)
  header_bare_re <- "^(?:[A-Za-z_.][A-Za-z0-9_.]*|\\.C\\([A-Za-z_.][A-Za-z0-9_.]*\\)):$"
  header_typed_re <- "^(?:[A-Za-z_.][A-Za-z0-9_.]*|\\.C\\([A-Za-z_.][A-Za-z0-9_.]*\\)): .+"

  i <- 1L
  while (i <= n) {
    if (noise[i]) { i <- i + 1L; next }
    line <- lines[i]

    # Untypeable/timeout pattern: header line followed by "untypeable:" or "timeout:"
    if (i + 1L <= n && grepl(header_bare_re, line) &&
        grepl("^(?:untypeable|timeout):", lines[i + 1L])) {
      # Extract name (strip optional .C( ) wrapper and trailing colon)
      m <- regmatches(line, regexec("^\\.C\\(([A-Za-z_.][A-Za-z0-9_.]*)\\):$", line))[[1]]
      func_name <- if (length(m) == 2L) m[2L] else sub(":$", "", line)
      st <- if (startsWith(lines[i + 1L], "timeout:")) "timeout" else "untypeable"
      et <- sub("^(?:untypeable|timeout): ", "", lines[i + 1L], perl = TRUE)
      i <- i + 2L

      # Collect detail lines (name:, argument:, function:)
      detail_start <- i
      while (i <= n && grepl("^(?:name|argument|function): ", lines[i], perl = TRUE)) {
        i <- i + 1L
      }
      ed <- if (i > detail_start) {
        paste(lines[detail_start:(i - 1L)], collapse = "\n")
      } else NA_character_

      push(func_name, st, NA_character_, et, ed)
      next
    }

    # Typed function: "name: type_sig" or ".C(name): type_sig"
    if (grepl(header_typed_re, line)) {
      parts <- regmatches(line, regexec("^\\.C\\(([A-Za-z_.][A-Za-z0-9_.]*)\\): (.+)$", line))[[1]]
      if (length(parts) < 3L) {
        parts <- regmatches(line, regexec("^([A-Za-z_.][A-Za-z0-9_.]*): (.+)$", line))[[1]]
      }
      if (length(parts) == 3L) {
        push(parts[2L], "typed", parts[3L], NA_character_, NA_character_)
      }
    }
    i <- i + 1L
  }

  if (k == 0L) {
    return(data.frame(
      function_name = character(), status = character(),
      type_sig = character(), error_title = character(),
      error_detail = character(), stringsAsFactors = FALSE
    ))
  }

  data.frame(
    function_name = fname[seq_len(k)],
    status        = status[seq_len(k)],
    type_sig      = type_sig[seq_len(k)],
    error_title   = err_title[seq_len(k)],
    error_detail  = err_detail[seq_len(k)],
    stringsAsFactors = FALSE
  )
}

# Per-package worker — returns summary row (data.frame), functions (data.frame
# with package col) or NULL, and crash row (data.frame) or NULL.
process_package <- function(exit_file) {
  pkg <- sub("\\.exit$", "", basename(exit_file))
  out_file <- file.path(raw_dir, paste0(pkg, ".out"))
  time_file <- file.path(raw_dir, paste0(pkg, ".time"))

  exit_code <- as.integer(readLines(exit_file, n = 1L, warn = FALSE))
  elapsed <- if (file.exists(time_file)) {
    as.numeric(readLines(time_file, n = 1L, warn = FALSE))
  } else {
    NA_real_
  }

  crashed <- exit_code != 0L

  lines <- if (file.exists(out_file)) readLines(out_file, warn = FALSE) else character()

  crash_row <- NULL
  if (crashed) {
    exception_line <- grep("^\\s*(?:Failure|Invalid_argument)\\(", lines, value = TRUE, perl = TRUE)
    if (length(exception_line) == 0L) {
      exception_line <- grep("^r-c-typing: internal error", lines, value = TRUE)
    }
    error_message <- if (length(exception_line) > 0L) {
      trimws(exception_line[1L])
    } else if (exit_code == 137L) {
      "killed (SIGKILL, likely timeout or OOM)"
    } else if (exit_code == 124L) {
      "killed (timeout)"
    } else if (length(lines) > 0L) {
      first <- lines[!is_noise_vec(lines) & !grepl("^\\s", lines)]
      if (length(first) > 0L) trimws(first[1L]) else NA_character_
    } else {
      NA_character_
    }
    crash_row <- data.frame(
      package = pkg, exit_code = exit_code,
      error_message = error_message, stringsAsFactors = FALSE
    )
  }

  # Entry point counts
  ep_line <- grep("^Entry points detected:", lines, value = TRUE)
  if (length(ep_line) > 0L) {
    ep_str <- ep_line[1L]
    extract_ep <- function(name) {
      m <- regmatches(ep_str, regexec(paste0(name, "=(\\d+)"), ep_str))[[1]]
      if (length(m) == 2L) as.integer(m[2L]) else 0L
    }
    ep_call <- extract_ep("Call")
    ep_c <- extract_ep("C")
    ep_fortran <- extract_ep("Fortran")
    ep_external <- extract_ep("External")
  } else {
    ep_call <- NA_integer_
    ep_c <- NA_integer_
    ep_fortran <- NA_integer_
    ep_external <- NA_integer_
  }

  # Entry point names (for n_ep_typed)
  ep_names <- character()
  ep_header_idx <- grep("^Entry points for \\.", lines)
  for (hi in ep_header_idx) {
    j <- hi + 1L
    while (j <= length(lines) && grepl("^\\s+\\S", lines[j])) {
      ep_names <- c(ep_names, trimws(lines[j]))
      j <- j + 1L
    }
  }

  funcs <- parse_output(lines)
  n_functions <- nrow(funcs)
  n_typed <- sum(funcs$status == "typed")
  n_untypeable <- sum(funcs$status == "untypeable")
  n_timeout <- sum(funcs$status == "timeout")
  pct_typed <- if (n_functions > 0L) round(100 * n_typed / n_functions, 1) else NA_real_

  typed_names <- if (n_typed > 0L) funcs$function_name[funcs$status == "typed"] else character()
  n_ep_typed <- sum(ep_names %in% typed_names)

  summary_row <- data.frame(
    package = pkg, ep_call = ep_call, ep_c = ep_c,
    ep_fortran = ep_fortran, ep_external = ep_external,
    n_functions = n_functions, n_typed = n_typed, n_ep_typed = n_ep_typed,
    n_untypeable = n_untypeable, n_timeout = n_timeout,
    pct_typed = pct_typed, elapsed_sec = elapsed,
    exit_code = exit_code, crashed = as.integer(crashed),
    stringsAsFactors = FALSE
  )

  funcs_row <- if (n_functions > 0L) {
    funcs$package <- pkg
    funcs
  } else NULL

  list(summary = summary_row, functions = funcs_row, crash = crash_row)
}

# Process all packages in parallel. mc.preschedule=FALSE so uneven workloads
# (like vctrs at ~50k lines vs tiny packages) spread across cores.
n_cores <- max(1L, min(length(exit_files), parallel::detectCores(logical = FALSE)))
results <- if (n_cores > 1L && .Platform$OS.type != "windows") {
  mclapply(exit_files, function(f) {
    tryCatch(process_package(f), error = function(e) {
      list(summary = NULL, functions = NULL, crash = NULL,
           error = sprintf("%s: %s", basename(f), conditionMessage(e)))
    })
  }, mc.cores = n_cores, mc.preschedule = FALSE)
} else {
  lapply(exit_files, process_package)
}

# Surface any per-package errors swallowed by mclapply.
errs <- Filter(Negate(is.null), lapply(results, `[[`, "error"))
if (length(errs) > 0L) {
  for (e in errs) message("parse error: ", e)
  quit(status = 1)
}

all_summary <- Filter(Negate(is.null), lapply(results, `[[`, "summary"))
all_functions <- Filter(Negate(is.null), lapply(results, `[[`, "functions"))
all_crashes <- Filter(Negate(is.null), lapply(results, `[[`, "crash"))

# Write CSVs
summary_df <- do.call(rbind, all_summary)
write.csv(summary_df, file.path(results_dir, "summary.csv"), row.names = FALSE)
cat(sprintf("summary.csv: %d packages\n", nrow(summary_df)))

if (length(all_functions) > 0) {
  functions_df <- do.call(rbind, all_functions)
  functions_df <- functions_df[, c("package", "function_name", "status", "type_sig", "error_title", "error_detail")]
  write.csv(functions_df, file.path(results_dir, "functions.csv"), row.names = FALSE)
  cat(sprintf("functions.csv: %d functions\n", nrow(functions_df)))
} else {
  write.csv(
    data.frame(package = character(), function_name = character(), status = character(),
               type_sig = character(), error_title = character(), error_detail = character(),
               stringsAsFactors = FALSE),
    file.path(results_dir, "functions.csv"), row.names = FALSE
  )
  cat("functions.csv: 0 functions\n")
}

if (length(all_crashes) > 0) {
  crashes_df <- do.call(rbind, all_crashes)
  write.csv(crashes_df, file.path(results_dir, "crashes.csv"), row.names = FALSE)
  cat(sprintf("crashes.csv: %d packages\n", nrow(crashes_df)))
} else {
  write.csv(
    data.frame(package = character(), exit_code = integer(), error_message = character(), stringsAsFactors = FALSE),
    file.path(results_dir, "crashes.csv"), row.names = FALSE
  )
  cat("crashes.csv: 0 packages\n")
}
