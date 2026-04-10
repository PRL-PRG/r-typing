#!/usr/bin/env Rscript
# Parse raw type-checker output into summary.csv, functions.csv, and crashes.csv

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

# Lines to filter out (noise)
is_noise <- function(line) {
  grepl("^Typing (package|file):", line) ||
    grepl("^Not supported yet:", line) ||
    grepl("^Missing:", line) ||
    grepl("^Prep_incl", line) ||
    grepl("^Entry points detected:", line) ||
    grepl("^Entry points for ", line) ||
    grepl("^Native calls found", line) ||
    grepl("^C source files found", line) ||
    grepl("^Creating fresh variable:", line) ||
    grepl("^r-c-typing:", line) ||
    grepl("^\\s+(Called from|Raised at|Failure)", line) ||
    grepl("^\\s*$", line)
}

# Parse one package's .out file into a list of function records
parse_output <- function(lines) {
  functions <- list()
  i <- 1
  n <- length(lines)

  while (i <= n) {
    line <- lines[i]

    # Skip noise lines
    if (is_noise(line)) {
      i <- i + 1
      next
    }

    # Check for untypeable pattern:
    #   func_name:          (or .C(func_name):)
    #   untypeable: error_title
    #   [optional detail lines]
    is_func_header <- grepl("^[A-Za-z_.][A-Za-z0-9_.]*:$", line) ||
                      grepl("^\\.C\\([A-Za-z_.][A-Za-z0-9_.]*\\):$", line)
    if (is_func_header && i + 1 <= n && grepl("^untypeable:", lines[i + 1])) {
      m <- regmatches(line, regexec("^\\.C\\(([A-Za-z_.][A-Za-z0-9_.]*)\\):$", line))[[1]]
      func_name <- if (length(m) == 2) m[2] else sub(":$", "", line)
      error_title <- sub("^untypeable: ", "", lines[i + 1])
      i <- i + 2

      # Collect detail lines until next function block or noise.
      # Only break on "name:" (colon at EOL) which starts a new untypeable block,
      # not on "name: value" detail lines like "name: Rf_asLogical".
      detail_lines <- character()
      while (i <= n &&
             !grepl("^[A-Za-z_.][A-Za-z0-9_.]*:$", lines[i]) &&
             !grepl("^\\.C\\([A-Za-z_.][A-Za-z0-9_.]*\\):$", lines[i]) &&
             !is_noise(lines[i])) {
        detail_lines <- c(detail_lines, lines[i])
        i <- i + 1
      }

      functions[[length(functions) + 1]] <- data.frame(
        function_name = func_name,
        status = "untypeable",
        type_sig = NA_character_,
        error_title = error_title,
        error_detail = if (length(detail_lines) > 0) paste(detail_lines, collapse = "\n") else NA_character_,
        stringsAsFactors = FALSE
      )
      next
    }

    # Check for typed function pattern: func_name: type_signature
    # Also handles .C(func_name): type_signature
    # The type signature part contains at least one non-whitespace character after ": "
    if (grepl("^([A-Za-z_.][A-Za-z0-9_.]*|\\.C\\([A-Za-z_.][A-Za-z0-9_.]*\\)): .+", line)) {
      parts <- regmatches(line, regexec("^\\.C\\(([A-Za-z_.][A-Za-z0-9_.]*)\\): (.+)$", line))[[1]]
      if (length(parts) < 3) {
        parts <- regmatches(line, regexec("^([A-Za-z_.][A-Za-z0-9_.]*): (.+)$", line))[[1]]
      }
      if (length(parts) == 3) {
        functions[[length(functions) + 1]] <- data.frame(
          function_name = parts[2],
          status = "typed",
          type_sig = parts[3],
          error_title = NA_character_,
          error_detail = NA_character_,
          stringsAsFactors = FALSE
        )
        i <- i + 1
        next
      }
    }

    # Unrecognized line — skip
    i <- i + 1
  }

  if (length(functions) == 0) return(data.frame(
    function_name = character(),
    status = character(),
    type_sig = character(),
    error_title = character(),
    error_detail = character(),
    stringsAsFactors = FALSE
  ))

  do.call(rbind, functions)
}

# Process all packages
all_functions <- list()
all_summary <- list()
all_crashes <- list()

for (exit_file in exit_files) {
  pkg <- sub("\\.exit$", "", basename(exit_file))
  out_file <- file.path(raw_dir, paste0(pkg, ".out"))
  time_file <- file.path(raw_dir, paste0(pkg, ".time"))

  exit_code <- as.integer(readLines(exit_file, n = 1, warn = FALSE))
  elapsed <- if (file.exists(time_file)) {
    as.numeric(readLines(time_file, n = 1, warn = FALSE))
  } else {
    NA_real_
  }

  crashed <- exit_code != 0

  if (crashed) {
    # Extract error message from crash output
    out_lines <- if (file.exists(out_file)) readLines(out_file, warn = FALSE) else character()
    # Look for OCaml exception lines in crash backtrace
    exception_line <- grep("^\\s*(Failure|Invalid_argument)\\(", out_lines, value = TRUE)
    if (length(exception_line) == 0) {
      # Also try the "r-c-typing: internal error" line
      exception_line <- grep("^r-c-typing: internal error", out_lines, value = TRUE)
    }
    error_message <- if (length(exception_line) > 0) {
      trimws(exception_line[1])
    } else if (length(out_lines) > 0) {
      # Fall back to first non-empty, non-header line
      first <- out_lines[nchar(trimws(out_lines)) > 0 & !grepl("^Typing (package|file):", out_lines)]
      if (length(first) > 0) trimws(first[1]) else NA_character_
    } else {
      NA_character_
    }
    all_crashes[[length(all_crashes) + 1]] <- data.frame(
      package = pkg,
      exit_code = exit_code,
      error_message = error_message,
      stringsAsFactors = FALSE
    )
  }

  # Parse output
  lines <- if (file.exists(out_file)) readLines(out_file, warn = FALSE) else character()

  # Extract entry point counts from "Entry points detected: Call=N, C=N, Fortran=N, External=N"
  ep_line <- grep("^Entry points detected:", lines, value = TRUE)
  if (length(ep_line) > 0) {
    ep_str <- ep_line[1]
    extract_ep <- function(name) {
      m <- regmatches(ep_str, regexec(paste0(name, "=(\\d+)"), ep_str))[[1]]
      if (length(m) == 2) as.integer(m[2]) else 0L
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

  # Extract entry point names from "Entry points for .XXX convention:" blocks
  ep_names <- character()
  ep_header_idx <- grep("^Entry points for \\.", lines)
  for (hi in ep_header_idx) {
    j <- hi + 1
    while (j <= length(lines) && grepl("^\\s+\\S", lines[j])) {
      ep_names <- c(ep_names, trimws(lines[j]))
      j <- j + 1
    }
  }

  funcs <- parse_output(lines)

  n_functions <- nrow(funcs)
  n_typed <- sum(funcs$status == "typed")
  n_untypeable <- sum(funcs$status == "untypeable")
  pct_typed <- if (n_functions > 0) round(100 * n_typed / n_functions, 1) else NA_real_

  # Count how many entry points were typed successfully
  typed_names <- if (n_typed > 0) funcs$function_name[funcs$status == "typed"] else character()
  n_ep_typed <- sum(ep_names %in% typed_names)

  all_summary[[length(all_summary) + 1]] <- data.frame(
    package = pkg,
    ep_call = ep_call,
    ep_c = ep_c,
    ep_fortran = ep_fortran,
    ep_external = ep_external,
    n_functions = n_functions,
    n_typed = n_typed,
    n_ep_typed = n_ep_typed,
    n_untypeable = n_untypeable,
    pct_typed = pct_typed,
    elapsed_sec = elapsed,
    exit_code = exit_code,
    crashed = as.integer(crashed),
    stringsAsFactors = FALSE
  )

  if (n_functions > 0) {
    funcs$package <- pkg
    all_functions[[length(all_functions) + 1]] <- funcs
  }
}

# Write CSVs
summary_df <- do.call(rbind, all_summary)
write.csv(summary_df, file.path(results_dir, "summary.csv"), row.names = FALSE)
cat(sprintf("summary.csv: %d packages\n", nrow(summary_df)))

if (length(all_functions) > 0) {
  functions_df <- do.call(rbind, all_functions)
  # Reorder columns
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
