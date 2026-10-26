#!/usr/bin/env Rscript
# Generate a summary webpage from typing results.
# Usage: Rscript scripts/generate_webpage.R <results_dir> <output.html> [baseline_dir]
#
# When [baseline_dir] is given (and points to a dir with summary.csv +
# functions.csv), the index page also shows progress vs that baseline:
# global Δ cards, per-package Δ-typed column, and was/now status tags
# on per-function rows.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: Rscript generate_webpage.R <results_dir> <output.html> [baseline_dir]")
}

results_dir <- args[1]
output_file <- args[2]
baseline_dir <- if (length(args) >= 3 && nzchar(args[3]) && tolower(args[3]) != "none") {
  args[3]
} else {
  NULL
}

# Validate baseline (silently disable if csv missing).
if (!is.null(baseline_dir)) {
  b_summary_path  <- file.path(baseline_dir, "summary.csv")
  b_funcs_path    <- file.path(baseline_dir, "functions.csv")
  if (!file.exists(b_summary_path) || !file.exists(b_funcs_path)) {
    message(sprintf("Baseline dir '%s' has no summary.csv/functions.csv; ignoring.", baseline_dir))
    baseline_dir <- NULL
  }
}

summary_df   <- read.csv(file.path(results_dir, "summary.csv"),  stringsAsFactors = FALSE)
functions_df <- read.csv(file.path(results_dir, "functions.csv"), stringsAsFactors = FALSE)
crashes_df   <- read.csv(file.path(results_dir, "crashes.csv"),  stringsAsFactors = FALSE)

b_summary_df   <- if (!is.null(baseline_dir)) read.csv(file.path(baseline_dir, "summary.csv"),  stringsAsFactors = FALSE) else NULL
b_functions_df <- if (!is.null(baseline_dir)) read.csv(file.path(baseline_dir, "functions.csv"), stringsAsFactors = FALSE) else NULL

# Written by the TypR-era parser only; NULL for a NativeSem-era results dir.
read_opt_csv <- function(dir, name) {
  path <- file.path(dir, name)
  if (file.exists(path)) read.csv(path, stringsAsFactors = FALSE) else NULL
}
links_df        <- read_opt_csv(results_dir, "links.csv")
entry_points_df <- read_opt_csv(results_dir, "entry_points.csv")

# --- Column aliases ---
# The CSV schema grows additively. NativeSem-era runs carry only the native
# columns; TypR runs keep those names (still meaning native) and add r_*
# (strict) and rg_* (gradual) columns beside them, plus side/mode on
# functions.csv. Everything below reads through these aliases, so one script
# renders either kind of results dir, against a baseline of either kind.
getcol <- function(df, name, default = NA) {
  if (!is.null(df) && name %in% names(df)) df[[name]] else rep(default, NROW(df))
}

native_cols <- function(df) {
  ep_call     <- getcol(df, "ep_call", 0L)
  ep_c        <- getcol(df, "ep_c", 0L)
  ep_fortran  <- getcol(df, "ep_fortran", 0L)
  ep_external <- getcol(df, "ep_external", 0L)
  list(
    n          = getcol(df, "n_functions", 0L),
    typed      = getcol(df, "n_typed", 0L),
    untypeable = getcol(df, "n_untypeable", 0L),
    timeout    = getcol(df, "n_timeout", 0L),
    ep_call = ep_call, ep_c = ep_c, ep_fortran = ep_fortran, ep_external = ep_external,
    # Distinct entry-point symbols. Older runs only have the per-convention
    # counts, whose sum counts a symbol twice when it is reached under two
    # conventions.
    ep         = if (!is.null(df) && "n_ep" %in% names(df)) df$n_ep
                 else ep_call + ep_c + ep_fortran + ep_external,
    ep_typed   = getcol(df, "n_ep_typed", 0L),
    # TypR-era only (NA before).
    ep_reached = getcol(df, "n_ep_reached", NA_integer_),
    ep_linked  = getcol(df, "n_ep_linked", NA_integer_),
    from_c_sig = getcol(df, "n_from_c_sig", NA_integer_),
    n_c_files  = getcol(df, "n_c_files", NA_integer_)
  )
}

# R definitions (r_*: strict run, rg_*: gradual run). All NA on a
# NativeSem-era run, which is what HAVE_R tests.
r_cols <- function(df) {
  list(
    n               = getcol(df, "r_n_defs", NA_integer_),
    typed           = getcol(df, "r_n_typed", NA_integer_),
    untypeable      = getcol(df, "r_n_untypeable", NA_integer_),
    timeout         = getcol(df, "r_n_timeout", NA_integer_),
    skipped         = getcol(df, "r_n_skipped", NA_integer_),
    unknown         = getcol(df, "r_n_unknown", NA_integer_),
    stmts           = getcol(df, "r_n_stmts", NA_integer_),
    unbound_prelude = getcol(df, "r_n_unbound_prelude", NA_integer_),
    unbound_native  = getcol(df, "r_n_unbound_native", NA_integer_),
    files_unparsed  = getcol(df, "r_n_files_unparsed", NA_integer_),
    g_typed         = getcol(df, "rg_n_typed", NA_integer_),
    g_untypeable    = getcol(df, "rg_n_untypeable", NA_integer_),
    g_timeout       = getcol(df, "rg_n_timeout", NA_integer_)
  )
}

# Every pre-TypR function row is a native function from the one (strict) run.
# `degenerate` (result type is `empty`) is NA there: it is then recomputed
# from the type string, which only works for native types.
backfill_functions <- function(df) {
  if (is.null(df)) return(NULL)
  df$side <- getcol(df, "side", "native")
  df$mode <- getcol(df, "mode", "strict")
  df$degenerate <- getcol(df, "degenerate", NA)
  df$anonymous <- getcol(df, "anonymous", FALSE)
  for (col in c("timing_sec", "file", "line", "annotated", "error_name",
                "unbound_kind", "error_line"))
    df[[col]] <- getcol(df, col, NA)
  df
}
functions_df   <- backfill_functions(functions_df)
b_functions_df <- backfill_functions(b_functions_df)

nat   <- native_cols(summary_df)
rs    <- r_cols(summary_df)
b_nat <- if (!is.null(b_summary_df)) native_cols(b_summary_df) else NULL
HAVE_NATIVE <- any(nat$n > 0, na.rm = TRUE)
HAVE_R      <- any(!is.na(rs$n))

# Baseline matching key: package, side, file, name, occurrence, mode.
# `occurrence` numbers repeated rows in order within a mode (R files hold many
# anonymous statements). `file` only takes part when both runs record it, so
# a NativeSem-era baseline still matches a TypR run. Mode comes last, so a
# strict key turns into its gradual twin by replacing the suffix.
SEP <- "\x1f"
use_file_key <- !is.null(b_functions_df) &&
  any(!is.na(functions_df$file)) && any(!is.na(b_functions_df$file))
function_keys <- function(df) {
  if (nrow(df) == 0L) return(character())
  file <- if (use_file_key) ifelse(is.na(df$file), "", df$file) else ""
  stem <- paste(df$package, df$side, file, df$function_name, sep = SEP)
  occurrence <- ave(seq_along(stem), stem, df$mode, FUN = seq_along)
  paste(stem, occurrence, df$mode, sep = SEP)
}
gradual_key <- function(key) sub(paste0(SEP, "strict$"), paste0(SEP, "gradual"), key)
functions_df$key <- function_keys(functions_df)
if (!is.null(b_functions_df)) b_functions_df$key <- function_keys(b_functions_df)

# Which C functions are analysed is not fixed: TypR keeps those the entry
# points reach, and all of them when it finds no entry point (e.g. the R side
# did not parse). Raw typed counts would then credit or blame functions for
# entering or leaving the count, so native deltas are taken over the functions
# both runs analysed, and the change of scope is reported apart. One row per
# package present in both runs.
native_scope <- NULL
if (!is.null(b_functions_df)) {
  pkg_levels <- union(summary_df$package, b_summary_df$package)
  typed_by_key <- function(df) {
    nd <- df[df$side == "native" & df$mode == "strict", , drop = FALSE]
    split(setNames(nd$status == "typed", nd$key), factor(nd$package, levels = pkg_levels))
  }
  now_t    <- typed_by_key(functions_df)
  before_t <- typed_by_key(b_functions_df)
  pkgs <- intersect(summary_df$package, b_summary_df$package)
  native_scope <- data.frame(package = pkgs,
    added   = vapply(pkgs, function(p) sum(!(names(now_t[[p]]) %in% names(before_t[[p]]))), integer(1)),
    removed = vapply(pkgs, function(p) sum(!(names(before_t[[p]]) %in% names(now_t[[p]]))), integer(1)),
    typed_now = vapply(pkgs, function(p) {
      a <- now_t[[p]]; sum(a[names(a) %in% names(before_t[[p]])]) }, integer(1)),
    typed_before = vapply(pkgs, function(p) {
      b <- before_t[[p]]; sum(b[names(b) %in% names(now_t[[p]])]) }, integer(1)),
    row.names = pkgs, stringsAsFactors = FALSE)
}
scope_moved <- function(pkg) !is.null(native_scope) && pkg %in% native_scope$package &&
  native_scope[pkg, "added"] + native_scope[pkg, "removed"] > 0L

raw_dir <- "work/raw_output"
sources_root <- "work/sources"
pkg_dir <- file.path(dirname(output_file), "pkg")
dir.create(pkg_dir, recursive = TRUE, showWarnings = FALSE)

# --- Checker version (captured at typecheck time by record_checker_version.sh) ---
read_one_line <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  v <- tryCatch(readLines(path, n = 1L, warn = FALSE), error = function(e) character())
  if (length(v) == 0L || !nzchar(v[1L])) NA_character_ else v[1L]
}
read_all_lines <- function(path) {
  if (!file.exists(path)) return(character())
  tryCatch(readLines(path, warn = FALSE), error = function(e) character())
}
# The four commits a TypR checker was built from (see
# record_checker_version.sh). NULL for a NativeSem-era results dir.
read_provenance <- function(dir) {
  path <- file.path(dir, "provenance.json")
  if (!file.exists(path) || !requireNamespace("jsonlite", quietly = TRUE)) return(NULL)
  tryCatch(jsonlite::fromJSON(path), error = function(e) NULL)
}
load_checker_meta <- function(dir) {
  list(
    version = read_one_line(file.path(dir, "checker_version.txt")),
    subject = read_one_line(file.path(dir, "checker_subject.txt")),
    status  = read_all_lines(file.path(dir, "checker_status.txt")),
    prov    = read_provenance(dir)
  )
}
checker_meta  <- load_checker_meta(results_dir)
b_checker_meta <- if (!is.null(baseline_dir)) load_checker_meta(baseline_dir) else NULL

# Vendor Prism (syntax highlighter) assets next to the generated HTML.
# Sourced from assets/ at repo root; script is expected to run with cwd = repo root.
assets_dst <- file.path(dirname(output_file), "assets")
dir.create(assets_dst, recursive = TRUE, showWarnings = FALSE)
prism_assets <- c(
  "prism.css",
  "prism-core.min.js",
  "prism-clike.min.js",
  "prism-c.min.js",
  "prism-r.min.js"
)
for (f in prism_assets) {
  src <- file.path("assets", f)
  if (file.exists(src)) {
    file.copy(src, file.path(assets_dst, f), overwrite = TRUE)
  } else {
    warning(sprintf("missing dashboard asset %s; syntax highlighting may not work", src))
  }
}

# Asset reference helpers; prefix is "" for index, "../" for per-package pages.
prism_head <- function(prefix = "") sprintf('<link rel="stylesheet" href="%sassets/prism.css">', prefix)
prism_script <- function(prefix = "") {
  paste(sprintf(
    '<script src="%sassets/%s" defer></script>',
    prefix,
    c("prism-core.min.js", "prism-clike.min.js", "prism-c.min.js", "prism-r.min.js")
  ), collapse = "")
}

# --- Aggregate stats ---
n_packages    <- nrow(summary_df)
# A package counts as crashed when either of its runs did.
crashed_any   <- getcol(summary_df, "crashed", 0L) %in% 1 | getcol(summary_df, "rg_crashed", 0L) %in% 1
n_crashed     <- sum(crashed_any)
n_ok          <- n_packages - n_crashed

total_functions  <- sum(nat$n, na.rm = TRUE)
total_typed      <- sum(nat$typed, na.rm = TRUE)
total_untypeable <- sum(nat$untypeable, na.rm = TRUE)
total_timeout    <- sum(nat$timeout, na.rm = TRUE)
total_ep_call    <- sum(nat$ep_call,     na.rm = TRUE)
total_ep_c       <- sum(nat$ep_c,        na.rm = TRUE)
total_ep_fortran <- sum(nat$ep_fortran,  na.rm = TRUE)
total_ep_external<- sum(nat$ep_external, na.rm = TRUE)
total_ep         <- if ("n_ep" %in% names(summary_df)) sum(nat$ep, na.rm = TRUE) else
                      total_ep_call + total_ep_c + total_ep_fortran + total_ep_external
total_ep_typed   <- sum(nat$ep_typed,  na.rm = TRUE)

pct_overall  <- if (total_functions > 0) round(100 * total_typed / total_functions, 1) else 0
pct_ep_typed <- if (total_ep > 0) round(100 * total_ep_typed / total_ep, 1) else 0

# Per-error-category counts (status untypeable+timeout, grouped by error_title;
# all timeouts go into a single "timeout" bucket). One side and mode at a time:
# the native and R error vocabularies are disjoint.
error_category_counts <- function(fdf, side = "native", mode = "strict") {
  fdf <- fdf[fdf$side == side & fdf$mode == mode & !fdf$anonymous, , drop = FALSE]
  if (nrow(fdf) == 0) return(setNames(integer(0), character(0)))
  # A `fallback` function failed inference before being bound at its C
  # signature: its failure is counted too.
  ut <- fdf[fdf$status != "typed", ]
  if (nrow(ut) == 0) return(setNames(integer(0), character(0)))
  cat_label <- ifelse(ut$status == "timeout", "timeout",
               ifelse(ut$status == "skipped", "skipped (TypR caught an exception)",
               ifelse(ut$status == "unknown", "unrecognised failure (no type, no error block)",
               ifelse(is.na(ut$error_title), ut$status, ut$error_title))))
  tbl <- table(cat_label)
  setNames(as.integer(tbl), names(tbl))
}
err_now  <- error_category_counts(functions_df)
err_base <- if (!is.null(b_functions_df)) error_category_counts(b_functions_df) else integer(0)

# error_cats data.frame for the legacy "Error and timeout categories" table
error_cats <- if (length(err_now) > 0) {
  ord <- order(-err_now)
  data.frame(category = names(err_now)[ord], count = err_now[ord], stringsAsFactors = FALSE)
} else {
  data.frame(category = character(0), count = integer(0))
}

# --- HTML helpers ---
esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  x <- gsub(">", "&gt;",  x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

# --- Function types: one overload per line ---
# A function type is often an intersection of arrows, its overloads:
# "∀'a. ([A] -> B) & ([C] -> D)", or for an R closure
# "∀'a. (((x: A) -> B) & ((x: C) -> D))<> with { }". Printed as one string, the
# overloads are hard to tell apart; type_sig_html shades them alternately and
# puts the top-level & in bold. The type is split at the &s of
# nesting depth 0 (counting (), [] and {}, not <>: "->" contains a ">"), and
# only when every part is a parenthesized arrow; any other type is printed as
# is.
split_top_and <- function(s) {
  ch <- strsplit(s, "")[[1]]
  depth <- 0L; start <- 1L; parts <- character(); i <- 1L; n <- length(ch)
  while (i <= n) {
    c <- ch[i]
    if (c %in% c("(", "[", "{")) depth <- depth + 1L
    else if (c %in% c(")", "]", "}")) depth <- depth - 1L
    else if (depth == 0L && c == "&" && i > 1L && i < n && ch[i - 1L] == " " && ch[i + 1L] == " ") {
      parts <- c(parts, paste(ch[start:(i - 2L)], collapse = ""))
      start <- i + 2L
    }
    i <- i + 1L
  }
  c(parts, paste(ch[start:n], collapse = ""))
}
# The position of the parenthesis closing the one at position 1, or NA.
closing_paren <- function(ch) {
  depth <- 0L
  for (i in seq_along(ch)) {
    if (ch[i] %in% c("(", "[", "{")) depth <- depth + 1L
    else if (ch[i] %in% c(")", "]", "}")) {
      depth <- depth - 1L
      if (depth == 0L) return(i)
    }
  }
  NA_integer_
}
is_paren_arrow <- function(p) {
  ch <- strsplit(p, "")[[1]]
  length(ch) > 1L && ch[1] == "(" && identical(closing_paren(ch), length(ch)) && grepl("->", p, fixed = TRUE)
}
# The arrow of a function type, "D -> R" or "D --> R": the first one at
# nesting depth 0 (an arrow in a parameter type is nested), in bold, and the
# return type R marked, so that it can be found at a glance. A type that is
# not an arrow at depth 0 is tried inside its outer parentheses, as in the
# R form "((x: A) -> R)<> with { }"; anything else is printed as is.
arrow_html <- function(s) {
  ch <- strsplit(s, "")[[1]]
  n <- length(ch); depth <- 0L; i <- 1L
  while (i < n) {
    c <- ch[i]
    if (c %in% c("(", "[", "{")) depth <- depth + 1L
    else if (c %in% c(")", "]", "}")) depth <- depth - 1L
    else if (depth == 0L && c == "-" && ch[i + 1L] %in% c(">", "-")) {
      len <- if (ch[i + 1L] == ">") 2L else if (i + 2L <= n && ch[i + 2L] == ">") 3L else 0L
      if (len > 0L) {
        dom <- if (i > 1L) paste(ch[1:(i - 1L)], collapse = "") else ""
        arr <- paste(ch[i:(i + len - 1L)], collapse = "")
        ret <- if (i + len <= n) paste(ch[(i + len):n], collapse = "") else ""
        return(sprintf('%s<b class="ty-arrow">%s</b><span class="ty-ret">%s</span>',
                       esc(dom), esc(arr), esc(ret)))
      }
    }
    i <- i + 1L
  }
  if (n > 2L && ch[1] == "(") {
    j <- closing_paren(ch)
    if (!is.na(j) && j > 2L) {
      inner <- paste(ch[2:(j - 1L)], collapse = "")
      if (grepl("->", inner, fixed = TRUE))
        return(paste0("(", arrow_html(inner), ")",
                      if (j < n) esc(paste(ch[(j + 1L):n], collapse = "")) else ""))
    }
  }
  esc(s)
}

type_sig_html <- function(s) {
  if (is.na(s) || !nzchar(s)) return(esc(s))
  quant <- ""; rest <- s
  m <- regmatches(s, regexec("^(\u2200[^.]*\\.) (.*)$", s))[[1]]
  if (length(m) == 3L) { quant <- m[2]; rest <- m[3] }
  open <- ""; close <- ""
  parts <- split_top_and(rest)
  if (length(parts) < 2L && startsWith(rest, "(")) {
    ch <- strsplit(rest, "")[[1]]
    j <- closing_paren(ch)
    if (!is.na(j) && j > 2L) {
      inner <- paste(ch[2:(j - 1L)], collapse = "")
      inner_parts <- split_top_and(inner)
      if (length(inner_parts) >= 2L) {
        parts <- inner_parts; open <- "("
        close <- paste0(")", if (j < length(ch)) paste(ch[(j + 1L):length(ch)], collapse = "") else "")
      }
    }
  }
  if (length(parts) < 2L || !all(vapply(parts, is_paren_arrow, logical(1))))
    return(paste0(if (nzchar(quant)) sprintf('<span class="ovl-quant">%s</span> ', esc(quant)) else "",
                  arrow_html(rest)))
  ovls <- vapply(seq_along(parts), function(k) {
    p <- parts[k]
    cls <- c("ovl", if (k %% 2L == 0L) "ovl-alt", if (grepl("-> empty\\)$", p)) "ovl-err")
    sprintf('<span class="%s">%s</span>', paste(cls, collapse = " "), arrow_html(p))
  }, character(1))
  paste0(if (nzchar(quant)) sprintf('<span class="ovl-quant">%s</span> ', esc(quant)) else "",
         if (nzchar(open)) esc(open) else "",
         paste(ovls, collapse = '<b class="ovl-and">&amp;</b>'),
         if (nzchar(close)) sprintf('<span class="ovl-close">%s</span>', esc(close)) else "")
}

pct_bar <- function(typed, total, empty_label = "no functions analysed") {
  if (is.na(total) || total == 0) return(sprintf('<span class="na">%s</span>', empty_label))
  pct <- round(100 * typed / total, 1)
  hue <- round(pct * 1.2)  # 0=red, 120=green
  sprintf(
    '<div class="bar-bg"><div class="bar-fill" style="width:%.1f%%;background:hsl(%d,70%%,45%%)"></div><span class="bar-label">%d / %d (%.1f%%)</span></div>',
    pct, hue, typed, total, pct
  )
}

# good_dir = +1 (higher is better) or -1 (lower is better); fmt formats the
# magnitude (e.g. "%.1f%%" for percentage points).
delta_html <- function(delta, good_dir = 1, fmt = "%d") {
  if (is.na(delta)) return('<span class="delta delta-na">&mdash;</span>')
  if (delta == 0)  return('<span class="delta delta-zero">0</span>')
  good <- (delta > 0 && good_dir > 0) || (delta < 0 && good_dir < 0)
  cls <- if (good) "delta-good" else "delta-bad"
  sign <- if (delta > 0) "+" else "" # negative numbers already include their sign
  sprintf(paste0('<span class="delta %s">%s', fmt, '</span>'), cls, sign, delta)
}

# Format a per-function timing value (seconds) for the dashboard. The runner
# emits 6 decimals which is too noisy; show 3 digits below 1s, 2 above.
fmt_func_timing <- function(t) {
  if (is.na(t)) return("&mdash;")
  if (t < 1) sprintf("%.3f s", t) else sprintf("%.2f s", t)
}

# --- "empty return type" detection ---
# A typed function whose *result* type is unconditionally `empty` (bottom) is a
# red flag: inference concluded the function never produces a usable value. We
# only flag when *every* top-level arrow returns `empty` (so a refinement like
# `((A -> ()) & (B -> empty))`, where only the null-input branch is bottom, is
# NOT flagged).

# Strip one or more layers of parentheses that wrap the whole string. Brackets
# {} and [] count toward depth so we don't mis-peel `({...})`.
peel_outer_parens <- function(s) {
  s <- trimws(s)
  repeat {
    if (nchar(s) < 2L || substr(s, 1L, 1L) != "(") break
    chars <- strsplit(s, "", fixed = TRUE)[[1]]
    depth <- 0L; matched_at <- -1L
    for (i in seq_along(chars)) {
      c <- chars[i]
      if (c == "(" || c == "{" || c == "[") depth <- depth + 1L
      else if (c == ")" || c == "}" || c == "]") {
        depth <- depth - 1L
        if (depth == 0L) { matched_at <- i; break }
      }
    }
    if (matched_at == nchar(s)) s <- trimws(substr(s, 2L, nchar(s) - 1L)) else break
  }
  s
}

returns_only_empty <- function(sig) {
  if (is.na(sig) || !nzchar(trimws(sig))) return(FALSE)
  s0 <- gsub("<\\.\\.\\.>", "", sig)        # drop recursive-type markers
  s0 <- sub("^∀[^.]*\\.\\s*", "", s0)  # strip leading `∀'a.` quantifier
  rec <- function(s) {
    s <- peel_outer_parens(s)
    if (!nzchar(s)) return(FALSE)
    chars <- strsplit(s, "", fixed = TRUE)[[1]]
    n <- length(chars); depth <- 0L
    arrows <- integer(0); seps <- integer(0); i <- 1L
    while (i <= n) {
      c <- chars[i]
      if (c == "(" || c == "{" || c == "[") depth <- depth + 1L
      else if (c == ")" || c == "}" || c == "]") depth <- depth - 1L
      else if (depth == 0L && c == "-" && i < n && chars[i + 1L] == ">") {
        arrows <- c(arrows, i); i <- i + 1L
      } else if (depth == 0L && (c == "&" || c == "|")) seps <- c(seps, i)
      i <- i + 1L
    }
    # Top-level union/intersection: every operand must return empty.
    if (length(seps) > 0L) {
      bounds <- c(0L, seps, n + 1L)
      for (k in seq_len(length(bounds) - 1L)) {
        if (!rec(substr(s, bounds[k] + 1L, bounds[k + 1L] - 1L))) return(FALSE)
      }
      return(TRUE)
    }
    if (length(arrows) == 0L) return(identical(trimws(s), "empty"))
    last <- arrows[length(arrows)]
    identical(trimws(peel_outer_parens(substr(s, last + 2L, n))), "empty")
  }
  rec(s0)
}

# Typed with an `empty` result. TypR computes it on the type itself
# (`degenerate`); older results only have the type string, and the parser
# above, which only understands native types.
is_empty_ret <- function(fdf) {
  if (nrow(fdf) == 0L) return(logical())
  typed <- fdf$status == "typed"
  deg <- as.logical(fdf$degenerate)
  legacy <- is.na(deg)
  deg[legacy] <- vapply(fdf$type_sig[legacy], returns_only_empty, logical(1), USE.NAMES = FALSE)
  typed & deg
}

# --- Status-change classification (vs baseline) ---
# Rank statuses so we can colour a transition as an improvement or regression.
# `fallback` (bound at its declared C signature after inference failed) ranks
# with the failures: inference did fail, and NativeSem-era results recorded
# the same functions as `untypeable`, so that transition is a relabelling, not
# progress. `skipped`/`unparseable` were never attempted, so they rank with
# absence.
status_rank <- function(s) {
  switch(s, "typed" = 2L, "fallback" = 1L, "timeout" = 1L, "untypeable" = 1L,
         "internal" = 1L, "skipped" = 0L, "unparseable" = 0L, 0L)
}

# Returns a list(kind, html) describing how `now` differs from `before`.
#   kind: "new" | "changed" | "unchanged"
# `before` is NA when the function was absent from the baseline.
change_info <- function(now, before) {
  if (is.na(before)) {
    return(list(kind = "new",
      html = ' <span class="badge badge-change-new" title="not present in the baseline run">new</span>'))
  }
  if (identical(now, before)) return(list(kind = "unchanged", html = ""))
  rn <- status_rank(now); rb <- status_rank(before)
  cls <- if (rn > rb) "badge-change-good" else if (rn < rb) "badge-change-bad" else "badge-change-neutral"
  list(kind = "changed",
    html = sprintf(' <span class="badge %s" title="status changed since the baseline run">%s &rarr; %s</span>',
                   cls, esc(before), esc(now)))
}

# Inline checker version (e.g. `fa6d98c0-dirty`). Renders nothing when the
# version file is missing (older results regenerated after the fact).
PROV_REPOS <- c(typr = "PRL-PRG/typr", rsem = "E-Sh4rk/Rsem",
                nativesem = "programLyrique/nativesem", rstt = "E-Sh4rk/rstt")
PROV_LABELS <- c(typr = "TypR", rsem = "Rsem", nativesem = "NativeSem", rstt = "rstt")

# TypR: the four commits, each linked, and whether the submodules were the
# ones typr pins. Otherwise the inline checker version (e.g.
# `fa6d98c0-dirty`), or nothing when the version file is missing (older
# results regenerated after the fact).
checker_version_html <- function(meta = checker_meta) {
  prov <- meta$prov
  if (!is.null(prov)) {
    parts <- vapply(names(PROV_REPOS), function(k) {
      sha <- prov[[k]]
      if (is.null(sha) || is.na(sha) || !nzchar(sha)) return("")
      sprintf('%s <a href="https://github.com/%s/commit/%s"><code>%s</code></a>',
              PROV_LABELS[[k]], PROV_REPOS[[k]], esc(sha), esc(substr(sha, 1L, 7L)))
    }, character(1))
    dirty <- if (!is.na(meta$version) && endsWith(meta$version, "-dirty"))
      ' <span class="badge badge-dirty" title="the typr checkout had uncommitted changes">dirty</span>' else ""
    pinned <- if (isTRUE(as.logical(prov$pinned)))
      '<span class="badge badge-pinned" title="submodules at the commits typr pins: a reproducible build">pinned</span>'
    else
      '<span class="badge badge-unpinned" title="submodules refreshed to their branch tips before building">unpinned</span>'
    return(sprintf(' &middot; %s %s%s', paste(parts[nzchar(parts)], collapse = " &middot; "), pinned, dirty))
  }
  if (is.null(meta$version) || is.na(meta$version)) return("")
  tip <- if (!is.na(meta$subject)) sprintf(' title="%s"', esc(meta$subject)) else ""
  sprintf(' &middot; checker <code%s>%s</code>', tip, esc(meta$version))
}

# Inline list of uncommitted changes; `git status --porcelain` codes are
# translated to readable labels. First `max_inline` entries render directly;
# any remainder lives inside an expandable <details>. Emitted as a <div> so
# the (block-level) <details> is valid HTML. Returns "" when clean.
porcelain_label <- function(code) {
  switch(code,
    "??" = "untracked",
    "!!" = "ignored",
    "M " = , " M" = , "MM" = "modified",
    "A " = , "AM" = "added",
    "D " = , " D" = "deleted",
    "R " = , "RM" = "renamed",
    "C " = "copied",
    "U " = , "UU" = "conflict",
    trimws(code)
  )
}

checker_status_html <- function(meta = checker_meta, max_inline = 8L) {
  lines <- meta$status
  lines <- lines[nzchar(lines)]
  if (length(lines) == 0L) return("")

  fmt_one <- function(l) {
    code <- substr(l, 1L, 2L)
    path <- substr(l, 4L, nchar(l))
    label <- porcelain_label(code)
    cls <- paste0("dirty-", gsub("[^a-z]", "", tolower(label)))
    sprintf('<code class="%s"><span class="dirty-tag">%s</span> %s</code>',
            cls, esc(label), esc(path))
  }
  formatted <- vapply(lines, fmt_one, character(1), USE.NAMES = FALSE)
  total <- length(formatted)

  if (total <= max_inline) {
    return(sprintf('<div class="dirty-files">uncommitted (%d): %s</div>',
                   total, paste(formatted, collapse = " ")))
  }
  visible <- formatted[seq_len(max_inline)]
  hidden  <- formatted[(max_inline + 1L):total]
  sprintf(
    '<div class="dirty-files">uncommitted (%d): %s <details class="dirty-more"><summary>show %d more</summary><div class="dirty-rest">%s</div></details></div>',
    total,
    paste(visible, collapse = " "),
    length(hidden),
    paste(hidden, collapse = " ")
  )
}

progress_card <- function(label, before, after, good_dir = 1, fmt = "%d") {
  delta <- after - before
  sprintf(
    '<div class="stat-card progress"><div class="value">%s</div><div class="label">%s</div><div class="progress-line">%s &rarr; %s %s</div></div>',
    sprintf(fmt, after), esc(label), sprintf(fmt, before), sprintf(fmt, after),
    delta_html(delta, good_dir, fmt)
  )
}

# --- Source extraction (ctags) ---
# Uses ctags default (u-ctags) tab-separated output, parsed with regex. No
# JSON dependency. Format:
#   name<TAB>path<TAB>/^pattern$/;"<TAB>kind<TAB>line:N<TAB>...<TAB>end:N
ctags_available <- function() {
  bin <- Sys.which("ctags")
  nzchar(bin)
}
HAVE_CTAGS <- ctags_available()
if (!HAVE_CTAGS) {
  msg <- "ctags not in PATH; source snippets disabled."
  if (identical(Sys.getenv("CI"), "true")) stop(msg)
  message(msg)
}

# Cache: pkg -> data.frame(name, path, line, end)
ctags_cache <- new.env(parent = emptyenv())

extract_field <- function(parts, key) {
  # parts: character vector of trailing fields like "line:36", "end:40".
  hit <- grep(paste0("^", key, ":"), parts, value = TRUE)
  if (length(hit) == 0) return(NA_character_)
  sub(paste0("^", key, ":"), "", hit[1])
}

ctags_for_pkg <- function(pkg) {
  if (!HAVE_CTAGS) return(NULL)
  if (!is.null(ctags_cache[[pkg]])) return(ctags_cache[[pkg]])
  src <- file.path(sources_root, pkg, "src")
  if (!dir.exists(src)) {
    ctags_cache[[pkg]] <- data.frame()
    return(ctags_cache[[pkg]])
  }
  src_abs <- normalizePath(src, mustWork = FALSE)
  out <- tryCatch(
    suppressWarnings(system2("ctags",
      c("--languages=C,C++",
        "--kinds-C=f", "--kinds-C++=f", "--fields=+ne",
        "-R", "-f", "-", src_abs),
      stdout = TRUE, stderr = FALSE)),
    error = function(e) character()
  )
  if (length(out) == 0) {
    ctags_cache[[pkg]] <- data.frame()
    return(ctags_cache[[pkg]])
  }
  # Drop any leading non-tag lines (warnings ought to go to stderr but be safe).
  out <- out[!grepl("^!_TAG", out) & nzchar(out)]
  fields <- strsplit(out, "\t", fixed = TRUE)
  ok     <- vapply(fields, function(f) length(f) >= 4L, logical(1))
  fields <- fields[ok]
  if (length(fields) == 0) {
    ctags_cache[[pkg]] <- data.frame()
    return(ctags_cache[[pkg]])
  }
  name  <- vapply(fields, `[`, character(1), 1L)
  path  <- vapply(fields, `[`, character(1), 2L)
  trail <- lapply(fields, function(f) f[5:length(f)])
  line  <- as.integer(vapply(trail, extract_field, character(1), "line"))
  endL  <- as.integer(vapply(trail, extract_field, character(1), "end"))
  df <- data.frame(name = name, path = path, line = line, end = endL, stringsAsFactors = FALSE)
  df <- df[!is.na(df$name) & !is.na(df$line), , drop = FALSE]
  # Keep first definition for each name (static helpers across files).
  df <- df[!duplicated(df$name), , drop = FALSE]
  ctags_cache[[pkg]] <- df
  df
}

# Returns NULL when no snippet is available; otherwise list(file, snippet).
source_snippet <- function(pkg, name, max_lines = 200) {
  tags <- ctags_for_pkg(pkg)
  if (is.null(tags) || nrow(tags) == 0) return(NULL)
  hit <- tags[tags$name == name, , drop = FALSE]
  if (nrow(hit) == 0) return(NULL)
  fp <- hit$path[1]
  if (!file.exists(fp)) return(NULL)
  lines <- tryCatch(readLines(fp, warn = FALSE), error = function(e) character())
  if (length(lines) == 0) return(NULL)
  start_l <- hit$line[1]
  end_l   <- hit$end[1]
  if (is.na(end_l) || end_l < start_l) {
    cap_l <- min(start_l + max_lines, length(lines))
    found_close <- FALSE
    for (i in seq(start_l, cap_l)) {
      if (grepl("^\\}", lines[i])) {
        end_l <- i; found_close <- TRUE; break
      }
    }
    if (!found_close) end_l <- cap_l
  }
  end_l <- min(end_l, length(lines))
  lang <- if (grepl("\\.(cpp|cc|cxx|hpp|hxx)$", fp, ignore.case = TRUE)) "cpp" else "c"
  list(file = fp, line = start_l, lang = lang,
       snippet = paste(lines[start_l:end_l], collapse = "\n"))
}

# --- Entry-point name + calling-convention extraction ---
# Returns a data.frame with one row per (name, convention) pair. The same name
# may appear under multiple conventions in pathological cases; the renderer
# de-dupes when emitting badges.
parse_ep_table <- function(out_path) {
  empty <- data.frame(name = character(), convention = character(), stringsAsFactors = FALSE)
  if (!file.exists(out_path)) return(empty)
  lines <- readLines(out_path, warn = FALSE)
  ep_header_idx <- grep("^Entry points for \\.", lines)
  if (length(ep_header_idx) == 0L) return(empty)
  names_acc <- character()
  conv_acc <- character()
  for (hi in ep_header_idx) {
    conv <- sub("^Entry points for (\\.[A-Za-z]+) convention:$", "\\1", lines[hi])
    j <- hi + 1L
    while (j <= length(lines) && grepl("^\\s+\\S", lines[j])) {
      names_acc <- c(names_acc, trimws(lines[j]))
      conv_acc <- c(conv_acc, conv)
      j <- j + 1L
    }
  }
  unique(data.frame(name = names_acc, convention = conv_acc, stringsAsFactors = FALSE))
}

# Convention(s) for a given function name, ordered to match R's calling-
# convention listing. Returns character(0) when the name is not an entry point.
ep_conv_for <- function(ep_tbl, name) {
  if (nrow(ep_tbl) == 0L) return(character())
  hits <- ep_tbl$convention[ep_tbl$name == name]
  if (length(hits) == 0L) return(character())
  conv_order <- c(".Call", ".C", ".Fortran", ".External")
  hits <- unique(hits)
  hits[order(match(hits, conv_order), hits)]
}

# --- Common HTML head/style ---
common_style <- function() {
  paste0(
    '<style>',
    ':root { --bg: #fafafa; --card: #fff; --border: #e0e0e0; --text: #1a1a1a; --muted: #666; --accent: #2266cc; --good: #166534; --bad: #991b1b; --good-bg: #dcfce7; --bad-bg: #fee2e2; }',
    '* { box-sizing: border-box; margin: 0; padding: 0; }',
    'body { font-family: "Inter", system-ui, -apple-system, sans-serif; background: var(--bg); color: var(--text); line-height: 1.5; padding: 2rem 1rem; max-width: 1400px; margin: 0 auto; }',
    'h1 { font-size: 2rem; margin-bottom: .25rem; }',
    'h1 .r { color: var(--accent); }',
    'h1 a { color: inherit; text-decoration: none; }',
    '.subtitle { color: var(--muted); margin-bottom: 2rem; }',
    '.subtitle:has(+ .dirty-files) { margin-bottom: .25rem; }',
    '.subtitle code { font-family: "JetBrains Mono", monospace; font-size: .85rem; background: #f0f0f0; padding: .1em .35em; border-radius: 3px; color: var(--text); }',
    '.dirty-files { color: var(--muted); font-size: .8rem; margin-bottom: 2rem; line-height: 1.7; }',
    '.dirty-files code { font-family: "JetBrains Mono", monospace; font-size: .75rem; padding: .1em .4em; border-radius: 3px; margin-right: .15rem; white-space: nowrap; }',
    '.dirty-files .dirty-tag { font-weight: 600; margin-right: .35em; opacity: .8; }',
    '.dirty-files .dirty-untracked { background: #fef3c7; color: #92400e; }',
    '.dirty-files .dirty-modified  { background: #dbeafe; color: #1e40af; }',
    '.dirty-files .dirty-added     { background: #dcfce7; color: #166534; }',
    '.dirty-files .dirty-deleted   { background: #fee2e2; color: #991b1b; }',
    '.dirty-files .dirty-renamed   { background: #ede9fe; color: #5b21b6; }',
    '.dirty-files details { display: inline; }',
    '.dirty-files details summary { display: inline; cursor: pointer; color: var(--accent); list-style: none; user-select: none; }',
    '.dirty-files details summary::-webkit-details-marker { display: none; }',
    '.dirty-files details summary::before { content: "▸ "; }',
    '.dirty-files details[open] summary::before { content: "▾ "; }',
    '.dirty-files details summary:hover { text-decoration: underline; }',
    '.dirty-files .dirty-rest { margin-top: .35rem; padding-left: 1rem; }',
    '.stats { display: grid; grid-template-columns: repeat(auto-fit, minmax(180px, 1fr)); gap: 1rem; margin-bottom: 2rem; }',
    '.stat-card { background: var(--card); border: 1px solid var(--border); border-radius: 8px; padding: 1rem 1.25rem; }',
    '.stat-card .value { font-size: 1.75rem; font-weight: 700; }',
    '.stat-card .label { color: var(--muted); font-size: .85rem; }',
    '.stat-card.progress .progress-line { font-size: .85rem; color: var(--muted); margin-top: .25rem; font-family: "JetBrains Mono", monospace; }',
    'h2 { font-size: 1.25rem; margin: 2rem 0 1rem; }',
    'h2 small { font-weight: 400; color: var(--muted); font-size: .85rem; margin-left: .5rem; }',
    'table { width: 100%; border-collapse: collapse; background: var(--card); border: 1px solid var(--border); border-radius: 8px; overflow: hidden; }',
    'th, td { text-align: left; padding: .6rem .8rem; border-bottom: 1px solid var(--border); vertical-align: top; }',
    'th { background: #f5f5f5; font-size: .85rem; font-weight: 600; text-transform: uppercase; letter-spacing: .03em; color: var(--muted); }',
    'tr:last-child td { border-bottom: none; }',
    'td.pkg { font-weight: 600; font-family: "JetBrains Mono", "Fira Code", monospace; }',
    'td.pkg a { color: var(--accent); text-decoration: none; }',
    'td.pkg a:hover { text-decoration: underline; }',
    'td.num { text-align: right; font-family: "JetBrains Mono", monospace; }',
    '.bar-bg { background: #eee; border-radius: 4px; height: 22px; position: relative; overflow: hidden; min-width: 120px; }',
    '.bar-fill { height: 100%; border-radius: 4px; transition: width .3s; }',
    '.bar-label { position: absolute; top: 0; left: 8px; line-height: 22px; font-size: .8rem; font-weight: 600; color: #333; white-space: nowrap; }',
    '.na { color: var(--muted); font-size: .85rem; font-style: italic; }',
    '.badge { display: inline-block; font-size: .75rem; font-weight: 600; padding: .15em .5em; border-radius: 4px; }',
    '.badge-ok { background: var(--good-bg); color: var(--good); }',
    '.badge-typed { background: var(--good-bg); color: var(--good); }',
    '.badge-untypeable { background: var(--bad-bg); color: var(--bad); }',
    '.badge-timeout { background: #fef3c7; color: #92400e; }',
    '.badge-crash { background: var(--bad-bg); color: var(--bad); }',
    '.badge-entry { background: #e0e7ff; color: #3730a3; }',
    '.badge-conv { background: #ccfbf1; color: #115e59; font-family: "JetBrains Mono", monospace; }',
    '.badge-empty { background: #fde68a; color: #92400e; }',
    '.badge-change-new { background: #e0e7ff; color: #3730a3; }',
    '.badge-change-good { background: var(--good-bg); color: var(--good); font-family: "JetBrains Mono", monospace; }',
    '.badge-change-bad  { background: var(--bad-bg);  color: var(--bad); font-family: "JetBrains Mono", monospace; }',
    '.badge-change-neutral { background: #f0f0f0; color: var(--muted); font-family: "JetBrains Mono", monospace; }',
    '.filters { display: flex; gap: 1rem; align-items: center; flex-wrap: wrap; padding: .75rem 1rem; background: #fff; border: 1px solid var(--border); border-radius: 8px; margin-bottom: 1rem; position: sticky; top: 0; z-index: 10; }',
    '.filters label { font-size: .9rem; user-select: none; }',
    '.filters input[type="search"] { padding: .25rem .5rem; border: 1px solid var(--border); border-radius: 4px; font: inherit; min-width: 12rem; }',
    '.filters .count { margin-left: auto; color: var(--muted); font-family: "JetBrains Mono", monospace; font-size: .85rem; }',
    '.delta { font-family: "JetBrains Mono", monospace; font-weight: 600; padding: .1em .4em; border-radius: 4px; font-size: .85rem; }',
    '.delta-good { background: var(--good-bg); color: var(--good); }',
    '.delta-bad  { background: var(--bad-bg);  color: var(--bad); }',
    '.delta-zero { color: var(--muted); }',
    '.delta-na   { color: var(--muted); }',
    '.crash-msg { font-family: monospace; font-size: .85rem; color: var(--bad); }',
    '.phase-legend { color: var(--muted); font-size: .85rem; margin: .5rem 0 1rem; max-width: 60rem; }',
    '.total-row td { background: #fffbeb; font-weight: 700; font-size: 1.05rem; border-bottom: 2px solid #fcd34d; }',
    '.total-row td:first-child { color: #92400e; text-transform: uppercase; letter-spacing: .03em; font-size: .9rem; }',
    '.phase-legend code { font-family: "JetBrains Mono", monospace; font-size: .8rem; background: #f0f0f0; padding: .05em .3em; border-radius: 3px; }',
    '.error-row td:first-child { font-family: monospace; font-size: .9rem; }',
    '.ep-breakdown { font-size: .85rem; color: var(--muted); }',
    '.ep-breakdown code { font-family: "JetBrains Mono", monospace; font-size: .8rem; }',
    '.func-list { background: var(--card); border: 1px solid var(--border); border-radius: 8px; padding: 1rem; margin-bottom: 1rem; }',
    '.func-list h3 { font-family: "JetBrains Mono", monospace; font-size: 1rem; margin-bottom: .25rem; }',
    '.type-sig { font-family: "JetBrains Mono", monospace; font-size: .85rem; color: var(--text); white-space: pre-wrap; word-break: break-word; }',
    '.type-sig .ovl { background: #dbeafe; border-radius: 3px; padding: 0 .15rem; box-decoration-break: clone; -webkit-box-decoration-break: clone; }',
    '.type-sig .ovl-alt { background: #fef3c7; }',
    '.type-sig .ovl-err { opacity: .6; }',
    '.type-sig .ovl-and { color: var(--accent); margin: 0 .3rem; }',
    '.type-sig .ovl-quant, .type-sig .ovl-close { color: var(--muted); }',
    '.type-sig .ty-arrow { color: #b45309; font-weight: 800; margin: 0 .1rem; }',
    '.type-sig .ty-ret { font-weight: 600; }',
    '.timing { font-family: "JetBrains Mono", monospace; font-size: .8rem; color: var(--muted); margin-top: .25rem; }',
    '.error-line { font-family: "JetBrains Mono", monospace; font-size: .85rem; color: var(--bad); white-space: pre-wrap; }',
    '.error-detail pre { font-family: "JetBrains Mono", monospace; font-size: .8rem; background: #f8f8f8; padding: .5rem; border-radius: 4px; overflow-x: auto; white-space: pre-wrap; }',
    'details { margin-top: .5rem; }',
    'details summary { cursor: pointer; color: var(--accent); font-size: .85rem; user-select: none; }',
    'details summary:hover { text-decoration: underline; }',
    'details pre.source { font-family: "JetBrains Mono", monospace; font-size: .8rem; background: #f8f8f8; padding: .5rem; border-radius: 4px; overflow-x: auto; line-height: 1.4; margin-top: .25rem; max-height: 600px; overflow-y: auto; }',
    'details pre.source code[class*="language-"] { font-family: inherit; font-size: inherit; background: transparent; text-shadow: none; }',
    'details .source-meta { color: var(--muted); font-size: .75rem; margin-top: .25rem; }',
    '.back-link { display: inline-block; margin-bottom: 1rem; color: var(--accent); text-decoration: none; font-size: .9rem; }',
    '.back-link:hover { text-decoration: underline; }',
    'footer { margin-top: 3rem; text-align: center; color: var(--muted); font-size: .8rem; }',
    '.badge-fallback { background: #e0f2fe; color: #075985; }',
    '.badge-pinned { background: var(--good-bg); color: var(--good); }',
    '.badge-unpinned { background: #fef3c7; color: #92400e; }',
    '.badge-dirty { background: var(--bad-bg); color: var(--bad); }',
    '.subtitle a { color: inherit; }',
    '.howto { background: var(--card); border: 1px solid var(--border); border-radius: 8px; padding: .5rem 1rem; margin: -1rem 0 2rem; font-size: .9rem; }',
    '.howto p { margin: .5rem 0; max-width: 60rem; }',
    '.howto code { font-family: "JetBrains Mono", monospace; font-size: .8rem; background: #f0f0f0; padding: .05em .3em; border-radius: 3px; }',
    '.stats.secondary { grid-template-columns: repeat(auto-fit, minmax(150px, 1fr)); margin-top: -1rem; }',
    '.stats.secondary .stat-card { background: transparent; padding: .5rem .9rem; }',
    '.stats.secondary .value { font-size: 1.1rem; color: var(--muted); }',
    '.funnel { display: grid; grid-template-columns: 1fr .8fr 1fr; gap: 1rem; margin-bottom: 2rem; }',
    '.funnel.single { grid-template-columns: 1fr; }',
    '.fpanel { background: var(--card); border: 1px solid var(--border); border-radius: 8px; padding: 1rem; }',
    '.fpanel h3 { font-size: .85rem; text-transform: uppercase; letter-spacing: .03em; color: var(--muted); margin-bottom: .5rem; }',
    '.fstage { margin-bottom: .6rem; }',
    '.flabel { font-size: .85rem; margin-bottom: .15rem; }',
    '.fnote { font-size: .78rem; color: var(--muted); margin-top: .15rem; }',
    '@media (max-width: 800px) { .funnel { grid-template-columns: 1fr; } table { display: block; overflow-x: auto; } }',
    'th.grp { text-align: center; border-left: 1px solid var(--border); }',
    'colgroup.grp-start, td.grp-start, th.grp-start { border-left: 1px solid var(--border); }',
    '.chip { display: inline-block; font-size: .7rem; font-weight: 600; padding: .05em .4em; border-radius: 4px; background: #fef3c7; color: #92400e; margin-top: .2rem; }',
    '.delta-gap { font-family: "JetBrains Mono", monospace; font-weight: 600; padding: .1em .4em; border-radius: 4px; font-size: .85rem; background: #ede9fe; color: #5b21b6; }',
    'td .delta { display: inline-block; margin-top: .2rem; }',
    'h3.sub { font-size: 1rem; margin: 1.25rem 0 .5rem; }',
    '.leaders td.name { font-family: "JetBrains Mono", monospace; }',
    '.leaders td.pkgs a { color: var(--accent); text-decoration: none; margin-right: .4rem; }',
    '.file-head { font-family: "JetBrains Mono", monospace; font-size: .9rem; font-weight: 600; color: var(--muted); margin: 1.25rem 0 .5rem; }',
    '.unbound-line { font-family: "JetBrains Mono", monospace; font-size: .8rem; color: var(--muted); }',
    '.filters fieldset { border: none; display: flex; gap: .6rem; align-items: center; }',
    '.filters legend { float: left; font-size: .8rem; color: var(--muted); margin-right: .3rem; }',
    '.filter-chip { font-size: .85rem; background: #e0e7ff; color: #3730a3; padding: .1em .5em; border-radius: 4px; }',
    '.filter-chip button { border: none; background: none; cursor: pointer; color: inherit; font: inherit; }',
    'body:not(.mode-gradual) .st-gradual, body.mode-gradual .st-strict { display: none; }',
    '</style>'
  )
}

# --- TypR presentation helpers ---

tot <- function(x) sum(x, na.rm = TRUE)

stat_card <- function(value, label, sub = "") {
  sprintf('<div class="stat-card"><div class="value">%s</div><div class="label">%s</div>%s</div>',
          value, label, if (nzchar(sub)) sprintf('<div class="ep-breakdown">%s</div>', sub) else "")
}

pct_str <- function(num, den) if (is.na(den) || den == 0) "&mdash;" else sprintf("%.1f%%", 100 * num / den)

# Baseline change of a count, or "" without a baseline that has the column.
card_delta <- function(now, before, good_dir = 1) {
  if (is.null(before) || is.na(before)) return("")
  paste0(" ", delta_html(as.integer(now - before), good_dir))
}

# One stage of the native -> R funnel: a bar scaled to the panel's own
# denominator. Stages may exceed it (a fallback signature can be bound without
# being typed), so the bar is clamped while the label keeps the real count.
funnel_stage <- function(label, n, of, note = "") {
  pct <- if (is.na(of) || of == 0) 0 else 100 * n / of
  w <- min(100, pct)
  sprintf('<div class="fstage"><div class="flabel">%s</div><div class="bar-bg"><div class="bar-fill" style="width:%.1f%%;background:hsl(%d,70%%,45%%)"></div><span class="bar-label">%d / %d (%.1f%%)</span></div>%s</div>',
          label, w, round(w * 1.2), as.integer(n), as.integer(of), pct,
          if (nzchar(note)) sprintf('<div class="fnote">%s</div>', note) else "")
}

funnel_panel <- function(title, stages) {
  sprintf('<div class="fpanel"><h3>%s</h3>%s</div>', title, paste(stages, collapse = ""))
}

# Why a package with C code has no entry point. A call site in an R file that
# did not parse is invisible to TypR, so say so rather than claim there is
# none.
no_ep_sentence <- function(unparsed) {
  calls <- '<code>.Call</code>/<code>.C</code>/<code>.Fortran</code>/<code>.External</code>'
  if (isTRUE(unparsed > 0))
    sprintf('No %s site found in the R code that parsed (%d R file%s did not), so no entry point.',
            calls, as.integer(unparsed), if (unparsed == 1) "" else "s")
  else sprintf('No %s site in the R code, so no entry point.', calls)
}

# Two panels sharing a hinge rather than one shrinking bar: C functions and R
# definitions are not comparable quantities. `f` holds the counts, for one
# package or summed over all of them.
funnel_html <- function(f) {
  r_panel <- funnel_panel("R definitions", c(
    funnel_stage("named top-level definitions", f$r_defs, f$r_defs),
    funnel_stage("not blocked by an unbound native symbol", f$r_defs - f$unbound_native, f$r_defs,
      if (f$unbound_native > 0) sprintf(
        '%d definitions report <code>unbound variable</code> on a native symbol TypR could not bind &mdash; the native panel&rsquo;s loss, arriving here.',
        as.integer(f$unbound_native)) else ""),
    funnel_stage("typed, gradual", f$rg_typed, f$r_defs),
    funnel_stage("typed, strict", f$r_typed, f$r_defs,
      if (f$prelude > 0) sprintf('%d of the strict failures are a name missing from the prelude.',
                                 as.integer(f$prelude)) else "")))
  if (!isTRUE(f$has_c))
    return(sprintf('<div class="funnel single">%s</div>', r_panel))
  # With no entry point to start from, TypR types every C function instead.
  native <- if (f$fn == 0)
    sprintf('<div class="fpanel"><h3>Native functions</h3><p class="na">No C function analysed.</p></div>')
  else funnel_panel("Native functions", c(
    funnel_stage(if (f$ep == 0) "C functions (no entry point, so all of them)"
                 else "C functions reachable from an entry point", f$fn, f$fn),
    funnel_stage("typed", f$typed, f$fn,
      if (isTRUE(f$fallback > 0)) sprintf('%d more bound at their declared C signature after inference failed.',
                                          as.integer(f$fallback)) else "")))
  hinge <- if (f$ep == 0)
    sprintf('<div class="fpanel"><h3>Native &rarr; R</h3><p class="na">%s</p></div>', no_ep_sentence(f$unparsed))
  else funnel_panel("Native &rarr; R", c(
    funnel_stage("entry points", f$ep, f$ep),
    funnel_stage("defined in <code>src/</code>", f$ep_reached, f$ep),
    funnel_stage("typed", f$ep_typed, f$ep),
    funnel_stage("bound in R", f$ep_linked, f$ep)))
  sprintf('<div class="funnel">%s%s%s</div>', native, hinge, r_panel)
}

funnel_counts <- function(idx) {
  list(
    has_c = any(nat$n_c_files[idx] > 0, na.rm = TRUE) || any(nat$n[idx] > 0, na.rm = TRUE),
    fn = tot(nat$n[idx]), typed = tot(nat$typed[idx]), fallback = tot(nat$from_c_sig[idx]),
    ep = tot(nat$ep[idx]), ep_reached = tot(nat$ep_reached[idx]),
    ep_typed = tot(nat$ep_typed[idx]), ep_linked = tot(nat$ep_linked[idx]),
    r_defs = tot(rs$n[idx]), unbound_native = tot(rs$unbound_native[idx]),
    rg_typed = tot(rs$g_typed[idx]), r_typed = tot(rs$typed[idx]),
    prelude = tot(rs$unbound_prelude[idx]), unparsed = tot(rs$files_unparsed[idx]))
}

# Run status of a package across its two modes.
run_status_html <- function(i) {
  bad <- c(strict = getcol(summary_df, "crashed", 0L)[i] %in% 1,
           gradual = getcol(summary_df, "rg_crashed", 0L)[i] %in% 1)
  trunc <- c(strict = getcol(summary_df, "truncated", 0L)[i] %in% 1,
             gradual = getcol(summary_df, "rg_truncated", 0L)[i] %in% 1)
  if (any(bad)) return(sprintf('<span class="badge badge-crash">crashed (%s)</span>',
                               paste(names(bad)[bad], collapse = ", ")))
  if (any(trunc)) return(sprintf('<span class="badge badge-timeout" title="no summary record: the report is incomplete">truncated (%s)</span>',
                                 paste(names(trunc)[trunc], collapse = ", ")))
  '<span class="badge badge-ok">OK</span>'
}

# The R definitions of the strict run, the side every R-specific section
# below reads.
r_strict <- functions_df[functions_df$side == "r" & functions_df$mode == "strict", , drop = FALSE]

# --- Build INDEX page ---
html <- character()
h <- function(...) html <<- c(html, paste0(...))

h('<!DOCTYPE html>')
h('<html lang="en"><head><meta charset="utf-8">')
h('<meta name="viewport" content="width=device-width, initial-scale=1">')
h('<title>R we type yet?</title>')
h(common_style())
h('</head><body>')

h('<h1><span class="r">R</span> we type yet?</h1>')
h(sprintf('<p class="subtitle">%s for %d CRAN packages &mdash; generated %s%s</p>',
  if (HAVE_R) 'TypR results (Rsem on the R code, NativeSem on the C code)' else 'Type-checking results',
  n_packages, format(Sys.time(), "%Y-%m-%d %H:%M"),
  checker_version_html()))
h(checker_status_html())

if (HAVE_R) {
  h('<details class="howto"><summary>How to read this</summary>',
    '<p>TypR checks a whole package: NativeSem infers types for the C functions the R code calls, those types are bound under their R names, then Rsem checks the R code. Every package is checked twice. <strong>Strict</strong> reports every name it cannot resolve; <strong>gradual</strong> gives such names the dynamic type and carries on.</p>',
    '<p>Rsem is early. Most strict failures are <code>unbound variable</code> on a function the prelude (the R signatures TypR loads first) does not cover yet &mdash; mostly base R &mdash; not a limit of the type system. That is why the headline numbers are the strict/gradual pair and <strong>prelude debt</strong> (definitions blocked only by a missing prelude name), and why <strong>real typing failures</strong> counts only what still fails in gradual mode. Do not read the strict percentage as &ldquo;TypR types N% of R&rdquo;.</p>',
    '<p>The two sides are not independent: a C function that fails to type cannot be bound, and every R definition calling it then reports an unbound variable. The funnel below follows that path.</p>',
    '</details>')

  b_rs <- if (!is.null(b_summary_df)) r_cols(b_summary_df) else NULL
  b_tot <- function(x) if (is.null(x) || all(is.na(x))) NA else tot(x)
  r_defs <- tot(rs$n)
  hard   <- tot(rs$g_untypeable) + tot(rs$g_timeout)
  h('<div class="stats">')
  h(stat_card(n_packages, "Packages analysed", sprintf("%d without crash", n_ok)))
  h(stat_card(pct_str(tot(rs$typed), r_defs), "R definitions typed (strict)",
    sprintf("%d / %d &middot; gradual %s%s", tot(rs$typed), r_defs, pct_str(tot(rs$g_typed), r_defs),
            card_delta(tot(rs$typed), b_tot(b_rs$typed)))))
  h(stat_card(tot(rs$unbound_prelude), "Prelude debt",
    paste0("definitions blocked by a name the prelude lacks",
           card_delta(tot(rs$unbound_prelude), b_tot(b_rs$unbound_prelude), -1))))
  h(stat_card(hard, "Real typing failures",
    paste0("untypeable or timed out even in gradual mode",
           card_delta(hard, if (is.null(b_rs)) NA else b_tot(b_rs$g_untypeable) + b_tot(b_rs$g_timeout), -1))))
  moved <- if (is.null(native_scope)) native_scope else
    native_scope[native_scope$added + native_scope$removed > 0L, , drop = FALSE]
  h(stat_card(pct_str(total_typed, total_functions), "Native functions typed",
    sprintf("%d / %d C functions reachable from an entry point%s%s", total_typed, total_functions,
            if (is.null(native_scope)) "" else
              card_delta(sum(native_scope$typed_now), sum(native_scope$typed_before)),
            if (NROW(moved) == 0L) "" else sprintf(
              '<br><span title="the change counts only C functions analysed in both runs">scope changed in %d package%s (+%d / &minus;%d functions)</span>',
              nrow(moved), if (nrow(moved) == 1L) "" else "s", sum(moved$added), sum(moved$removed)))))
  h(stat_card(pct_str(tot(nat$ep_linked), total_ep), "Native &rarr; R link rate",
    sprintf("%d / %d entry points bound in R", tot(nat$ep_linked), total_ep)))
  h('</div>')
  h('<div class="stats secondary">')
  h(stat_card(total_ep, "entry points", sprintf('<code>.Call</code>&nbsp;%d &middot; <code>.C</code>&nbsp;%d &middot; <code>.Fortran</code>&nbsp;%d &middot; <code>.External</code>&nbsp;%d',
    total_ep_call, total_ep_c, total_ep_fortran, total_ep_external)))
  h(stat_card(tot(nat$from_c_sig), "native bound at C signature"))
  h(stat_card(total_timeout, "native timeouts"))
  h(stat_card(tot(rs$timeout), "R timeouts (strict)"))
  h(stat_card(tot(rs$skipped) + tot(rs$unknown), "R skipped / unrecognised"))
  h(stat_card(tot(rs$stmts), "anonymous R statements", "not in any denominator"))
  h(stat_card(tot(rs$files_unparsed), "R files not parsed"))
  h('</div>')

  h('<h2>From native to R</h2>')
  h(funnel_html(funnel_counts(seq_len(n_packages))))
}

# Stat cards
if (!HAVE_R) {
h('<div class="stats">')
h(sprintf('<div class="stat-card"><div class="value">%d</div><div class="label">Packages analysed</div></div>', n_packages))
h(sprintf('<div class="stat-card"><div class="value">%d</div><div class="label">Entrypoints found</div><div class="ep-breakdown"><code>.Call</code>&nbsp;%d &middot; <code>.C</code>&nbsp;%d &middot; <code>.Fortran</code>&nbsp;%d &middot; <code>.External</code>&nbsp;%d</div></div>',
  total_ep, total_ep_call, total_ep_c, total_ep_fortran, total_ep_external))
h(sprintf('<div class="stat-card"><div class="value">%d</div><div class="label">C functions analysed</div></div>', total_functions))
h(sprintf('<div class="stat-card"><div class="value">%.1f%%</div><div class="label">Analysed functions typed</div></div>', pct_overall))
h(sprintf('<div class="stat-card"><div class="value">%.1f%%</div><div class="label">Entrypoints typed</div><div class="ep-breakdown">%d / %d</div></div>',
  pct_ep_typed, total_ep_typed, total_ep))
h(sprintf('<div class="stat-card"><div class="value">%d</div><div class="label">Functions timed out</div></div>', total_timeout))
h(sprintf('<div class="stat-card"><div class="value">%d / %d</div><div class="label">Packages without crash</div></div>', n_ok, n_packages))
h('</div>')
}

# Per-package table
h('<h2>Per-package results</h2>')
max_elapsed <- suppressWarnings(max(summary_df$elapsed_sec, na.rm = TRUE))
time_digits <- if (!is.finite(max_elapsed)) 1L else if (max_elapsed < 10) 2L else if (max_elapsed < 100) 1L else 0L
time_fmt <- paste0("%.", time_digits, "f s")
fmt_time <- function(t) if (is.na(t)) '<span class="na">&mdash;</span>' else sprintf(time_fmt, t)

# Build a baseline lookup keyed by package name for quick delta computation.
b_typed_by_pkg <- if (!is.null(b_summary_df)) {
  setNames(b_nat$typed, b_summary_df$package)
} else NULL

b_rtyped_by_pkg <- if (!is.null(b_summary_df) && "r_n_typed" %in% names(b_summary_df)) {
  setNames(b_summary_df$r_n_typed, b_summary_df$package)
} else NULL
# Change of a per-package count since the baseline, rendered inside the cell
# it qualifies; "" without a baseline, "new" for a package it lacks.
cell_delta <- function(now, by_pkg, pkg) {
  if (is.null(baseline_dir) || is.null(by_pkg)) return("")
  if (!(pkg %in% names(by_pkg))) return(' <span class="delta delta-good">new</span>')
  paste0(" ", delta_html(as.integer(now - by_pkg[[pkg]]), +1))
}
chip <- function(n, what) if (isTRUE(n > 0)) sprintf(' <span class="chip">%d %s</span>', as.integer(n), what) else ""

# Typed change over the C functions both runs analysed (see native_scope).
b_common_typed_by_pkg <- if (!is.null(native_scope))
  setNames(native_scope$typed_before, native_scope$package) else NULL
native_typed_delta <- function(pkg)
  cell_delta(if (is.null(native_scope)) NA else native_scope[pkg, "typed_now"], b_common_typed_by_pkg, pkg)

# "36 -> 22" beside the function count of a package whose analysed C functions
# differ from the baseline's, with the likely reason as a tooltip.
scope_badge <- function(i) {
  pkg <- summary_df$package[i]
  if (!scope_moved(pkg)) return("")
  j <- match(pkg, b_summary_df$package)
  why <- if (isTRUE(b_nat$ep[j] == 0 && nat$ep[i] > 0))
    "entry points found: only the C functions they reach are analysed now"
  else if (isTRUE(b_nat$ep[j] > 0 && nat$ep[i] == 0))
    "no entry point found any more: every C function is analysed now"
  else if (isTRUE(b_nat$ep[j] != nat$ep[i]))
    sprintf("%s entry points found (%d &rarr; %d), reaching a different set of C functions",
      if (nat$ep[i] > b_nat$ep[j]) "more" else "fewer", as.integer(b_nat$ep[j]), as.integer(nat$ep[i]))
  else "the set of analysed C functions changed"
  sprintf(' <span class="badge badge-change-neutral" title="%s (+%d / &minus;%d since the baseline run). The typed change counts only functions analysed in both runs.">%d &rarr; %d</span>',
    why, native_scope[pkg, "added"], native_scope[pkg, "removed"], as.integer(b_nat$n[j]), as.integer(nat$n[i]))
}

if (HAVE_R) {
  pkg_table_th <- c('<th>Package</th>', '<th class="grp-start">Functions</th>', '<th>Typed</th>',
    '<th class="grp-start">EPs</th>', '<th>Bound in R</th>',
    '<th class="grp-start">Defs</th>', '<th>Strict</th>', '<th>Gradual</th>',
    '<th title="typed in gradual mode but not in strict mode">Gap</th>',
    '<th title="strict failures on a name missing from the prelude">Prelude debt</th>',
    '<th class="grp-start">Time</th>', '<th>Status</th>')
  h('<table><colgroup><col></colgroup><colgroup span="2"></colgroup><colgroup span="2"></colgroup><colgroup span="5"></colgroup><colgroup span="2"></colgroup>',
    '<thead><tr><th></th><th colspan="2" class="grp">Native</th><th colspan="2" class="grp">Linkage</th>',
    '<th colspan="5" class="grp">R definitions</th><th colspan="2" class="grp">Run</th></tr>',
    '<tr>', paste(pkg_table_th, collapse = ""), '</tr></thead><tbody>')
  for (i in seq_len(nrow(summary_df))) {
    pkg <- summary_df$package[i]
    pkg_link <- sprintf('<a href="pkg/%s.html">%s</a>', esc(pkg), esc(pkg))
    no_c <- !isTRUE(nat$n_c_files[i] > 0) && !isTRUE(nat$n[i] > 0)
    native_cells <- if (no_c) '<td class="grp-start" colspan="4"><span class="na">no native code</span></td>' else
      sprintf('<td class="num grp-start">%d%s</td><td>%s%s%s</td><td class="num grp-start"><span title=".Call %d &middot; .C %d &middot; .Fortran %d &middot; .External %d">%d</span></td><td>%s</td>',
        nat$n[i], scope_badge(i),
        pct_bar(nat$typed[i], nat$n[i], if (nat$ep[i] == 0) "no entry point" else "no functions analysed"),
        chip(nat$timeout[i], "timed out"), native_typed_delta(pkg),
        nat$ep_call[i], nat$ep_c[i], nat$ep_fortran[i], nat$ep_external[i], nat$ep[i],
        pct_bar(nat$ep_linked[i], nat$ep[i], "no entry point"))
    gap <- rs$g_typed[i] - rs$typed[i]
    debt <- rs$unbound_prelude[i]
    h(sprintf('<tr><td class="pkg">%s</td>%s<td class="num grp-start">%s</td><td>%s%s%s</td><td>%s</td><td class="num">%s</td><td class="num">%s</td><td class="num grp-start">%s</td><td>%s</td></tr>',
      pkg_link, native_cells,
      if (is.na(rs$n[i])) "&mdash;" else as.character(rs$n[i]),
      pct_bar(rs$typed[i], rs$n[i], "no R definitions"), chip(rs$timeout[i], "timed out"),
      cell_delta(rs$typed[i], b_rtyped_by_pkg, pkg),
      pct_bar(rs$g_typed[i], rs$n[i], "no R definitions"),
      if (is.na(gap)) "&mdash;" else sprintf('<span class="delta-gap">+%d</span>', as.integer(gap)),
      if (is.na(debt) || debt == 0) as.character(if (is.na(debt)) "&mdash;" else 0L) else
        sprintf('<a href="pkg/%s.html#unbound">%d</a>', esc(pkg), as.integer(debt)),
      fmt_time(summary_df$elapsed_sec[i]),
      run_status_html(i)))
  }
  if (!is.null(b_summary_df)) {
    for (pkg in setdiff(b_summary_df$package, summary_df$package)) {
      h(sprintf('<tr><td class="pkg">%s</td><td colspan="%d"><span class="delta delta-bad">gone</span> (was in baseline, not in current run)</td></tr>',
        esc(pkg), length(pkg_table_th) - 1L))
    }
  }
  h('</tbody></table>')
} else {
pkg_table_th <- c('<th>Package</th>', '<th>Entrypoints</th>', '<th>.Call</th>', '<th>.C</th>',
  '<th>.Fortran</th>', '<th>.External</th>', '<th>EP typing progress</th>',
  '<th>Functions analysed</th>', if (!is.null(baseline_dir)) '<th>Change</th>',
  '<th>Timed out</th>', '<th>Typing progress</th>', '<th>Typing time</th>', '<th>Status</th>')
h('<table><thead><tr>', paste(pkg_table_th, collapse = ""), '</tr></thead><tbody>')
for (i in seq_len(nrow(summary_df))) {
  r <- summary_df[i, ]
  ep <- nat$ep[i]
  n_to <- nat$timeout[i]
  badge <- if (r$crashed) '<span class="badge badge-crash">crashed</span>' else '<span class="badge badge-ok">OK</span>'
  pkg_link <- sprintf('<a href="pkg/%s.html">%s</a>', esc(r$package), esc(r$package))

  delta_td <- ""
  if (!is.null(baseline_dir)) {
    if (!is.null(b_typed_by_pkg) && r$package %in% names(b_typed_by_pkg)) {
      d <- nat$typed[i] - as.integer(b_typed_by_pkg[r$package])
      delta_td <- sprintf('<td class="num">%s</td>', delta_html(d, +1))
    } else {
      delta_td <- '<td class="num"><span class="delta delta-good">new</span></td>'
    }
  }

  h(sprintf('<tr><td class="pkg">%s</td><td class="num">%d</td><td class="num">%d</td><td class="num">%d</td><td class="num">%d</td><td class="num">%d</td><td>%s</td><td class="num">%d</td>%s<td class="num">%d</td><td>%s</td><td class="num">%s</td><td>%s</td></tr>',
    pkg_link, ep, nat$ep_call[i], nat$ep_c[i], nat$ep_fortran[i], nat$ep_external[i],
    pct_bar(nat$ep_typed[i], ep), nat$n[i], delta_td, n_to,
    pct_bar(nat$typed[i], nat$n[i]),
    fmt_time(r$elapsed_sec), badge))
}

# Rows for packages that disappeared (present in baseline only).
if (!is.null(b_summary_df)) {
  gone <- setdiff(b_summary_df$package, summary_df$package)
  for (pkg in gone) {
    h(sprintf('<tr><td class="pkg">%s</td><td colspan="%d"><span class="delta delta-bad">gone</span> (was in baseline, not in current run)</td></tr>',
      esc(pkg), length(pkg_table_th) - 1L))
  }
}
h('</tbody></table>')
}

# Missing prelude names: the most actionable R-side view. Only names whose
# failure is the prelude's: a package's own names (cycles, ordering) and
# native symbols TypR could not bind are fixed elsewhere.
if (HAVE_R && nrow(r_strict) > 0) {
  blocked <- r_strict[!r_strict$anonymous & r_strict$unbound_kind %in% "prelude" &
                      !is.na(r_strict$error_name), , drop = FALSE]
  if (nrow(blocked) > 0) {
    pkgs_by_name <- split(blocked$package, blocked$error_name)
    n_blocked <- vapply(pkgs_by_name, length, integer(1))
    names_ord <- names(pkgs_by_name)[order(-n_blocked, names(pkgs_by_name))]
    leader_row <- function(nm) {
      per_pkg <- table(pkgs_by_name[[nm]])
      per_pkg <- per_pkg[order(-per_pkg, names(per_pkg))]
      where <- paste(sprintf('<a href="pkg/%s.html#unbound=%s">%s&nbsp;(%d)</a>',
        esc(names(per_pkg)), esc(utils::URLencode(nm, reserved = TRUE)), esc(names(per_pkg)),
        as.integer(per_pkg)), collapse = " ")
      sprintf('<tr><td class="name">%s</td><td class="num">%d</td><td class="num">%d</td><td class="pkgs">%s</td></tr>',
        esc(nm), n_blocked[[nm]], length(per_pkg), where)
    }
    h('<h2 id="prelude-gaps">Missing prelude names <small>by strict-mode definitions blocked</small></h2>')
    h('<p class="phase-legend">Names used by some definition that are neither defined in a parsed R file of the package, nor a native symbol, nor declared in the prelude. Mostly base R, where a signature in <code>prelude/base.R</code> unblocks the row &mdash; unless a definition then fails on its next missing name. Two other kinds land here too, and no prelude work fixes them: functions imported from other packages (<code>rlang::caller_env</code>), and the package&rsquo;s own functions defined in an R file that did not parse. Definitions blocked by the package&rsquo;s own (parsed) names or by native symbols are not listed.</p>')
    h('<table class="leaders"><thead><tr><th>Name</th><th style="text-align:right">Definitions blocked</th><th style="text-align:right">Packages</th><th>Where</th></tr></thead><tbody>')
    shown <- head(names_ord, 25L)
    for (nm in shown) h(leader_row(nm))
    h('</tbody></table>')
    rest <- setdiff(names_ord, shown)
    if (length(rest) > 0L) {
      h(sprintf('<details><summary>%d more names</summary><table class="leaders"><tbody>', length(rest)))
      for (nm in rest) h(leader_row(nm))
      h('</tbody></table></details>')
    }
  }
}

# Native -> R linkage: why an entry point did not become an R binding.
if (HAVE_R && !is.null(links_df)) {
  h('<h2 id="linkage">Native &rarr; R linkage</h2>')
  not_bound <- links_df[!as.logical(links_df$bound), , drop = FALSE]
  absent <- if (!is.null(entry_points_df))
    entry_points_df[is.na(entry_points_df$native_status), , drop = FALSE] else NULL
  if (nrow(not_bound) == 0L) {
    h(sprintf('<p class="na">All %d entry points are bound in R.</p>', nrow(links_df)))
  } else {
    REASONS <- c(
      "no native type"         = "no type: not defined in <code>src/</code>, or NativeSem failed on it",
      "empty native type"      = "typed as <code>empty</code>: no call can succeed",
      "not a function type"    = "the native type is not a function type",
      "empty after conversion" = "no R calling convention fits its C type")
    reason_label <- function(r) if (r %in% names(REASONS)) REASONS[[r]] else esc(r)
    counts <- table(not_bound$reject_reason)
    h(sprintf('<p class="phase-legend">%d of %d entry points are not bound in R: %s.</p>',
      nrow(not_bound), nrow(links_df),
      paste(sprintf('%d &mdash; %s', as.integer(counts), vapply(names(counts), reason_label, character(1))),
            collapse = "; ")))
    h('<table><thead><tr><th>Package</th><th>R name</th><th>C symbol</th><th>Why not bound</th><th>Native type</th></tr></thead><tbody>')
    nb <- not_bound[order(not_bound$package, not_bound$reject_reason, not_bound$symbol), , drop = FALSE]
    for (i in seq_len(nrow(nb))) {
      h(sprintf('<tr><td class="pkg"><a href="pkg/%s.html">%s</a></td><td class="error-row"><code>%s</code></td><td><code>%s</code></td><td>%s</td><td class="type-sig">%s</td></tr>',
        esc(nb$package[i]), esc(nb$package[i]), esc(nb$r_name[i]), esc(nb$symbol[i]),
        reason_label(nb$reject_reason[i]),
        if (is.na(nb$native_type[i])) '<span class="na">&mdash;</span>' else type_sig_html(nb$native_type[i])))
    }
    h('</tbody></table>')
  }
  if (!is.null(absent) && nrow(absent) > 0L) {
    per_pkg <- split(absent$symbol, absent$package)
    h(sprintf('<p class="phase-legend">%d entry points reached from R have no definition in <code>src/</code> (registered under another name, generated, or defined in a dependency): %s.</p>',
      nrow(absent), paste(sprintf('<a href="pkg/%s.html">%s</a> (%d)', esc(names(per_pkg)), esc(names(per_pkg)),
                                  lengths(per_pkg)), collapse = ", ")))
  }
  blocked_native <- r_strict[!r_strict$anonymous & r_strict$unbound_kind %in% "native", , drop = FALSE]
  n_pkgs <- length(unique(blocked_native$package))
  h(sprintf('<p class="phase-legend"><strong>%d R definition%s across %d package%s report <code>unbound variable</code> on a native symbol.</strong> That is where a native failure lands on the R side.</p>',
    nrow(blocked_native), if (nrow(blocked_native) == 1) "" else "s", n_pkgs, if (n_pkgs == 1) "" else "s"))
}

# Error categories, one table per side: the native and R vocabularies are
# disjoint, and a merged table would sort every native category under R's
# `unbound variable`.
render_error_table <- function(now, base) {
  ord <- order(-now)
  base_col <- if (!is.null(baseline_dir)) '<th style="text-align:right">Before</th><th style="text-align:right">&Delta;</th>' else ''
  h('<table><thead><tr><th>Error</th>', base_col, '<th style="text-align:right">Count</th></tr></thead><tbody>')
  for (cat in names(now)[ord]) {
    cnt <- now[[cat]]
    extra <- ""
    if (!is.null(baseline_dir)) {
      bcnt <- as.integer(base[cat]); if (is.na(bcnt)) bcnt <- 0L
      extra <- sprintf('<td class="num">%d</td><td class="num">%s</td>', bcnt, delta_html(cnt - bcnt, -1))
    }
    h(sprintf('<tr class="error-row"><td>%s</td>%s<td class="num">%d</td></tr>', esc(cat), extra, cnt))
  }
  # Categories present in baseline but absent now (good!)
  if (!is.null(baseline_dir)) {
    for (cat in setdiff(names(base), names(now))) {
      bcnt <- as.integer(base[cat])
      h(sprintf('<tr class="error-row"><td>%s</td><td class="num">%d</td><td class="num">%s</td><td class="num">0</td></tr>',
        esc(cat), bcnt, delta_html(-bcnt, -1)))
    }
  }
  h('</tbody></table>')
}
if (HAVE_R) {
  err_r_now  <- error_category_counts(functions_df, "r", "strict")
  err_r_base <- if (!is.null(b_functions_df)) error_category_counts(b_functions_df, "r", "strict") else integer(0)
  if (length(err_now) + length(err_r_now) > 0L) {
    h('<h2>Error categories</h2>')
    if (length(err_now) > 0L) {
      h('<h3 class="sub">Native functions</h3>')
      render_error_table(err_now, err_base)
    }
    if (length(err_r_now) > 0L) {
      h('<h3 class="sub">R definitions <small>strict mode</small></h3>')
      render_error_table(err_r_now, err_r_base)
    }
  }
} else if (HAVE_NATIVE && nrow(error_cats) > 0) {
  h('<h2>Error and timeout categories</h2>')
  render_error_table(err_now, err_base)
}

# Definitions typed as `empty` (bottom). Kept separate from the error tables
# because these DID type: an `empty` result is a quality red flag, not a
# failure. Placed next to the error section because it belongs to the same
# family of red flags. One table per side, each against its own typed count.
empty_counts_by_pkg <- function(fdf, side = "native", mode = "strict") {
  fdf <- fdf[fdf$side == side & fdf$mode == mode & !fdf$anonymous, , drop = FALSE]
  if (nrow(fdf) == 0L) return(setNames(integer(0), character(0)))
  ty <- fdf[is_empty_ret(fdf), , drop = FALSE]
  if (nrow(ty) == 0L) return(setNames(integer(0), character(0)))
  tbl <- table(ty$package)
  setNames(as.integer(tbl), names(tbl))
}

render_empty_section <- function(side, typed_counts, heading, legend, what) {
  emp_now  <- empty_counts_by_pkg(functions_df, side)
  emp_base <- if (!is.null(b_functions_df)) empty_counts_by_pkg(b_functions_df, side) else integer(0)
  total_empty      <- sum(emp_now)
  total_empty_base <- sum(emp_base)
  if (total_empty == 0L && length(emp_base) == 0L) return(invisible())
  total_typed_side <- tot(typed_counts)
  h(heading)
  h(legend)

  cur_for  <- function(p) { v <- emp_now[p];  if (is.na(v)) 0L else as.integer(v) }
  base_for <- function(p) { v <- emp_base[p]; if (is.na(v)) 0L else as.integer(v) }
  pkgs <- union(names(emp_now), names(emp_base))
  pkgs <- pkgs[order(-vapply(pkgs, cur_for, integer(1)), pkgs)]
  typed_by_pkg <- setNames(typed_counts, summary_df$package)

  base_col <- if (!is.null(baseline_dir)) '<th style="text-align:right">Before</th><th style="text-align:right">&Delta;</th>' else ''
  h(sprintf('<table><thead><tr><th>Package</th><th style="text-align:right">Empty returns</th><th style="text-align:right">%% of %s</th>', what),
    base_col, '</tr></thead>')
  # The total line comes first, tinted, so the global figure and its change
  # are visible at a glance.
  total_pct <- if (total_typed_side > 0L) sprintf("%.1f%%", 100 * total_empty / total_typed_side) else "&mdash;"
  total_extra <- if (!is.null(baseline_dir))
    sprintf('<td class="num">%d</td><td class="num">%s</td>',
            total_empty_base, delta_html(total_empty - total_empty_base, -1))
  else ""
  h('<tbody>')
  h(sprintf('<tr class="total-row"><td>All packages</td><td class="num">%d</td><td class="num">%s</td>%s</tr>',
    total_empty, total_pct, total_extra))
  for (p in pkgs) {
    cnt <- cur_for(p)
    nt <- as.integer(typed_by_pkg[p]); if (is.na(nt)) nt <- 0L
    pctp <- if (nt > 0L) sprintf("%.1f%%", 100 * cnt / nt) else "&mdash;"
    extra <- ""
    if (!is.null(baseline_dir)) {
      extra <- sprintf('<td class="num">%d</td><td class="num">%s</td>',
                       base_for(p), delta_html(cnt - base_for(p), -1))
    }
    pkg_link <- sprintf('<a href="pkg/%s.html#empty">%s</a>', esc(p), esc(p))
    h(sprintf('<tr><td class="pkg">%s</td><td class="num">%d</td><td class="num">%s</td>%s</tr>',
      pkg_link, cnt, pctp, extra))
  }
  h('</tbody></table>')
}

if (HAVE_NATIVE) {
  render_empty_section("native", nat$typed,
    if (HAVE_R) '<h2>Typed as <code>empty</code> <small>typed, but the inferred result is <code>empty</code> (bottom)</small></h2><h3 class="sub">Native functions</h3>'
    else '<h2>Functions typed as <code>empty</code> <small>typed, but the inferred result is <code>empty</code> (bottom)</small></h2>',
    paste0('<p class="phase-legend">An <code>empty</code> return means inference found no usable value &mdash; usually a red flag,',
      ' though it can be legitimate for bail-out helpers (e.g. <code>bail_if</code>-style functions that never return).',
      ' Drill into a package and use the <em>empty return only</em> filter to see which functions.</p>'),
    "pkg typed")
}
if (HAVE_R) {
  render_empty_section("r", rs$typed,
    if (HAVE_NATIVE) '<h3 class="sub">R definitions <small>strict mode</small></h3>'
    else '<h2>Typed as <code>empty</code> <small>R definitions, strict mode</small></h2>',
    paste0('<p class="phase-legend">An R definition typed <code>empty</code> is a function inference concluded can never return &mdash;',
      ' more alarming than on the C side, where bail-out helpers that always raise an R error are common.</p>'),
    "pkg R typed")
}

# Per-function (per-definition) inference timing.
timing_rows_of <- function(fdf) {
  fdf <- fdf[!is.na(fdf$timing_sec), , drop = FALSE]
  if (nrow(fdf) == 0L) return(NULL)
  do.call(rbind, lapply(split(fdf, fdf$package), function(d) {
    t <- d$timing_sec
    data.frame(package = d$package[1], n = nrow(d), n_timeout = sum(d$status == "timeout"),
               total = sum(t), mean = mean(t), median = median(t),
               sd = if (length(t) >= 2L) sd(t) else NA_real_, min = min(t), max = max(t),
               stringsAsFactors = FALSE)
  }))
}
if (HAVE_R) {
  native_t <- timing_rows_of(functions_df[functions_df$side == "native", , drop = FALSE])
  r_t      <- timing_rows_of(r_strict)
  if (!is.null(native_t) || !is.null(r_t)) {
    if (!is.null(native_t)) native_t$side <- "native"
    if (!is.null(r_t)) r_t$side <- "R (strict)"
    tt <- rbind(native_t, r_t)
    # Packages by their total time, a package's two rows together.
    pkg_total <- tapply(tt$total, tt$package, sum)
    tt <- tt[order(-pkg_total[tt$package], tt$package, tt$side), , drop = FALSE]
    h('<h2>Inference timing <small>per native function and per R definition</small></h2>')
    h('<table><thead><tr><th>Package</th><th>Side</th>',
      '<th style="text-align:right">Timed</th><th style="text-align:right">Timed out</th>',
      '<th style="text-align:right">Total</th><th style="text-align:right">Avg</th>',
      '<th style="text-align:right">Median</th><th style="text-align:right">SD</th>',
      '<th style="text-align:right">Min</th><th style="text-align:right">Max</th></tr></thead><tbody>')
    for (i in seq_len(nrow(tt))) {
      r <- tt[i, ]
      h(sprintf('<tr><td class="pkg"><a href="pkg/%s.html">%s</a></td><td>%s</td><td class="num">%d</td><td class="num">%d</td><td class="num">%s</td><td class="num">%s</td><td class="num">%s</td><td class="num">%s</td><td class="num">%s</td><td class="num">%s</td></tr>',
        esc(r$package), esc(r$package), r$side, r$n, r$n_timeout,
        fmt_func_timing(r$total), fmt_func_timing(r$mean), fmt_func_timing(r$median),
        fmt_func_timing(r$sd), fmt_func_timing(r$min), fmt_func_timing(r$max)))
    }
    h('</tbody></table>')
  }
} else {
has_timing_col <- "mean_typing_sec" %in% names(summary_df)
if (HAVE_NATIVE && has_timing_col && any(!is.na(summary_df$mean_typing_sec))) {
  timing_rows <- summary_df[!is.na(summary_df$mean_typing_sec), , drop = FALSE]
  timing_rows <- timing_rows[order(-timing_rows$total_typing_sec), , drop = FALSE]

  h('<h2>Per-function inference timing <small>sum of <code>--log-times</code> per function</small></h2>')
  h('<table><thead><tr>',
    '<th>Package</th>',
    '<th style="text-align:right">Functions timed</th>',
    '<th style="text-align:right">Timed out</th>',
    '<th style="text-align:right">Total</th>',
    '<th style="text-align:right">Avg</th>',
    '<th style="text-align:right">Median</th>',
    '<th style="text-align:right">SD</th>',
    '<th style="text-align:right">Min</th>',
    '<th style="text-align:right">Max</th>',
    '</tr></thead><tbody>')
  for (i in seq_len(nrow(timing_rows))) {
    r <- timing_rows[i, ]
    pkg_link <- sprintf('<a href="pkg/%s.html">%s</a>', esc(r$package), esc(r$package))
    n_to <- if ("n_timeout" %in% names(r)) r$n_timeout else 0L
    h(sprintf(
      '<tr><td class="pkg">%s</td><td class="num">%d / %d</td><td class="num">%d</td><td class="num">%s</td><td class="num">%s</td><td class="num">%s</td><td class="num">%s</td><td class="num">%s</td><td class="num">%s</td></tr>',
      pkg_link,
      r$n_timed_meas, r$n_functions, n_to,
      fmt_func_timing(r$total_typing_sec),
      fmt_func_timing(r$mean_typing_sec),
      fmt_func_timing(r$median_typing_sec),
      fmt_func_timing(r$sd_typing_sec),
      fmt_func_timing(r$min_typing_sec),
      fmt_func_timing(r$max_typing_sec)))
  }
  h('</tbody></table>')
}
}

# Native pipeline phase breakdown. Only the phases some package has data for
# get a column: NativeSem's own driver reported five (under --log-times),
# TypR reports load_ty, parsing and the function pass. The R side has no
# phase instrumentation. `overhead = fundef_pass - total_typing_sec` is the
# time inside the function pass not attributed to any function body.
# `recording_save` only fires when --mlsem-recording is set.
PHASES <- c(phase_load_ty_sec = "load_ty", phase_parsing_sec = "parsing",
            phase_call_graph_sec = "call_graph", phase_non_fundef_sec = "non_fundef",
            phase_fundef_pass_sec = "fundef_pass", phase_recording_save_sec = "recording_save")
phase_have <- vapply(names(PHASES), function(col)
  col %in% names(summary_df) && any(!is.na(summary_df[[col]])), logical(1))
if (HAVE_NATIVE && phase_have[["phase_fundef_pass_sec"]]) {
  shown <- names(PHASES)[phase_have]
  phase_rows <- summary_df[!is.na(summary_df$phase_fundef_pass_sec), , drop = FALSE]
  phase_rows <- phase_rows[order(-phase_rows$elapsed_sec), , drop = FALSE]

  if (HAVE_R) {
    h('<h2>Native pipeline phases <small>strict run</small></h2>')
    h('<p class="phase-legend">TypR reports the native side&rsquo;s phases only; the R side has no phase instrumentation. ',
      '<strong>Wall</strong> is the whole strict run, R side included.',
      ' <strong>&Sigma; infer_ast</strong> is the sum of every native function&rsquo;s inference time.',
      ' <strong>overhead</strong> = <code>fundef_pass &minus; &Sigma; infer_ast</code>: time spent inside the function pass that <em>isn\'t</em> attributed to any function body.',
      '</p>')
  } else {
    h('<h2>Pipeline phase breakdown <small>from <code>Phase:</code> lines under <code>--log-times</code></small></h2>')
    h('<p class="phase-legend">',
      '<strong>&Sigma; infer_ast</strong> is the sum of every function\'s <code>infer_ast</code> body-inference timing (the same total as in the table above).',
      ' <strong>overhead</strong> = <code>fundef_pass &minus; &Sigma; infer_ast</code>: time spent inside the Fundef loop that <em>isn\'t</em> attributed to any function body &mdash; <code>PAst.transform</code> per function, dispatch (StrMap lookups, idenv union), and GC between calls.',
      '</p>')
  }
  h('<table><thead><tr>',
    '<th>Package</th>',
    '<th style="text-align:right">Wall</th>',
    paste(sprintf('<th style="text-align:right">%s</th>', PHASES[setdiff(shown, "phase_recording_save_sec")]), collapse = ""),
    '<th style="text-align:right" title="sum of every function\'s infer_ast timing">&Sigma; infer_ast</th>',
    if ("phase_recording_save_sec" %in% shown) '<th style="text-align:right">recording_save</th>' else '',
    '<th style="text-align:right" title="fundef_pass minus the sum of per-function infer_ast timings">overhead<br><small>fundef_pass &minus; &Sigma; infer_ast</small></th>',
    '</tr></thead><tbody>')
  for (i in seq_len(nrow(phase_rows))) {
    r <- phase_rows[i, ]
    overhead <- if (!is.na(r$total_typing_sec))
      r$phase_fundef_pass_sec - r$total_typing_sec else NA_real_
    pkg_link <- sprintf('<a href="pkg/%s.html">%s</a>', esc(r$package), esc(r$package))
    phase_cells <- paste(vapply(setdiff(shown, "phase_recording_save_sec"), function(col)
      sprintf('<td class="num">%s</td>', fmt_func_timing(r[[col]])), character(1)), collapse = "")
    rec_cell <- if ("phase_recording_save_sec" %in% shown)
      sprintf('<td class="num">%s</td>', fmt_func_timing(r$phase_recording_save_sec))
    else ""
    h(sprintf(
      '<tr><td class="pkg">%s</td><td class="num">%s</td>%s<td class="num">%s</td>%s<td class="num">%s</td></tr>',
      pkg_link,
      fmt_func_timing(r$elapsed_sec),
      phase_cells,
      fmt_func_timing(r$total_typing_sec),
      rec_cell,
      fmt_func_timing(overhead)))
  }
  h('</tbody></table>')
}

# Crashes (one row per run that did not exit 0; TypR runs carry their mode).
if (nrow(crashes_df) > 0) {
  has_mode <- "mode" %in% names(crashes_df)
  h('<h2>Crash details</h2>')
  h('<table><thead><tr><th>Package</th>', if (has_mode) '<th>Mode</th>' else '',
    '<th>Error</th></tr></thead><tbody>')
  for (i in seq_len(nrow(crashes_df))) {
    h(sprintf('<tr><td class="pkg">%s</td>%s<td class="crash-msg">%s</td></tr>',
      esc(crashes_df$package[i]),
      if (has_mode) sprintf('<td>%s</td>', esc(crashes_df$mode[i])) else '',
      esc(crashes_df$error_message[i])))
  }
  h('</tbody></table>')
}

h('<footer>Generated by <strong>r-typing</strong> pipeline')
if (!is.null(baseline_dir)) {
  b_ver_suffix <- if (!is.null(b_checker_meta) && !is.na(b_checker_meta$version)) {
    sprintf(' @ <code>%s</code>', esc(b_checker_meta$version))
  } else ""
  h(sprintf(' &middot; baseline: <code>%s</code>%s', esc(baseline_dir), b_ver_suffix))
}
h('</footer>')
h('</body></html>')

writeLines(html, output_file)
cat(sprintf("Wrote %s\n", output_file))

# --- Build PER-PACKAGE pages ---

# Baseline per-function status, keyed by function_keys(), for detecting status
# changes on per-package pages. NULL when no baseline.
b_status_by_key <- if (!is.null(b_functions_df)) {
  setNames(b_functions_df$status, b_functions_df$key)
} else NULL

status_badge <- function(status) {
  switch(status,
    "typed"      = '<span class="badge badge-typed">typed</span>',
    "untypeable" = '<span class="badge badge-untypeable">untypeable</span>',
    "timeout"    = '<span class="badge badge-timeout">timeout</span>',
    "fallback"   = '<span class="badge badge-fallback" title="inference failed; bound at its declared C signature">C signature</span>',
    "skipped"    = '<span class="badge badge-timeout" title="TypR caught an exception while checking it">skipped</span>',
    "unknown"    = '<span class="badge badge-untypeable" title="no type and no error block: a failure TypR did not recognise">unknown</span>',
    "internal"   = '<span class="badge badge-untypeable" title="internal error in NativeSem">internal</span>',
    sprintf('<span class="badge">%s</span>', esc(status))
  )
}

# --- R source snippets ---
# TypR reports where each R definition starts. It ends where the next one in
# the same file starts, less the blank and comment lines that belong to that
# next one (its roxygen block). No ctags needed, and anonymous statements,
# backticked names and S3 methods are located like any other definition.
r_file_cache <- new.env(parent = emptyenv())
r_snippet <- function(pkg, file, line, next_line, max_lines = 200L) {
  if (is.na(file) || is.na(line)) return(NULL)
  fp <- file.path(sources_root, pkg, file)
  lines <- r_file_cache[[fp]]
  if (is.null(lines)) {
    lines <- if (file.exists(fp)) tryCatch(readLines(fp, warn = FALSE), error = function(e) character()) else character()
    r_file_cache[[fp]] <- lines
  }
  if (line > length(lines)) return(NULL)
  end_l <- if (is.na(next_line)) length(lines) else next_line - 1L
  end_l <- min(end_l, line + max_lines, length(lines))
  while (end_l > line && grepl("^\\s*(#.*)?$", lines[end_l])) end_l <- end_l - 1L
  list(file = file, line = line, lang = "r", snippet = paste(lines[line:end_l], collapse = "\n"))
}

# One card per native function or R definition. `r` is its functions.csv row;
# `g` the same R definition in the gradual run (NULL for native functions,
# whose typing does not depend on the mode).
render_function_block <- function(pkg, r, g = NULL, is_entry = FALSE, entry_conv = character(),
                                  snip = NULL, with_source = TRUE,
                                  change = list(kind = "unchanged", html = "")) {
  name <- r$function_name
  status <- r$status
  status_g <- if (is.null(g)) status else g$status
  hh <- character()
  entry_attr <- if (is_entry) "true" else "false"
  conv_attr <- if (length(entry_conv) > 0L) paste(entry_conv, collapse = ",") else ""
  conv_badges <- if (length(entry_conv) > 0L) {
    paste(sprintf(' <span class="badge badge-conv">%s</span>', esc(entry_conv)), collapse = "")
  } else ""
  changed_attr <- if (!identical(change$kind, "unchanged")) "true" else "false"
  empty_ret <- isTRUE(is_empty_ret(r))
  empty_attr <- if (empty_ret) "true" else "false"
  empty_badge <- if (empty_ret)
    ' <span class="badge badge-empty" title="result type is empty (bottom) &mdash; inference found no usable return; often a red flag">empty return</span>' else ''
  status_html <- if (identical(status, status_g)) status_badge(status) else
    sprintf('<span class="st-strict">%s</span><span class="st-gradual">%s</span>',
            status_badge(status), status_badge(status_g))
  unbound <- if (isTRUE(!is.na(r$unbound_kind)) && !is.na(r$error_name)) r$error_name else ""
  annotated_badge <- if (isTRUE(as.logical(r$annotated)))
    ' <span class="badge badge-entry" title="checked against a #| signature rather than inferred">annotated</span>' else ''
  hh <- c(hh, sprintf('<div class="func-list" data-side="%s" data-status="%s" data-status-g="%s" data-unbound="%s" data-unbound-kind="%s" data-entry="%s" data-conv="%s" data-name="%s" data-changed="%s" data-empty-ret="%s"><h3>%s %s%s%s%s%s%s</h3>',
    esc(r$side), esc(status), esc(status_g), esc(unbound),
    esc(if (nzchar(unbound)) r$unbound_kind else ""),
    entry_attr, esc(conv_attr), esc(tolower(name)), changed_attr, empty_attr, esc(name),
    status_html,
    if (is_entry) ' <span class="badge badge-entry">entry</span>' else '',
    conv_badges,
    annotated_badge,
    change$html,
    empty_badge))
  if (status %in% c("typed", "fallback")) {
    if (!is.na(r$type_sig) && nzchar(r$type_sig)) {
      hh <- c(hh, sprintf('<div class="type-sig">%s</div>', type_sig_html(r$type_sig)))
    }
  }
  if (status != "typed") {
    if (!is.na(r$error_title) && nzchar(r$error_title)) {
      where <- if (!is.na(getcol(r, "error_line")[1])) sprintf(' <span class="unbound-line">(line %d)</span>', as.integer(r$error_line)) else ""
      hh <- c(hh, sprintf('<div class="error-line">%s%s</div>', esc(r$error_title), where))
    }
    if (nzchar(unbound)) {
      hh <- c(hh, sprintf('<div class="unbound-line">unbound: <code>%s</code> &mdash; %s</div>', esc(unbound),
        switch(r$unbound_kind,
          prelude = "a name the prelude does not declare",
          package = "one of the package&rsquo;s own names, itself untyped",
          native  = "a native symbol TypR could not bind",
          esc(r$unbound_kind))))
    }
    if (!is.na(r$error_detail) && nzchar(r$error_detail)) {
      hh <- c(hh, sprintf('<details class="error-detail"><summary>error detail</summary><pre>%s</pre></details>',
        esc(r$error_detail)))
    }
  }
  # The gradual run, when it saw this definition differently.
  if (!is.null(g) && (!identical(status, status_g) || !identical(r$type_sig, g$type_sig))) {
    body <- if (isTRUE(!is.na(g$type_sig))) sprintf('<div class="type-sig">%s</div>', type_sig_html(g$type_sig))
            else if (isTRUE(!is.na(g$error_title))) sprintf('<div class="error-line">%s</div>', esc(g$error_title))
            else ""
    hh <- c(hh, sprintf('<details><summary>gradual mode: %s</summary>%s</details>', esc(status_g), body))
  }
  if (!is.na(r$timing_sec)) {
    hh <- c(hh, sprintf('<div class="timing">timing: %s</div>', fmt_func_timing(r$timing_sec)))
  }
  if (with_source) {
    if (is.null(snip)) {
      hh <- c(hh, sprintf('<div class="source-meta"><em>(definition not located in %s)</em></div>',
                          if (r$side == "r") "R/" else "src/"))
    } else {
      hh <- c(hh, sprintf('<details><summary>source</summary><pre class="source language-%s"><code class="language-%s">%s</code></pre><div class="source-meta">%s:%d</div></details>',
        snip$lang, snip$lang, esc(snip$snippet), esc(snip$file), snip$line))
    }
  }
  hh <- c(hh, '</div>')
  paste(hh, collapse = "")
}

STATUS_ORDER <- c("typed", "fallback", "untypeable", "timeout", "skipped", "unknown", "internal")

write_package_page <- function(pkg) {
  si <- match(pkg, summary_df$package)
  if (is.na(si)) return(invisible(NULL))
  here   <- functions_df[functions_df$package == pkg, , drop = FALSE]
  rows   <- here[here$side == "native" & here$mode == "strict", , drop = FALSE]
  r_rows <- here[here$side == "r" & here$mode == "strict", , drop = FALSE]
  g_by_key <- here[here$side == "r" & here$mode == "gradual", , drop = FALSE]
  g_by_key <- split(g_by_key, g_by_key$key)
  has_r <- nrow(r_rows) > 0L

  # Entry points: TypR lists them in entry_points.csv; NativeSem printed them.
  ep_tbl <- if (!is.null(entry_points_df)) {
    e <- entry_points_df[entry_points_df$package == pkg, , drop = FALSE]
    data.frame(name = e$symbol, convention = sprintf(".%s", e$convention), stringsAsFactors = FALSE)
  } else parse_ep_table(file.path(raw_dir, paste0(pkg, ".out")))
  ep_set <- unique(ep_tbl$name)
  ep_missing <- setdiff(ep_set, rows$function_name)
  has_native <- nrow(rows) > 0L || length(ep_missing) > 0L

  have_baseline <- !is.null(b_status_by_key)
  base_status_for <- function(key) {
    if (!have_baseline) return(NA_character_)
    # b_status_by_key is a named character vector, so `[[` on an absent key
    # errors ("subscript out of bounds") rather than returning NULL. Functions
    # newly reached by the call graph won't be in the baseline, so guard the key.
    if (!(key %in% names(b_status_by_key))) return(NA_character_)
    unname(b_status_by_key[[key]])
  }
  # Definitions present in the baseline for this package but absent now.
  # Exclude entry points already rendered as 'unknown' (ep_missing) so cards
  # don't double up and the counts stay accurate.
  gone_of <- function(side, current) {
    if (!have_baseline) return(functions_df[0, , drop = FALSE])
    b_here <- b_functions_df[b_functions_df$package == pkg &
                             b_functions_df$side == side & b_functions_df$mode == "strict", , drop = FALSE]
    b_here <- b_here[!(b_here$key %in% current$key) & !(b_here$function_name %in% ep_missing), , drop = FALSE]
    b_here[order(b_here$function_name), , drop = FALSE]
  }
  gone   <- gone_of("native", rows)
  r_gone <- gone_of("r", r_rows)

  n_native_cards <- nrow(rows) + length(ep_missing) + nrow(gone)
  n_r_cards <- nrow(r_rows) + nrow(r_gone)
  total_cards <- n_native_cards + n_r_cards

  subtitle <- sprintf('%d functions analysed &middot; %d typed &middot; %d untypeable &middot; %d timed out &middot; %d entry points (%d typed)',
    nat$n[si], nat$typed[si], nat$untypeable[si], nat$timeout[si], nat$ep[si], nat$ep_typed[si])
  if (HAVE_R) {
    subtitle <- sprintf('%s &middot; R: %s definitions, %s typed strict, %s gradual, prelude debt %s',
      if (has_native) paste("native:", subtitle) else "no native code",
      rs$n[si], rs$typed[si], rs$g_typed[si], rs$unbound_prelude[si])
  }

  ph <- character()
  ph <- c(ph, '<!DOCTYPE html>',
    '<html lang="en"><head><meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width, initial-scale=1">',
    sprintf('<title>%s &middot; r-typing</title>', esc(pkg)),
    common_style(),
    prism_head("../"),
    '</head><body>',
    '<a href="../index.html" class="back-link">&larr; all packages</a>',
    sprintf('<h1><span class="r">%s</span></h1>', esc(pkg)),
    sprintf('<p class="subtitle">%s%s</p>', subtitle, checker_version_html()),
    checker_status_html())
  if (HAVE_R) ph <- c(ph, funnel_html(funnel_counts(si)))
  if (!HAVE_R) ph <- c(ph, sprintf('<h2>Functions <small>%d</small></h2>', total_cards))

  # Filter toolbar. Status boxes for every status present; side and mode
  # switches only where the page has both.
  statuses <- unique(c(rows$status, r_rows$status,
                       unlist(lapply(g_by_key, function(g) g$status), use.names = FALSE)))
  statuses <- c(intersect(STATUS_ORDER, statuses), sort(setdiff(statuses, STATUS_ORDER)))
  if (!HAVE_R) statuses <- union(c("typed", "untypeable", "timeout"), statuses)
  status_boxes <- vapply(statuses, function(s)
    sprintf('<label><input type="checkbox" data-status-box="%s" checked> %s</label>', esc(s), esc(s)),
    character(1))
  radio <- function(name, legend, values, labels) sprintf('<fieldset><legend>%s</legend>%s</fieldset>', legend,
    paste(sprintf('<label><input type="radio" name="%s" value="%s"%s> %s</label>', name, values,
                  ifelse(seq_along(values) == 1L, " checked", ""), labels), collapse = ""))
  ph <- c(ph,
    '<div class="filters">',
    if (has_native && has_r) radio("side", "side", c("all", "native", "r"), c("all", "native", "R")) else '',
    if (has_r) radio("mode", "mode", c("strict", "gradual"), c("strict", "gradual")) else '',
    if (has_native) '<label><input type="checkbox" id="flt-ep"> Entry points only</label>' else '',
    paste(status_boxes, collapse = ""),
    if (have_baseline) '<label><input type="checkbox" id="flt-changed"> changed only</label>' else '',
    '<label><input type="checkbox" id="flt-empty"> empty return only</label>',
    '<span class="filter-chip" id="flt-unbound" hidden></span>',
    '<input type="search" id="flt-name" placeholder="name…">',
    sprintf('<span class="count"><span id="flt-count">%d</span> / %d</span>', total_cards, total_cards),
    '</div>')

  baseline_change <- function(r) if (have_baseline)
    change_info(r$status, base_status_for(r$key)) else list(kind = "unchanged", html = "")
  # A native function leaving a package whose scope moved was dropped from the
  # analysis, not lost: neutral rather than red.
  gone_card <- function(g, side) {
    dropped <- side == "native" && scope_moved(pkg)
    sprintf(
      '<div class="func-list" data-side="%s" data-status="gone" data-entry="%s" data-conv="" data-name="%s" data-changed="true" data-empty-ret="false"><h3>%s <span class="badge %s">%s</span></h3><div class="error-line">was <strong>%s</strong> in the baseline; %s</div></div>',
      side, if (g$function_name %in% ep_set) "true" else "false", esc(tolower(g$function_name)),
      esc(g$function_name),
      if (dropped) "badge-change-neutral" else "badge-change-bad", if (dropped) "not analysed" else "gone",
      esc(g$status),
      if (dropped) "the set of analysed C functions changed and no longer includes it"
      else "no longer typed/reached in this run")
  }

  if (total_cards == 0) {
    ph <- c(ph, '<p class="na">No functions recorded.</p>')
  }
  if (has_native) {
    if (HAVE_R) ph <- c(ph, sprintf('<section data-section="native"><h2>Native functions <small>%d</small></h2>', n_native_cards))
    if (nrow(rows) > 0) {
      ord <- order(match(rows$status, STATUS_ORDER), rows$function_name)
      rows <- rows[ord, , drop = FALSE]
      for (i in seq_len(nrow(rows))) {
        r <- rows[i, ]
        is_ep <- r$function_name %in% ep_set
        ph <- c(ph, render_function_block(pkg, r,
          is_entry = is_ep,
          entry_conv = if (is_ep) ep_conv_for(ep_tbl, r$function_name) else character(),
          snip = source_snippet(pkg, r$function_name),
          change = baseline_change(r)))
      }
    }
    # Functions that existed in the baseline but are gone now (status change:
    # disappeared). Rendered as lightweight cards so the "changed only" filter
    # surfaces regressions in coverage.
    for (j in seq_len(nrow(gone))) ph <- c(ph, gone_card(gone[j, ], "native"))
    # Entry points the R code reaches with no definition among the native
    # functions: rendered as 'unknown' cards.
    for (nm in ep_missing) {
      convs <- ep_conv_for(ep_tbl, nm)
      conv_attr <- paste(convs, collapse = ",")
      conv_badges <- paste(sprintf(' <span class="badge badge-conv">%s</span>', esc(convs)), collapse = "")
      ph <- c(ph, sprintf(
        '<div class="func-list" data-side="native" data-status="unknown" data-entry="true" data-conv="%s" data-name="%s" data-changed="false" data-empty-ret="false"><h3>%s <span class="badge">unknown</span> <span class="badge badge-entry">entry</span>%s</h3><div class="error-line">%s</div></div>',
        esc(conv_attr), esc(tolower(nm)), esc(nm), conv_badges,
        if (!is.null(entry_points_df)) "called from R, but not defined in src/ (registered under another name, generated, or defined in a dependency)"
        else "no record in functions.csv (likely the entry point itself was filtered)"))
    }
    if (HAVE_R) ph <- c(ph, '</section>')
  }
  if (has_r) {
    ph <- c(ph, sprintf('<section data-section="r"><h2>R definitions <small>%d</small></h2>', n_r_cards))
    # In the order Rsem checked them, one group per file. A definition's
    # source runs up to the next definition of its file.
    next_line <- ave(r_rows$line, r_rows$file, FUN = function(l) {
      s <- sort(unique(l[!is.na(l)]))
      vapply(l, function(x) { n <- s[s > x]; if (length(n)) n[1] else NA_real_ }, numeric(1))
    })
    for (f in unique(r_rows$file)) {
      ph <- c(ph, sprintf('<div class="file-group"><div class="file-head">%s</div>', esc(f)))
      for (i in which(r_rows$file == f)) {
        r <- r_rows[i, ]
        g <- g_by_key[[gradual_key(r$key)]]
        ph <- c(ph, render_function_block(pkg, r, g = g,
          snip = r_snippet(pkg, r$file, r$line, next_line[i]),
          change = baseline_change(r)))
      }
      ph <- c(ph, '</div>')
    }
    for (j in seq_len(nrow(r_gone))) ph <- c(ph, gone_card(r_gone[j, ], "r"))
    ph <- c(ph, '</section>')
  }

  ph <- c(ph, '<footer><a href="../index.html">&larr; all packages</a></footer>',
    '<script>',
    '(function () {',
    '  const ep = document.getElementById("flt-ep"); // null without native code',
    '  const stat = {};',
    '  document.querySelectorAll("input[data-status-box]").forEach(b => { stat[b.dataset.statusBox] = b; });',
    '  const changedOnly = document.getElementById("flt-changed"); // null when no baseline',
    '  const emptyOnly = document.getElementById("flt-empty");',
    '  const nameInput = document.getElementById("flt-name");',
    '  const unboundChip = document.getElementById("flt-unbound");',
    '  const cards = document.querySelectorAll(".func-list[data-status]");',
    '  const groups = document.querySelectorAll(".file-group, section[data-section]");',
    '  const counter = document.getElementById("flt-count");',
    '  const radio = name => { const r = document.querySelector(`input[name="${name}"]:checked`); return r ? r.value : null; };',
    '  // #empty: empty returns only. #unbound: blocked by a prelude gap;',
    '  // #unbound=<name>: blocked by that name (links from the index page).',
    '  let unbound = null;',
    '  if (location.hash === "#empty" && emptyOnly) emptyOnly.checked = true;',
    '  if (location.hash.startsWith("#unbound")) {',
    '    const eq = location.hash.indexOf("=");',
    '    unbound = { name: eq < 0 ? null : decodeURIComponent(location.hash.slice(eq + 1)) };',
    '    unboundChip.innerHTML = "";',
    '    unboundChip.append(unbound.name ? "blocked by " : "blocked by a prelude gap");',
    '    if (unbound.name) { const c = document.createElement("code"); c.textContent = unbound.name; unboundChip.append(c); }',
    '    const x = document.createElement("button"); x.textContent = " ×"; x.title = "clear";',
    '    x.addEventListener("click", () => { unbound = null; unboundChip.hidden = true; history.replaceState(null, "", location.pathname); apply(); });',
    '    unboundChip.append(x); unboundChip.hidden = false;',
    '  }',
    '  function apply() {',
    '    const side = radio("side") || "all";',
    '    const gradual = radio("mode") === "gradual";',
    '    document.body.classList.toggle("mode-gradual", gradual);',
    '    const epOnly = ep && ep.checked;',
    '    const wantChanged = changedOnly && changedOnly.checked;',
    '    const wantEmpty = emptyOnly && emptyOnly.checked;',
    '    const q = (nameInput.value || "").toLowerCase();',
    '    let visible = 0;',
    '    cards.forEach(c => {',
    '      const s = gradual && c.dataset.statusG ? c.dataset.statusG : c.dataset.status;',
    '      const box = stat[s];',
    '      const okSide = side === "all" || c.dataset.side === side;',
    '      const okEp = !epOnly || c.dataset.entry === "true";',
    '      // A status without a checkbox (gone, unknown) is never filtered out.',
    '      const okStat = !box || box.checked;',
    '      const okChanged = !wantChanged || c.dataset.changed === "true";',
    '      const okEmpty = !wantEmpty || c.dataset.emptyRet === "true";',
    '      const okUnbound = !unbound || (unbound.name ? c.dataset.unbound === unbound.name : c.dataset.unboundKind === "prelude");',
    '      const okName = !q || c.dataset.name.indexOf(q) !== -1;',
    '      const show = okSide && okEp && okStat && okChanged && okEmpty && okUnbound && okName;',
    '      c.style.display = show ? "" : "none";',
    '      if (show) visible++;',
    '    });',
    '    // Hide a file or a section once none of its cards is left.',
    '    groups.forEach(g => {',
    '      const any = Array.from(g.querySelectorAll(".func-list")).some(c => c.style.display !== "none");',
    '      g.style.display = any ? "" : "none";',
    '    });',
    '    counter.textContent = visible;',
    '  }',
    '  document.querySelectorAll(".filters input:not([type=search])")',
    '    .forEach(el => el.addEventListener("change", apply));',
    '  nameInput.addEventListener("input", apply);',
    '  apply();',
    '})();',
    '</script>',
    prism_script("../"),
    '</body></html>')

  writeLines(ph, file.path(pkg_dir, paste0(pkg, ".html")))
}

for (pkg in summary_df$package) {
  tryCatch(write_package_page(pkg), error = function(e) {
    message(sprintf("failed to write detail page for %s: %s", pkg, conditionMessage(e)))
  })
}
cat(sprintf("Wrote %d per-package pages to %s/\n", nrow(summary_df), pkg_dir))
