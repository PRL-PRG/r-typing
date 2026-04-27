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

raw_dir <- "work/raw_output"
sources_root <- "work/sources"
pkg_dir <- file.path(dirname(output_file), "pkg")
dir.create(pkg_dir, recursive = TRUE, showWarnings = FALSE)

# --- Aggregate stats ---
n_packages    <- nrow(summary_df)
n_crashed     <- sum(summary_df$crashed == 1)
n_ok          <- n_packages - n_crashed

total_functions  <- sum(summary_df$n_functions, na.rm = TRUE)
total_typed      <- sum(summary_df$n_typed, na.rm = TRUE)
total_untypeable <- sum(summary_df$n_untypeable, na.rm = TRUE)
total_timeout    <- if ("n_timeout" %in% names(summary_df)) sum(summary_df$n_timeout, na.rm = TRUE) else 0L
total_ep_call    <- sum(summary_df$ep_call,     na.rm = TRUE)
total_ep_c       <- sum(summary_df$ep_c,        na.rm = TRUE)
total_ep_fortran <- sum(summary_df$ep_fortran,  na.rm = TRUE)
total_ep_external<- sum(summary_df$ep_external, na.rm = TRUE)
total_ep         <- total_ep_call + total_ep_c + total_ep_fortran + total_ep_external
total_ep_typed   <- sum(summary_df$n_ep_typed,  na.rm = TRUE)

pct_overall  <- if (total_functions > 0) round(100 * total_typed / total_functions, 1) else 0
pct_ep_typed <- if (total_ep > 0) round(100 * total_ep_typed / total_ep, 1) else 0

# Per-error-category counts (status untypeable+timeout, grouped by error_title;
# all timeouts go into a single "timeout" bucket).
error_category_counts <- function(fdf) {
  if (nrow(fdf) == 0) return(setNames(integer(0), character(0)))
  ut <- fdf[fdf$status %in% c("untypeable", "timeout"), ]
  if (nrow(ut) == 0) return(setNames(integer(0), character(0)))
  cat_label <- ifelse(ut$status == "timeout", "timeout", ut$error_title)
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

pct_bar <- function(typed, total) {
  if (is.na(total) || total == 0) return('<span class="na">no functions analysed</span>')
  pct <- round(100 * typed / total, 1)
  hue <- round(pct * 1.2)  # 0=red, 120=green
  sprintf(
    '<div class="bar-bg"><div class="bar-fill" style="width:%.1f%%;background:hsl(%d,70%%,45%%)"></div><span class="bar-label">%d / %d (%.1f%%)</span></div>',
    pct, hue, typed, total, pct
  )
}

# good_dir = +1 (higher is better) or -1 (lower is better)
delta_html <- function(delta, good_dir = 1) {
  if (is.na(delta)) return('<span class="delta delta-na">&mdash;</span>')
  if (delta == 0)  return('<span class="delta delta-zero">0</span>')
  good <- (delta > 0 && good_dir > 0) || (delta < 0 && good_dir < 0)
  cls <- if (good) "delta-good" else "delta-bad"
  sign <- if (delta > 0) "+" else "" # negative numbers already include their sign
  sprintf('<span class="delta %s">%s%d</span>', cls, sign, delta)
}

progress_card <- function(label, before, after, good_dir = 1) {
  delta <- after - before
  sprintf(
    '<div class="stat-card progress"><div class="value">%d</div><div class="label">%s</div><div class="progress-line">%d &rarr; %d %s</div></div>',
    after, esc(label), before, after, delta_html(delta, good_dir)
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
if (!HAVE_CTAGS) message("ctags not in PATH; source snippets disabled.")

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
  list(file = fp, line = start_l, snippet = paste(lines[start_l:end_l], collapse = "\n"))
}

# --- Entry-point name extraction (lifted from parse_output.R) ---
parse_ep_names <- function(out_path) {
  if (!file.exists(out_path)) return(character())
  lines <- readLines(out_path, warn = FALSE)
  ep_names <- character()
  ep_header_idx <- grep("^Entry points for \\.", lines)
  for (hi in ep_header_idx) {
    j <- hi + 1L
    while (j <= length(lines) && grepl("^\\s+\\S", lines[j])) {
      ep_names <- c(ep_names, trimws(lines[j]))
      j <- j + 1L
    }
  }
  unique(ep_names)
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
    '.error-row td:first-child { font-family: monospace; font-size: .9rem; }',
    '.ep-breakdown { font-size: .85rem; color: var(--muted); }',
    '.ep-breakdown code { font-family: "JetBrains Mono", monospace; font-size: .8rem; }',
    '.func-list { background: var(--card); border: 1px solid var(--border); border-radius: 8px; padding: 1rem; margin-bottom: 1rem; }',
    '.func-list h3 { font-family: "JetBrains Mono", monospace; font-size: 1rem; margin-bottom: .25rem; }',
    '.type-sig { font-family: "JetBrains Mono", monospace; font-size: .85rem; color: var(--text); white-space: pre-wrap; word-break: break-word; }',
    '.error-line { font-family: "JetBrains Mono", monospace; font-size: .85rem; color: var(--bad); white-space: pre-wrap; }',
    '.error-detail pre { font-family: "JetBrains Mono", monospace; font-size: .8rem; background: #f8f8f8; padding: .5rem; border-radius: 4px; overflow-x: auto; white-space: pre-wrap; }',
    'details { margin-top: .5rem; }',
    'details summary { cursor: pointer; color: var(--accent); font-size: .85rem; user-select: none; }',
    'details summary:hover { text-decoration: underline; }',
    'details pre.source { font-family: "JetBrains Mono", monospace; font-size: .8rem; background: #f8f8f8; padding: .5rem; border-radius: 4px; overflow-x: auto; line-height: 1.4; margin-top: .25rem; max-height: 600px; overflow-y: auto; }',
    'details .source-meta { color: var(--muted); font-size: .75rem; margin-top: .25rem; }',
    '.back-link { display: inline-block; margin-bottom: 1rem; color: var(--accent); text-decoration: none; font-size: .9rem; }',
    '.back-link:hover { text-decoration: underline; }',
    'footer { margin-top: 3rem; text-align: center; color: var(--muted); font-size: .8rem; }',
    '</style>'
  )
}

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
h(sprintf('<p class="subtitle">Type-checking results for %d CRAN packages &mdash; generated %s</p>',
  n_packages, format(Sys.time(), "%Y-%m-%d %H:%M")))

# Stat cards
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

# Per-package table
h('<h2>Per-package results</h2>')
max_elapsed <- suppressWarnings(max(summary_df$elapsed_sec, na.rm = TRUE))
time_digits <- if (!is.finite(max_elapsed)) 1L else if (max_elapsed < 10) 2L else if (max_elapsed < 100) 1L else 0L
time_fmt <- paste0("%.", time_digits, "f s")
fmt_time <- function(t) if (is.na(t)) '<span class="na">&mdash;</span>' else sprintf(time_fmt, t)

# Build a baseline lookup keyed by package name for quick delta computation.
b_typed_by_pkg <- if (!is.null(b_summary_df)) {
  setNames(b_summary_df$n_typed, b_summary_df$package)
} else NULL

extra_th <- if (!is.null(baseline_dir)) '<th>Change</th>' else ''
h('<table><thead><tr>',
  '<th>Package</th><th>Entrypoints</th><th>.Call</th><th>.C</th><th>.Fortran</th><th>.External</th>',
  '<th>EP typing progress</th><th>Functions analysed</th>', extra_th,
  '<th>Timed out</th><th>Typing progress</th><th>Typing time</th><th>Status</th>',
  '</tr></thead><tbody>')
for (i in seq_len(nrow(summary_df))) {
  r <- summary_df[i, ]
  ep <- r$ep_call + r$ep_c + r$ep_fortran + r$ep_external
  n_to <- if ("n_timeout" %in% names(r)) r$n_timeout else 0L
  badge <- if (r$crashed) '<span class="badge badge-crash">crashed</span>' else '<span class="badge badge-ok">OK</span>'
  pkg_link <- sprintf('<a href="pkg/%s.html">%s</a>', esc(r$package), esc(r$package))

  delta_td <- ""
  if (!is.null(baseline_dir)) {
    if (!is.null(b_typed_by_pkg) && r$package %in% names(b_typed_by_pkg)) {
      d <- r$n_typed - as.integer(b_typed_by_pkg[r$package])
      delta_td <- sprintf('<td class="num">%s</td>', delta_html(d, +1))
    } else {
      delta_td <- '<td class="num"><span class="delta delta-good">new</span></td>'
    }
  }

  h(sprintf('<tr><td class="pkg">%s</td><td class="num">%d</td><td class="num">%d</td><td class="num">%d</td><td class="num">%d</td><td class="num">%d</td><td>%s</td><td class="num">%d</td>%s<td class="num">%d</td><td>%s</td><td class="num">%s</td><td>%s</td></tr>',
    pkg_link, ep, r$ep_call, r$ep_c, r$ep_fortran, r$ep_external,
    pct_bar(r$n_ep_typed, ep), r$n_functions, delta_td, n_to,
    pct_bar(r$n_typed, r$n_functions),
    fmt_time(r$elapsed_sec), badge))
}

# Rows for packages that disappeared (present in baseline only).
if (!is.null(b_summary_df)) {
  gone <- setdiff(b_summary_df$package, summary_df$package)
  for (pkg in gone) {
    h(sprintf('<tr><td class="pkg">%s</td><td colspan="%d"><span class="delta delta-bad">gone</span> (was in baseline, not in current run)</td></tr>',
      esc(pkg), if (!is.null(baseline_dir)) 12L else 11L))
  }
}
h('</tbody></table>')

# Error categories
if (nrow(error_cats) > 0) {
  h('<h2>Error and timeout categories</h2>')
  base_col <- if (!is.null(baseline_dir)) '<th style="text-align:right">Before</th><th style="text-align:right">&Delta;</th>' else ''
  h('<table><thead><tr><th>Error</th>', base_col, '<th style="text-align:right">Count</th></tr></thead><tbody>')
  for (i in seq_len(nrow(error_cats))) {
    cat <- error_cats$category[i]
    cnt <- error_cats$count[i]
    extra <- ""
    if (!is.null(baseline_dir)) {
      bcnt <- as.integer(err_base[cat]); if (is.na(bcnt)) bcnt <- 0L
      d <- cnt - bcnt
      extra <- sprintf('<td class="num">%d</td><td class="num">%s</td>', bcnt, delta_html(d, -1))
    }
    h(sprintf('<tr class="error-row"><td>%s</td>%s<td class="num">%d</td></tr>',
      esc(cat), extra, cnt))
  }
  # Categories present in baseline but absent now (good!)
  if (!is.null(baseline_dir)) {
    extinct <- setdiff(names(err_base), names(err_now))
    for (cat in extinct) {
      bcnt <- as.integer(err_base[cat])
      h(sprintf('<tr class="error-row"><td>%s</td><td class="num">%d</td><td class="num">%s</td><td class="num">0</td></tr>',
        esc(cat), bcnt, delta_html(-bcnt, -1)))
    }
  }
  h('</tbody></table>')
}

# Crashes
if (nrow(crashes_df) > 0) {
  h('<h2>Crash details</h2>')
  h('<table><thead><tr><th>Package</th><th>Error</th></tr></thead><tbody>')
  for (i in seq_len(nrow(crashes_df))) {
    h(sprintf('<tr><td class="pkg">%s</td><td class="crash-msg">%s</td></tr>',
      esc(crashes_df$package[i]), esc(crashes_df$error_message[i])))
  }
  h('</tbody></table>')
}

h('<footer>Generated by <strong>r-typing</strong> pipeline')
if (!is.null(baseline_dir)) h(sprintf(' &middot; baseline: <code>%s</code>', esc(baseline_dir)))
h('</footer>')
h('</body></html>')

writeLines(html, output_file)
cat(sprintf("Wrote %s\n", output_file))

# --- Build PER-PACKAGE pages ---

status_badge <- function(status) {
  switch(status,
    "typed"      = '<span class="badge badge-typed">typed</span>',
    "untypeable" = '<span class="badge badge-untypeable">untypeable</span>',
    "timeout"    = '<span class="badge badge-timeout">timeout</span>',
    sprintf('<span class="badge">%s</span>', esc(status))
  )
}

render_function_block <- function(pkg, name, status, type_sig, error_title, error_detail,
                                  is_entry = FALSE, with_source = TRUE) {
  hh <- character()
  entry_attr <- if (is_entry) "true" else "false"
  hh <- c(hh, sprintf('<div class="func-list" data-status="%s" data-entry="%s" data-name="%s"><h3>%s %s%s</h3>',
    esc(status), entry_attr, esc(tolower(name)), esc(name),
    status_badge(status),
    if (is_entry) ' <span class="badge badge-entry">entry</span>' else ''))
  if (status == "typed") {
    if (!is.na(type_sig) && nzchar(type_sig)) {
      hh <- c(hh, sprintf('<div class="type-sig">%s</div>', esc(type_sig)))
    }
  } else {
    if (!is.na(error_title) && nzchar(error_title)) {
      hh <- c(hh, sprintf('<div class="error-line">%s</div>', esc(error_title)))
    }
    if (!is.na(error_detail) && nzchar(error_detail)) {
      hh <- c(hh, sprintf('<details class="error-detail"><summary>error detail</summary><pre>%s</pre></details>',
        esc(error_detail)))
    }
  }
  if (with_source) {
    snip <- source_snippet(pkg, name)
    if (is.null(snip)) {
      hh <- c(hh, '<div class="source-meta"><em>(definition not located in src/)</em></div>')
    } else {
      hh <- c(hh, sprintf('<details><summary>source</summary><pre class="source">%s</pre><div class="source-meta">%s:%d</div></details>',
        esc(snip$snippet), esc(snip$file), snip$line))
    }
  }
  hh <- c(hh, '</div>')
  paste(hh, collapse = "")
}

write_package_page <- function(pkg) {
  rows <- functions_df[functions_df$package == pkg, , drop = FALSE]
  ep_names <- parse_ep_names(file.path(raw_dir, paste0(pkg, ".out")))
  s <- summary_df[summary_df$package == pkg, , drop = FALSE]
  if (nrow(s) == 0) return(invisible(NULL))
  s <- s[1, ]
  ep_total <- s$ep_call + s$ep_c + s$ep_fortran + s$ep_external

  ph <- character()
  ph <- c(ph, '<!DOCTYPE html>',
    '<html lang="en"><head><meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width, initial-scale=1">',
    sprintf('<title>%s &middot; r-typing</title>', esc(pkg)),
    common_style(),
    '</head><body>',
    '<a href="../index.html" class="back-link">&larr; all packages</a>',
    sprintf('<h1><span class="r">%s</span></h1>', esc(pkg)),
    sprintf('<p class="subtitle">%d functions analysed &middot; %d typed &middot; %d untypeable &middot; %d timed out &middot; %d entry points (%d typed)</p>',
      s$n_functions, s$n_typed, s$n_untypeable, if ("n_timeout" %in% names(s)) s$n_timeout else 0L,
      ep_total, s$n_ep_typed))

  # Unified function list with filter toolbar.
  ep_set <- ep_names
  ep_in_csv <- ep_set[ep_set %in% rows$function_name]
  ep_missing <- setdiff(ep_set, ep_in_csv)
  total_cards <- nrow(rows) + length(ep_missing)

  ph <- c(ph, sprintf('<h2>Functions <small>%d</small></h2>', total_cards))
  ph <- c(ph,
    '<div class="filters">',
    '<label><input type="checkbox" id="flt-ep"> Entry points only</label>',
    '<label><input type="checkbox" id="flt-typed" checked> typed</label>',
    '<label><input type="checkbox" id="flt-untypeable" checked> untypeable</label>',
    '<label><input type="checkbox" id="flt-timeout" checked> timeout</label>',
    '<input type="search" id="flt-name" placeholder="name…">',
    sprintf('<span class="count"><span id="flt-count">%d</span> / %d</span>', total_cards, total_cards),
    '</div>')

  if (total_cards == 0) {
    ph <- c(ph, '<p class="na">No functions recorded.</p>')
  } else {
    if (nrow(rows) > 0) {
      ord <- order(match(rows$status, c("typed", "untypeable", "timeout")), rows$function_name)
      rows <- rows[ord, , drop = FALSE]
      for (i in seq_len(nrow(rows))) {
        r <- rows[i, ]
        ph <- c(ph, render_function_block(pkg, r$function_name, r$status,
          r$type_sig, r$error_title, r$error_detail,
          is_entry = r$function_name %in% ep_set,
          with_source = TRUE))
      }
    }
    # Entry points missing from functions.csv: render as 'unknown' cards.
    for (nm in ep_missing) {
      ph <- c(ph, sprintf(
        '<div class="func-list" data-status="unknown" data-entry="true" data-name="%s"><h3>%s <span class="badge">unknown</span> <span class="badge badge-entry">entry</span></h3><div class="error-line">no record in functions.csv (likely the entry point itself was filtered)</div></div>',
        esc(tolower(nm)), esc(nm)))
    }
  }

  ph <- c(ph, '<footer><a href="../index.html">&larr; all packages</a></footer>',
    '<script>',
    '(function () {',
    '  const ep = document.getElementById("flt-ep");',
    '  const stat = {',
    '    typed: document.getElementById("flt-typed"),',
    '    untypeable: document.getElementById("flt-untypeable"),',
    '    timeout: document.getElementById("flt-timeout"),',
    '  };',
    '  const nameInput = document.getElementById("flt-name");',
    '  const cards = document.querySelectorAll(".func-list[data-status]");',
    '  const counter = document.getElementById("flt-count");',
    '  function apply() {',
    '    const epOnly = ep.checked;',
    '    const q = (nameInput.value || "").toLowerCase();',
    '    let visible = 0;',
    '    cards.forEach(c => {',
    '      const s = c.dataset.status;',
    '      const e = c.dataset.entry === "true";',
    '      const n = c.dataset.name;',
    '      const okEp = !epOnly || e;',
    '      const okStat = s === "unknown" || (stat[s] && stat[s].checked);',
    '      const okName = !q || n.indexOf(q) !== -1;',
    '      const show = okEp && okStat && okName;',
    '      c.style.display = show ? "" : "none";',
    '      if (show) visible++;',
    '    });',
    '    counter.textContent = visible;',
    '  }',
    '  [ep, stat.typed, stat.untypeable, stat.timeout].forEach(el => el.addEventListener("change", apply));',
    '  nameInput.addEventListener("input", apply);',
    '  apply();',
    '})();',
    '</script>',
    '</body></html>')

  writeLines(ph, file.path(pkg_dir, paste0(pkg, ".html")))
}

for (pkg in summary_df$package) {
  tryCatch(write_package_page(pkg), error = function(e) {
    message(sprintf("failed to write detail page for %s: %s", pkg, conditionMessage(e)))
  })
}
cat(sprintf("Wrote %d per-package pages to %s/\n", nrow(summary_df), pkg_dir))
