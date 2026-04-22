#!/usr/bin/env Rscript
# Generate a summary webpage from typing results.
# Usage: Rscript scripts/generate_webpage.R results/ output.html

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: Rscript generate_webpage.R <results_dir> <output.html>")
}

results_dir <- args[1]
output_file <- args[2]

summary_df  <- read.csv(file.path(results_dir, "summary.csv"), stringsAsFactors = FALSE)
functions_df <- read.csv(file.path(results_dir, "functions.csv"), stringsAsFactors = FALSE)
crashes_df  <- read.csv(file.path(results_dir, "crashes.csv"), stringsAsFactors = FALSE)

# --- Aggregate stats ---
n_packages    <- nrow(summary_df)
n_crashed     <- sum(summary_df$crashed == 1)
n_ok          <- n_packages - n_crashed

total_functions  <- sum(summary_df$n_functions, na.rm = TRUE)
total_typed      <- sum(summary_df$n_typed, na.rm = TRUE)
total_untypeable <- sum(summary_df$n_untypeable, na.rm = TRUE)
total_timeout    <- if ("n_timeout" %in% names(summary_df)) sum(summary_df$n_timeout, na.rm = TRUE) else 0L
total_ep_call    <- sum(summary_df$ep_call, na.rm = TRUE)
total_ep_c       <- sum(summary_df$ep_c, na.rm = TRUE)
total_ep_fortran <- sum(summary_df$ep_fortran, na.rm = TRUE)
total_ep_external<- sum(summary_df$ep_external, na.rm = TRUE)
total_ep         <- total_ep_call + total_ep_c + total_ep_fortran + total_ep_external
total_ep_typed   <- sum(summary_df$n_ep_typed, na.rm = TRUE)

pct_overall    <- if (total_functions > 0) round(100 * total_typed / total_functions, 1) else 0
pct_ep_typed   <- if (total_ep > 0) round(100 * total_ep_typed / total_ep, 1) else 0

# Error categories (includes both untypeable errors and timeouts)
error_cats <- if (nrow(functions_df) > 0) {
  ut <- functions_df[functions_df$status %in% c("untypeable", "timeout"), ]
  if (nrow(ut) > 0) {
    # Group all timeouts into a single "timeout" bucket regardless of the
    # specific duration reported in the message.
    cat_label <- ifelse(ut$status == "timeout", "timeout", ut$error_title)
    tbl <- sort(table(cat_label), decreasing = TRUE)
    data.frame(category = names(tbl), count = as.integer(tbl), stringsAsFactors = FALSE)
  } else {
    data.frame(category = character(0), count = integer(0))
  }
} else {
  data.frame(category = character(0), count = integer(0))
}

# --- HTML helpers ---
esc <- function(x) {
  x <- gsub("&", "&amp;", x)
  x <- gsub("<", "&lt;", x)
  x <- gsub(">", "&gt;", x)
  x <- gsub('"', "&quot;", x)
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

# --- Build HTML ---
html <- character()
h <- function(...) html <<- c(html, paste0(...))

h('<!DOCTYPE html>')
h('<html lang="en"><head><meta charset="utf-8">')
h('<meta name="viewport" content="width=device-width, initial-scale=1">')
h('<title>R we type yet?</title>')
h('<style>')
h(':root { --bg: #fafafa; --card: #fff; --border: #e0e0e0; --text: #1a1a1a; --muted: #666; --accent: #2266cc; }')
h('* { box-sizing: border-box; margin: 0; padding: 0; }')
h('body { font-family: "Inter", system-ui, -apple-system, sans-serif; background: var(--bg); color: var(--text); line-height: 1.5; padding: 2rem 1rem; max-width: 1400px; margin: 0 auto; }')
h('h1 { font-size: 2rem; margin-bottom: .25rem; }')
h('h1 .r { color: var(--accent); }')
h('.subtitle { color: var(--muted); margin-bottom: 2rem; }')
h('.stats { display: grid; grid-template-columns: repeat(auto-fit, minmax(180px, 1fr)); gap: 1rem; margin-bottom: 2rem; }')
h('.stat-card { background: var(--card); border: 1px solid var(--border); border-radius: 8px; padding: 1rem 1.25rem; }')
h('.stat-card .value { font-size: 1.75rem; font-weight: 700; }')
h('.stat-card .label { color: var(--muted); font-size: .85rem; }')
h('h2 { font-size: 1.25rem; margin: 2rem 0 1rem; }')
h('table { width: 100%; border-collapse: collapse; background: var(--card); border: 1px solid var(--border); border-radius: 8px; overflow: hidden; }')
h('th, td { text-align: left; padding: .6rem .8rem; border-bottom: 1px solid var(--border); }')
h('th { background: #f5f5f5; font-size: .85rem; font-weight: 600; text-transform: uppercase; letter-spacing: .03em; color: var(--muted); }')
h('tr:last-child td { border-bottom: none; }')
h('td.pkg { font-weight: 600; font-family: "JetBrains Mono", "Fira Code", monospace; }')
h('td.num { text-align: right; font-family: "JetBrains Mono", monospace; }')
h('.bar-bg { background: #eee; border-radius: 4px; height: 22px; position: relative; overflow: hidden; min-width: 120px; }')
h('.bar-fill { height: 100%; border-radius: 4px; transition: width .3s; }')
h('.bar-label { position: absolute; top: 0; left: 8px; line-height: 22px; font-size: .8rem; font-weight: 600; color: #333; white-space: nowrap; }')
h('.na { color: var(--muted); font-size: .85rem; font-style: italic; }')
h('.badge { display: inline-block; font-size: .75rem; font-weight: 600; padding: .15em .5em; border-radius: 4px; }')
h('.badge-ok { background: #dcfce7; color: #166534; }')
h('.badge-crash { background: #fee2e2; color: #991b1b; }')
h('.crash-msg { font-family: monospace; font-size: .85rem; color: #991b1b; }')
h('.error-row td:first-child { font-family: monospace; font-size: .9rem; }')
h('.ep-breakdown { font-size: .85rem; color: var(--muted); }')
h('.ep-breakdown code { font-family: "JetBrains Mono", monospace; font-size: .8rem; }')
h('footer { margin-top: 3rem; text-align: center; color: var(--muted); font-size: .8rem; }')
h('</style></head><body>')

# Header
h('<h1><span class="r">R</span> we type yet?</h1>')
h(sprintf('<p class="subtitle">Type-checking results for %d CRAN packages &mdash; generated %s</p>', n_packages, format(Sys.time(), "%Y-%m-%d %H:%M")))

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

# Pick one decimal precision for the elapsed-time column so all rows are
# comparable. Finer precision for small values, coarser for big ones.
max_elapsed <- suppressWarnings(max(summary_df$elapsed_sec, na.rm = TRUE))
time_digits <- if (!is.finite(max_elapsed)) {
  1L
} else if (max_elapsed < 10) {
  2L
} else if (max_elapsed < 100) {
  1L
} else {
  0L
}
time_fmt <- paste0("%.", time_digits, "f s")
fmt_time <- function(t) if (is.na(t)) '<span class="na">&mdash;</span>' else sprintf(time_fmt, t)

h('<table><thead><tr><th>Package</th><th>Entrypoints</th><th>.Call</th><th>.C</th><th>.Fortran</th><th>.External</th><th>EP typing progress</th><th>Functions analysed</th><th>Timed out</th><th>Typing progress</th><th>Typing time</th><th>Status</th></tr></thead><tbody>')
for (i in seq_len(nrow(summary_df))) {
  r <- summary_df[i, ]
  ep <- r$ep_call + r$ep_c + r$ep_fortran + r$ep_external
  n_to <- if ("n_timeout" %in% names(r)) r$n_timeout else 0L
  badge <- if (r$crashed) '<span class="badge badge-crash">crashed</span>' else '<span class="badge badge-ok">OK</span>'
  h(sprintf('<tr><td class="pkg">%s</td><td class="num">%d</td><td class="num">%d</td><td class="num">%d</td><td class="num">%d</td><td class="num">%d</td><td>%s</td><td class="num">%d</td><td class="num">%d</td><td>%s</td><td class="num">%s</td><td>%s</td></tr>',
    esc(r$package), ep, r$ep_call, r$ep_c, r$ep_fortran, r$ep_external,
    pct_bar(r$n_ep_typed, ep), r$n_functions, n_to, pct_bar(r$n_typed, r$n_functions),
    fmt_time(r$elapsed_sec), badge))
}
h('</tbody></table>')

# Error categories
if (nrow(error_cats) > 0) {
  h('<h2>Error and timeout categories</h2>')
  h('<table><thead><tr><th>Error</th><th style="text-align:right">Count</th></tr></thead><tbody>')
  for (i in seq_len(nrow(error_cats))) {
    h(sprintf('<tr class="error-row"><td>%s</td><td class="num">%d</td></tr>',
      esc(error_cats$category[i]), error_cats$count[i]))
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

h('<footer>Generated by <strong>r-typing</strong> pipeline</footer>')
h('</body></html>')

writeLines(html, output_file)
cat(sprintf("Wrote %s\n", output_file))
