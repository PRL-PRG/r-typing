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
load_checker_meta <- function(dir) {
  list(
    version = read_one_line(file.path(dir, "checker_version.txt")),
    subject = read_one_line(file.path(dir, "checker_subject.txt")),
    status  = read_all_lines(file.path(dir, "checker_status.txt"))
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
  "prism-c.min.js"
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
    c("prism-core.min.js", "prism-clike.min.js", "prism-c.min.js")
  ), collapse = "")
}

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

# --- Status-change classification (vs baseline) ---
# Rank statuses so we can colour a transition as an improvement or regression.
status_rank <- function(s) {
  switch(s, "typed" = 2L, "timeout" = 1L, "untypeable" = 1L, 0L)
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
checker_version_html <- function(meta = checker_meta) {
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
h(sprintf('<p class="subtitle">Type-checking results for %d CRAN packages &mdash; generated %s%s</p>',
  n_packages, format(Sys.time(), "%Y-%m-%d %H:%M"),
  checker_version_html()))
h(checker_status_html())

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

# Per-function inference timing (only when --log-times produced data
# for at least one package).
has_timing_col <- "mean_typing_sec" %in% names(summary_df)
if (has_timing_col && any(!is.na(summary_df$mean_typing_sec))) {
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

# Pipeline phase breakdown (from `Phase:` lines emitted under --log-times).
# `overhead = phase_fundef_pass_sec - total_typing_sec` captures the
# PAst.transform + GC-between-calls cost inside the Fundef loop that's not
# already attributed to any per-function timing. `recording_save` only fires
# when --mlsem-recording is set, so the column is suppressed unless at least
# one package has data.
phase_cols <- c("phase_load_ty_sec", "phase_parsing_sec",
                "phase_call_graph_sec", "phase_non_fundef_sec",
                "phase_fundef_pass_sec")
if (all(phase_cols %in% names(summary_df)) &&
    any(!is.na(summary_df$phase_fundef_pass_sec))) {
  phase_rows <- summary_df[!is.na(summary_df$phase_fundef_pass_sec), , drop = FALSE]
  phase_rows <- phase_rows[order(-phase_rows$elapsed_sec), , drop = FALSE]
  has_rec <- "phase_recording_save_sec" %in% names(phase_rows) &&
             any(!is.na(phase_rows$phase_recording_save_sec))

  h('<h2>Pipeline phase breakdown <small>from <code>Phase:</code> lines under <code>--log-times</code></small></h2>')
  h('<p class="phase-legend">',
    '<strong>&Sigma; infer_ast</strong> is the sum of every function\'s <code>infer_ast</code> body-inference timing (the same total as in the table above).',
    ' <strong>overhead</strong> = <code>fundef_pass &minus; &Sigma; infer_ast</code>: time spent inside the Fundef loop that <em>isn\'t</em> attributed to any function body &mdash; <code>PAst.transform</code> per function, dispatch (StrMap lookups, idenv union), and GC between calls.',
    '</p>')
  h('<table><thead><tr>',
    '<th>Package</th>',
    '<th style="text-align:right">Wall</th>',
    '<th style="text-align:right">load_ty</th>',
    '<th style="text-align:right">parsing</th>',
    '<th style="text-align:right">call_graph</th>',
    '<th style="text-align:right">non_fundef</th>',
    '<th style="text-align:right">fundef_pass</th>',
    '<th style="text-align:right" title="sum of every function\'s infer_ast timing">&Sigma; infer_ast</th>',
    if (has_rec) '<th style="text-align:right">recording_save</th>' else '',
    '<th style="text-align:right" title="fundef_pass minus the sum of per-function infer_ast timings">overhead<br><small>fundef_pass &minus; &Sigma; infer_ast</small></th>',
    '</tr></thead><tbody>')
  for (i in seq_len(nrow(phase_rows))) {
    r <- phase_rows[i, ]
    overhead <- if (!is.na(r$total_typing_sec))
      r$phase_fundef_pass_sec - r$total_typing_sec else NA_real_
    pkg_link <- sprintf('<a href="pkg/%s.html">%s</a>', esc(r$package), esc(r$package))
    rec_cell <- if (has_rec)
      sprintf('<td class="num">%s</td>', fmt_func_timing(r$phase_recording_save_sec))
    else ""
    h(sprintf(
      '<tr><td class="pkg">%s</td><td class="num">%s</td><td class="num">%s</td><td class="num">%s</td><td class="num">%s</td><td class="num">%s</td><td class="num">%s</td><td class="num">%s</td>%s<td class="num">%s</td></tr>',
      pkg_link,
      fmt_func_timing(r$elapsed_sec),
      fmt_func_timing(r$phase_load_ty_sec),
      fmt_func_timing(r$phase_parsing_sec),
      fmt_func_timing(r$phase_call_graph_sec),
      fmt_func_timing(r$phase_non_fundef_sec),
      fmt_func_timing(r$phase_fundef_pass_sec),
      fmt_func_timing(r$total_typing_sec),
      rec_cell,
      fmt_func_timing(overhead)))
  }
  h('</tbody></table>')
}

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

# Functions typed as `empty` (bottom). Kept separate from the error/timeout
# table above because these functions DID type — an `empty` result is a quality
# red flag, not a failure, and can be legitimate (bail-out helpers that never
# return, e.g. `bail_if`-style functions). Placed next to the error section
# because it belongs to the same family of red flags.
empty_counts_by_pkg <- function(fdf) {
  if (nrow(fdf) == 0L) return(setNames(integer(0), character(0)))
  ty <- fdf[fdf$status == "typed", , drop = FALSE]
  if (nrow(ty) == 0L) return(setNames(integer(0), character(0)))
  flag <- vapply(ty$type_sig, returns_only_empty, logical(1), USE.NAMES = FALSE)
  ty <- ty[flag, , drop = FALSE]
  if (nrow(ty) == 0L) return(setNames(integer(0), character(0)))
  tbl <- table(ty$package)
  setNames(as.integer(tbl), names(tbl))
}
emp_now  <- empty_counts_by_pkg(functions_df)
emp_base <- if (!is.null(b_functions_df)) empty_counts_by_pkg(b_functions_df) else integer(0)
total_empty      <- sum(emp_now)
total_empty_base <- sum(emp_base)

if (total_empty > 0L || length(emp_base) > 0L) {
  pct_empty <- if (total_typed > 0L) round(100 * total_empty / total_typed, 1) else 0
  h('<h2>Functions typed as <code>empty</code> <small>typed, but the inferred result is <code>empty</code> (bottom)</small></h2>')
  h('<p class="phase-legend">An <code>empty</code> return means inference found no usable value &mdash; usually a red flag,',
    ' though it can be legitimate for bail-out helpers (e.g. <code>bail_if</code>-style functions that never return).',
    ' Drill into a package and use the <em>empty return only</em> filter to see which functions.</p>')

  cur_for  <- function(p) { v <- emp_now[p];  if (is.na(v)) 0L else as.integer(v) }
  base_for <- function(p) { v <- emp_base[p]; if (is.na(v)) 0L else as.integer(v) }
  pkgs <- union(names(emp_now), names(emp_base))
  pkgs <- pkgs[order(-vapply(pkgs, cur_for, integer(1)), pkgs)]
  typed_by_pkg <- setNames(summary_df$n_typed, summary_df$package)

  base_col <- if (!is.null(baseline_dir)) '<th style="text-align:right">Before</th><th style="text-align:right">&Delta;</th>' else ''
  h('<table><thead><tr><th>Package</th><th style="text-align:right">Empty returns</th><th style="text-align:right">% of pkg typed</th>',
    base_col, '</tr></thead>')
  # Total row first (in <tfoot> but rendered at the top via CSS order would be
  # overkill); keep it as a tinted header-like total line so the global figure
  # and its change are visible at a glance.
  total_pct <- if (total_typed > 0L) sprintf("%.1f%%", 100 * total_empty / total_typed) else "&mdash;"
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

# Baseline per-function status, keyed by "<package>\x1f<function_name>", for
# detecting status changes on per-package pages. NULL when no baseline.
SEP <- "\x1f"
b_status_by_key <- if (!is.null(b_functions_df)) {
  setNames(b_functions_df$status,
           paste(b_functions_df$package, b_functions_df$function_name, sep = SEP))
} else NULL

status_badge <- function(status) {
  switch(status,
    "typed"      = '<span class="badge badge-typed">typed</span>',
    "untypeable" = '<span class="badge badge-untypeable">untypeable</span>',
    "timeout"    = '<span class="badge badge-timeout">timeout</span>',
    sprintf('<span class="badge">%s</span>', esc(status))
  )
}

render_function_block <- function(pkg, name, status, type_sig, error_title, error_detail,
                                  timing_sec = NA_real_,
                                  is_entry = FALSE, entry_conv = character(),
                                  with_source = TRUE,
                                  change = list(kind = "unchanged", html = "")) {
  hh <- character()
  entry_attr <- if (is_entry) "true" else "false"
  conv_attr <- if (length(entry_conv) > 0L) paste(entry_conv, collapse = ",") else ""
  conv_badges <- if (length(entry_conv) > 0L) {
    paste(sprintf(' <span class="badge badge-conv">%s</span>', esc(entry_conv)), collapse = "")
  } else ""
  changed_attr <- if (!identical(change$kind, "unchanged")) "true" else "false"
  empty_ret <- isTRUE(status == "typed") && returns_only_empty(type_sig)
  empty_attr <- if (empty_ret) "true" else "false"
  empty_badge <- if (empty_ret)
    ' <span class="badge badge-empty" title="result type is empty (bottom) &mdash; inference found no usable return; often a red flag">empty return</span>' else ''
  hh <- c(hh, sprintf('<div class="func-list" data-status="%s" data-entry="%s" data-conv="%s" data-name="%s" data-changed="%s" data-empty-ret="%s"><h3>%s %s%s%s%s%s</h3>',
    esc(status), entry_attr, esc(conv_attr), esc(tolower(name)), changed_attr, empty_attr, esc(name),
    status_badge(status),
    if (is_entry) ' <span class="badge badge-entry">entry</span>' else '',
    conv_badges,
    change$html,
    empty_badge))
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
  if (!is.na(timing_sec)) {
    hh <- c(hh, sprintf('<div class="timing">timing: %s</div>', fmt_func_timing(timing_sec)))
  }
  if (with_source) {
    snip <- source_snippet(pkg, name)
    if (is.null(snip)) {
      hh <- c(hh, '<div class="source-meta"><em>(definition not located in src/)</em></div>')
    } else {
      hh <- c(hh, sprintf('<details><summary>source</summary><pre class="source language-%s"><code class="language-%s">%s</code></pre><div class="source-meta">%s:%d</div></details>',
        snip$lang, snip$lang, esc(snip$snippet), esc(snip$file), snip$line))
    }
  }
  hh <- c(hh, '</div>')
  paste(hh, collapse = "")
}

write_package_page <- function(pkg) {
  rows <- functions_df[functions_df$package == pkg, , drop = FALSE]
  ep_tbl <- parse_ep_table(file.path(raw_dir, paste0(pkg, ".out")))
  ep_names <- unique(ep_tbl$name)
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
    prism_head("../"),
    '</head><body>',
    '<a href="../index.html" class="back-link">&larr; all packages</a>',
    sprintf('<h1><span class="r">%s</span></h1>', esc(pkg)),
    sprintf('<p class="subtitle">%d functions analysed &middot; %d typed &middot; %d untypeable &middot; %d timed out &middot; %d entry points (%d typed)%s</p>',
      s$n_functions, s$n_typed, s$n_untypeable, if ("n_timeout" %in% names(s)) s$n_timeout else 0L,
      ep_total, s$n_ep_typed,
      checker_version_html()),
    checker_status_html())

  # Unified function list with filter toolbar.
  ep_set <- ep_names
  ep_in_csv <- ep_set[ep_set %in% rows$function_name]
  ep_missing <- setdiff(ep_set, ep_in_csv)

  have_baseline <- !is.null(b_status_by_key)
  base_status_for <- function(nm) {
    if (!have_baseline) return(NA_character_)
    v <- b_status_by_key[[paste(pkg, nm, sep = SEP)]]
    if (is.null(v)) NA_character_ else v
  }
  # Functions present in the baseline for this package but absent now. Exclude
  # entry points already rendered as 'unknown' (ep_missing) so cards don't
  # double up and total_cards stays accurate.
  gone_names <- if (have_baseline) {
    b_here <- b_functions_df$function_name[b_functions_df$package == pkg]
    sort(setdiff(setdiff(b_here, rows$function_name), ep_missing))
  } else character()

  total_cards <- nrow(rows) + length(ep_missing) + length(gone_names)

  ph <- c(ph, sprintf('<h2>Functions <small>%d</small></h2>', total_cards))
  changed_filter <- if (have_baseline)
    '<label><input type="checkbox" id="flt-changed"> changed only</label>' else ''
  ph <- c(ph,
    '<div class="filters">',
    '<label><input type="checkbox" id="flt-ep"> Entry points only</label>',
    '<label><input type="checkbox" id="flt-typed" checked> typed</label>',
    '<label><input type="checkbox" id="flt-untypeable" checked> untypeable</label>',
    '<label><input type="checkbox" id="flt-timeout" checked> timeout</label>',
    changed_filter,
    '<label><input type="checkbox" id="flt-empty"> empty return only</label>',
    '<input type="search" id="flt-name" placeholder="name…">',
    sprintf('<span class="count"><span id="flt-count">%d</span> / %d</span>', total_cards, total_cards),
    '</div>')

  if (total_cards == 0) {
    ph <- c(ph, '<p class="na">No functions recorded.</p>')
  } else {
    if (nrow(rows) > 0) {
      ord <- order(match(rows$status, c("typed", "untypeable", "timeout")), rows$function_name)
      rows <- rows[ord, , drop = FALSE]
      has_timing <- "timing_sec" %in% names(rows)
      for (i in seq_len(nrow(rows))) {
        r <- rows[i, ]
        is_ep <- r$function_name %in% ep_set
        change <- if (have_baseline)
          change_info(r$status, base_status_for(r$function_name))
        else list(kind = "unchanged", html = "")
        ph <- c(ph, render_function_block(pkg, r$function_name, r$status,
          r$type_sig, r$error_title, r$error_detail,
          timing_sec = if (has_timing) r$timing_sec else NA_real_,
          is_entry = is_ep,
          entry_conv = if (is_ep) ep_conv_for(ep_tbl, r$function_name) else character(),
          with_source = TRUE,
          change = change))
      }
    }
    # Functions that existed in the baseline but are gone now (status change:
    # disappeared). Rendered as lightweight cards so the "changed only" filter
    # surfaces regressions in coverage.
    for (nm in gone_names) {
      was <- base_status_for(nm)
      ph <- c(ph, sprintf(
        '<div class="func-list" data-status="gone" data-entry="%s" data-conv="" data-name="%s" data-changed="true" data-empty-ret="false"><h3>%s <span class="badge badge-change-bad">gone</span></h3><div class="error-line">was <strong>%s</strong> in the baseline; no longer typed/reached in this run</div></div>',
        if (nm %in% ep_set) "true" else "false", esc(tolower(nm)), esc(nm), esc(was)))
    }
    # Entry points missing from functions.csv: render as 'unknown' cards.
    for (nm in ep_missing) {
      convs <- ep_conv_for(ep_tbl, nm)
      conv_attr <- paste(convs, collapse = ",")
      conv_badges <- paste(sprintf(' <span class="badge badge-conv">%s</span>', esc(convs)), collapse = "")
      ph <- c(ph, sprintf(
        '<div class="func-list" data-status="unknown" data-entry="true" data-conv="%s" data-name="%s" data-changed="false" data-empty-ret="false"><h3>%s <span class="badge">unknown</span> <span class="badge badge-entry">entry</span>%s</h3><div class="error-line">no record in functions.csv (likely the entry point itself was filtered)</div></div>',
        esc(conv_attr), esc(tolower(nm)), esc(nm), conv_badges))
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
    '  const changedOnly = document.getElementById("flt-changed"); // null when no baseline',
    '  const emptyOnly = document.getElementById("flt-empty");',
    '  if (emptyOnly && location.hash === "#empty") emptyOnly.checked = true;',
    '  const nameInput = document.getElementById("flt-name");',
    '  const cards = document.querySelectorAll(".func-list[data-status]");',
    '  const counter = document.getElementById("flt-count");',
    '  function apply() {',
    '    const epOnly = ep.checked;',
    '    const wantChanged = changedOnly && changedOnly.checked;',
    '    const wantEmpty = emptyOnly && emptyOnly.checked;',
    '    const q = (nameInput.value || "").toLowerCase();',
    '    let visible = 0;',
    '    cards.forEach(c => {',
    '      const s = c.dataset.status;',
    '      const e = c.dataset.entry === "true";',
    '      const n = c.dataset.name;',
    '      const okEp = !epOnly || e;',
    '      const okStat = s === "unknown" || s === "gone" || (stat[s] && stat[s].checked);',
    '      const okChanged = !wantChanged || c.dataset.changed === "true";',
    '      const okEmpty = !wantEmpty || c.dataset.emptyRet === "true";',
    '      const okName = !q || n.indexOf(q) !== -1;',
    '      const show = okEp && okStat && okChanged && okEmpty && okName;',
    '      c.style.display = show ? "" : "none";',
    '      if (show) visible++;',
    '    });',
    '    counter.textContent = visible;',
    '  }',
    '  [ep, stat.typed, stat.untypeable, stat.timeout, changedOnly, emptyOnly]',
    '    .filter(Boolean).forEach(el => el.addEventListener("change", apply));',
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
