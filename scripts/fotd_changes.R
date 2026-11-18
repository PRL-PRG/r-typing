# The changes of the functions of the day (.github/workflows/
# function-of-the-day.yml) between the baseline run and this one, for CI to
# post on their issues.
#
#   Rscript scripts/fotd_changes.R <baseline dir> <results dir> <featured.json> <out.json>
#
# featured.json: [{"issue": 12, "package": "vctrs", "name": "vec_slice"}, ...],
# the functions the issues are about. out.json: {"broken": false, "reason":
# null, "changes": [{"issue": 12, "body": "<markdown>"}, ...]}: one comment
# per function whose status, type, error or timeout changed in a mode.
#
# A broken run (most functions changing status at once, or missing, as when
# every package crashed) would comment on every issue twice: it is reported
# with broken = true and no changes instead.
#
# DASHBOARD_URL (the deployed dashboard) and GITHUB_* (the run) make links.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 4L) stop("usage: fotd_changes.R <baseline dir> <results dir> <featured.json> <out.json>")
prev_dir <- args[1]; cur_dir <- args[2]; featured_path <- args[3]; out_path <- args[4]

# The share of the functions present in both runs that may change status, and
# the share of the baseline's that may be missing, before the run is taken
# for broken.
MAX_CHANGED <- 0.25
MAX_MISSING <- 0.5

write_out <- function(changes = list(), broken = FALSE, reason = NULL) {
  jsonlite::write_json(list(broken = broken, reason = reason, changes = changes),
                       out_path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  invisible()
}

read_functions <- function(dir) {
  f <- file.path(dir, "functions.csv")
  if (!file.exists(f)) return(NULL)
  df <- read.csv(f, stringsAsFactors = FALSE)
  df <- df[df$side == "r", , drop = FALSE]
  if (is.null(df$error_line)) df$error_line <- NA
  df$id <- paste(df$package, df$function_name, df$mode, sep = "\x1f")
  # Repeated names (anonymous statements, redefinitions) identify nothing.
  df[!(df$id %in% df$id[duplicated(df$id)]), , drop = FALSE]
}

featured <- jsonlite::fromJSON(featured_path, simplifyVector = FALSE)
prev <- read_functions(prev_dir)
cur <- read_functions(cur_dir)
if (is.null(prev)) { message("No baseline: nothing to compare."); write_out(); quit(status = 0) }
if (is.null(cur)) stop("no functions.csv in ", cur_dir)

# --- Is this run broken? ---
prev_s <- prev[prev$mode == "strict", ]
cur_s <- cur[cur$mode == "strict", ]
missing <- mean(!(prev_s$id %in% cur_s$id))
both <- merge(prev_s[, c("id", "status")], cur_s[, c("id", "status")], by = "id")
changed <- if (nrow(both)) mean(both$status.x != both$status.y) else 0
if (missing > MAX_MISSING || changed > MAX_CHANGED) {
  reason <- sprintf("%.0f%% of the baseline's R functions are missing and %.0f%% of the others changed status: not commenting",
                    100 * missing, 100 * changed)
  message(reason)
  write_out(broken = TRUE, reason = reason)
  quit(status = 0)
}

# --- The changes ---
dashboard <- sub("/*$", "/", Sys.getenv("DASHBOARD_URL"))
e <- Sys.getenv(c("GITHUB_SERVER_URL", "GITHUB_REPOSITORY", "GITHUB_RUN_ID", "GITHUB_RUN_NUMBER"))
run_link <- if (all(nzchar(e[1:3])))
  sprintf("[run #%s](%s/%s/actions/runs/%s)", e[4], e[1], e[2], e[3]) else "this run"

row_of <- function(df, pkg, name, mode) {
  r <- df[df$id == paste(pkg, name, mode, sep = "\x1f"), , drop = FALSE]
  if (nrow(r)) r[1, ] else NULL
}
na_str <- function(x) if (is.null(x) || length(x) == 0L || is.na(x)) "" else as.character(x)
fence <- function(x, lang = "") sprintf("```%s\n%s\n```", lang, x)
secs <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x)) "?" else if (x < 0.01) "<0.01 s" else sprintf("%.2f s", x)
}
# What a row says: its status and, by status, its type or its error.
describe <- function(r) {
  status <- r$status
  if (status %in% c("typed", "fallback")) return(fence(na_str(r$type_sig)))
  if (status == "timeout") return(sprintf("timed out after %s", secs(r$timing_sec)))
  title <- na_str(r$error_title)
  line <- na_str(r$error_line)
  head <- paste0(if (nzchar(title)) title else status, if (nzchar(line)) sprintf(" (line %s)", line) else "")
  detail <- na_str(r$error_detail)
  paste(c(head, if (nzchar(detail)) fence(detail)), collapse = "\n")
}
same <- function(a, b) {
  identical(a$status, b$status) && identical(na_str(a$type_sig), na_str(b$type_sig)) &&
    (a$status %in% c("typed", "fallback", "timeout") ||
     (identical(na_str(a$error_title), na_str(b$error_title)) &&
      identical(na_str(a$error_detail), na_str(b$error_detail)) &&
      identical(na_str(a$error_line), na_str(b$error_line))))
}

changes <- list()
for (f in featured) {
  pkg <- f$package; name <- f$name
  parts <- character()
  for (mode in c("strict", "gradual")) {
    was <- row_of(prev, pkg, name, mode)
    if (is.null(was)) next   # not in the baseline (e.g. its package crashed): nothing to compare
    now <- row_of(cur, pkg, name, mode)
    if (is.null(now)) {
      parts <- c(parts, sprintf("**%s**: no longer in the results (renamed, removed, or its package did not finish).", mode))
      next
    }
    if (same(was, now)) next
    parts <- c(parts, sprintf("**%s**: %s → **%s** (%s, was %s)\n\nWas:\n%s\n\nNow:\n%s",
      mode, was$status, now$status, secs(now$timing_sec), secs(was$timing_sec), describe(was), describe(now)))
  }
  if (length(parts) == 0L) next
  card <- if (nzchar(dashboard)) sprintf(" ([dashboard](%spkg/%s.html#r-%s))", dashboard, pkg,
                                         gsub("[^A-Za-z0-9._-]", "_", name)) else ""
  body <- paste0(sprintf("`%s::%s` changed in %s%s.\n\n", pkg, name, run_link, card),
                 paste(parts, collapse = "\n\n"))
  changes[[length(changes) + 1L]] <- list(issue = f$issue, body = body)
}
message(sprintf("%d of %d functions of the day changed.", length(changes), length(featured)))
write_out(changes)
