#!/usr/bin/env Rscript
# Download source tarballs for packages listed in packages.txt
#
# A line of packages.txt is one of:
#   pkg                           a CRAN package
#   github:owner/repo[@ref]       a package on GitHub, at `ref` (a branch, tag
#                                 or commit; by default the default branch)
#   r:pkg                         a base package (e.g. compiler), from the
#                                 sources of the running R's version
# Every tarball ends up as <pkg>_<version>.tar.gz, with the package under
# <pkg>/, as CRAN's are: the extraction and installation take them all alike.
# download_status.csv records where each one came from (its `source`).

args <- commandArgs(trailingOnly = TRUE)
packages_file <- if (length(args) >= 1) args[1] else "packages.txt"
dest_dir <- if (length(args) >= 2) args[2] else "work/tarballs"

if (!file.exists(packages_file)) {
  stop("Package list not found: ", packages_file)
}

dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
dest_dir <- normalizePath(dest_dir)

packages <- readLines(packages_file)
packages <- trimws(sub("#.*$", "", packages))
packages <- packages[nchar(packages) > 0]

cat(sprintf("Downloading %d packages to %s\n", length(packages), dest_dir))

# Packs the package in `dir` as <pkg>_<version>.tar.gz in dest_dir, the
# package under <pkg>/. Returns the package's name and the tarball's name.
repack <- function(dir) {
  desc <- read.dcf(file.path(dir, "DESCRIPTION"), fields = c("Package", "Version"))
  pkg <- desc[1, "Package"]
  version <- desc[1, "Version"]
  stage <- tempfile("repack")
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE))
  file.copy(dir, stage, recursive = TRUE)
  file.rename(file.path(stage, basename(dir)), file.path(stage, pkg))
  file <- sprintf("%s_%s.tar.gz", pkg, version)
  old <- setwd(stage)
  on.exit(setwd(old), add = TRUE, after = FALSE)
  utils::tar(file.path(dest_dir, file), pkg, compression = "gzip", tar = "internal")
  list(package = unname(pkg), file = file)
}

# The one directory a tarball extracts to.
untar_one <- function(tarball, exdir, files = NULL) {
  untar(tarball, files = files, exdir = exdir)
  top <- list.dirs(exdir, recursive = FALSE)
  if (length(top) != 1) stop("expected one directory in ", basename(tarball))
  top
}

from_cran <- function(pkg) {
  result <- download.packages(pkg, destdir = dest_dir,
                              repos = "https://cloud.r-project.org",
                              type = "source", quiet = TRUE)
  if (nrow(result) == 0) stop("no result")
  list(package = pkg, file = basename(result[1, 2]), source = "cran")
}

# github:owner/repo[@ref]. GitHub's tarball of a commit has the repository
# under owner-repo-<short sha>/: the sha is recorded.
from_github <- function(spec) {
  m <- regmatches(spec, regexec("^github:([^/@]+)/([^/@]+)(?:@(.+))?$", spec, perl = TRUE))[[1]]
  if (length(m) == 0) stop("expected github:owner/repo[@ref]")
  ref <- if (nzchar(m[4])) m[4] else "HEAD"
  tmp <- tempfile("github")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  tarball <- file.path(tmp, "src.tar.gz")
  # With GITHUB_TOKEN (as in CI), authenticated: GitHub limits the
  # anonymous requests of an address, which CI runners share.
  token <- Sys.getenv("GITHUB_TOKEN")
  headers <- if (nzchar(token)) c(Authorization = paste("Bearer", token))
  download.file(sprintf("https://api.github.com/repos/%s/%s/tarball/%s", m[2], m[3], ref),
                tarball, quiet = TRUE, mode = "wb", headers = headers)
  top <- untar_one(tarball, file.path(tmp, "x"))
  sha <- sub(".*-", "", basename(top))
  r <- repack(top)
  c(r, source = sprintf("github:%s/%s@%s", m[2], m[3], sha))
}

# r:pkg, from the sources of this R's version: its src/library/<pkg>, whose
# DESCRIPTION.in has the version to fill in.
r_sources <- NULL
from_r <- function(spec) {
  pkg <- sub("^r:", "", spec)
  version <- paste(R.version$major, R.version$minor, sep = ".")
  tmp <- tempfile("rsrc")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  if (is.null(r_sources)) {
    r_sources <<- tempfile("R-", fileext = ".tar.gz")
    download.file(sprintf("https://cloud.r-project.org/src/base/R-%s/R-%s.tar.gz",
                          R.version$major, version),
                  r_sources, quiet = TRUE, mode = "wb")
  }
  top <- untar_one(r_sources, file.path(tmp, "x"),
                   files = sprintf("R-%s/src/library/%s", version, pkg))
  dir <- file.path(top, "src", "library", pkg)
  if (!dir.exists(dir)) stop("no package ", pkg, " in R ", version)
  if (!file.exists(file.path(dir, "DESCRIPTION"))) {
    desc <- readLines(file.path(dir, "DESCRIPTION.in"))
    writeLines(gsub("@VERSION@", version, desc, fixed = TRUE), file.path(dir, "DESCRIPTION"))
  }
  r <- repack(dir)
  c(r, source = sprintf("r:%s", version))
}

status <- data.frame(
  package = character(),
  status = character(),
  file = character(),
  source = character(),
  stringsAsFactors = FALSE
)

for (spec in packages) {
  cat(sprintf("  %s ... ", spec))
  tryCatch({
    r <- if (startsWith(spec, "github:")) from_github(spec)
         else if (startsWith(spec, "r:")) from_r(spec)
         else from_cran(spec)
    cat(sprintf("OK (%s)\n", r$file))
    status <- rbind(status, data.frame(
      package = r$package,
      status = "ok",
      file = r$file,
      source = r$source,
      stringsAsFactors = FALSE
    ))
  }, error = function(e) {
    cat(sprintf("FAILED (%s)\n", conditionMessage(e)))
    status <<- rbind(status, data.frame(
      package = spec,
      status = "failed",
      file = NA,
      source = NA,
      stringsAsFactors = FALSE
    ))
  })
}
if (!is.null(r_sources)) unlink(r_sources)

status_file <- file.path(dest_dir, "download_status.csv")
write.csv(status, status_file, row.names = FALSE)
cat(sprintf("\nDownload status written to %s\n", status_file))
cat(sprintf("  OK: %d, Failed: %d\n", sum(status$status == "ok"), sum(status$status == "failed")))
