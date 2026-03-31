#!/usr/bin/env Rscript
# Download source tarballs for packages listed in packages.txt

args <- commandArgs(trailingOnly = TRUE)
packages_file <- if (length(args) >= 1) args[1] else "packages.txt"
dest_dir <- if (length(args) >= 2) args[2] else "work/tarballs"

if (!file.exists(packages_file)) {
  stop("Package list not found: ", packages_file)
}

dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

packages <- readLines(packages_file)
packages <- trimws(packages)
packages <- packages[nchar(packages) > 0 & !startsWith(packages, "#")]

cat(sprintf("Downloading %d packages to %s\n", length(packages), dest_dir))

status <- data.frame(
  package = character(),
  status = character(),
  file = character(),
  stringsAsFactors = FALSE
)

for (pkg in packages) {
  cat(sprintf("  %s ... ", pkg))
  tryCatch({
    result <- download.packages(
      pkg,
      destdir = dest_dir,
      repos = "https://cloud.r-project.org",
      type = "source",
      quiet = TRUE
    )
    if (nrow(result) > 0) {
      cat("OK\n")
      status <- rbind(status, data.frame(
        package = pkg,
        status = "ok",
        file = basename(result[1, 2]),
        stringsAsFactors = FALSE
      ))
    } else {
      cat("FAILED (no result)\n")
      status <- rbind(status, data.frame(
        package = pkg,
        status = "failed",
        file = NA,
        stringsAsFactors = FALSE
      ))
    }
  }, error = function(e) {
    cat(sprintf("FAILED (%s)\n", conditionMessage(e)))
    status <<- rbind(status, data.frame(
      package = pkg,
      status = "failed",
      file = NA,
      stringsAsFactors = FALSE
    ))
  })
}

status_file <- file.path(dest_dir, "download_status.csv")
write.csv(status, status_file, row.names = FALSE)
cat(sprintf("\nDownload status written to %s\n", status_file))
cat(sprintf("  OK: %d, Failed: %d\n", sum(status$status == "ok"), sum(status$status == "failed")))
