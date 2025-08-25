#!/usr/bin/env Rscript

required <- c(
  "jsonlite",
  "pdftools",
  "digest",
  "tesseract",
  "cld2",
  "stringr"
)

is_installed <- function(pkg) {
  is.element(pkg, installed.packages()[, 1])
}

to_install <- required[!vapply(required, is_installed, logical(1))]

if (length(to_install) > 0) {
  lib_dir <- Sys.getenv("R_LIBS_USER", unset = "/workspace/.Rlibs")
  dir.create(lib_dir, recursive = TRUE, showWarnings = FALSE)
  .libPaths(c(lib_dir, .libPaths()))
  Sys.setenv(R_LIBS_USER = lib_dir)
  cat("Installing R packages to ", lib_dir, ": ", paste(to_install, collapse = ", "), "\n", sep = "")
  install.packages(to_install, repos = "https://cloud.r-project.org", lib = lib_dir)
} else {
  cat("All required R packages already installed.\n")
}

cat("R dependency check complete.\n")

