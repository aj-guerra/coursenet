#!/usr/bin/env Rscript

# Minimal R dependency bootstrap for using marker-pdf via R wrappers

lib_dir <- Sys.getenv("R_LIBS_USER", unset = "~/.Rlibs")
dir.create(lib_dir, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib_dir, .libPaths()))

required_packages <- c(
  "dotenv",     # load .env for GOOGLE_API_KEY, etc.
  "optparse",   # CLI arg parsing
  "processx"    # robust system process execution
)

installed <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!(pkg %in% installed)) {
    install.packages(pkg, repos = "https://cloud.r-project.org", lib = lib_dir)
  }
}

cat("R dependency install complete.\n")


