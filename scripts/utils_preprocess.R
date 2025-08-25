#!/usr/bin/env Rscript

# Shared utilities for PDF preprocessing scripts

# Configure R library path and ensure required packages are installed
setup_r_environment <- function() {
  lib_dir <- Sys.getenv("R_LIBS_USER", unset = "/workspace/.Rlibs")
  dir.create(lib_dir, recursive = TRUE, showWarnings = FALSE)
  .libPaths(c(lib_dir, .libPaths()))
  Sys.setenv(R_LIBS_USER = lib_dir)

  required_packages <- c(
    "jsonlite",
    "pdftools",
    "digest",
    "tesseract",
    "cld2",
    "stringr"
  )
  for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      install.packages(pkg, repos = "https://cloud.r-project.org", lib = lib_dir)
      library(pkg, character.only = TRUE)
    }
  }
}

# Simple key-value argument parser supporting --key value and --flag
parse_args <- function(argsv) {
  kv <- list()
  i <- 1
  while (i <= length(argsv)) {
    key <- argsv[i]
    if (startsWith(key, "--")) {
      key <- substring(key, 3)
      if ((i + 1) <= length(argsv) && !startsWith(argsv[i + 1], "--")) {
        kv[[key]] <- argsv[i + 1]
        i <- i + 2
      } else {
        kv[[key]] <- TRUE
        i <- i + 1
      }
    } else {
      i <- i + 1
    }
  }
  kv
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

# Ensure interim directories exist and return paths
setup_directories <- function(year) {
  dir_interim <- file.path("data", "interim", sprintf("year=%d", as.integer(year)))
  dir_pages <- file.path(dir_interim, "pages")
  dir.create(dir_pages, recursive = TRUE, showWarnings = FALSE)
  list(dir_interim = dir_interim, dir_pages = dir_pages)
}

# Compute file checksum (sha256)
file_checksum <- function(file_path) {
  digest::digest(file_path, algo = "sha256", file = TRUE)
}


