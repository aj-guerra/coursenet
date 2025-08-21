#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

# Ensure user library path is available for package loading
lib_dir <- Sys.getenv("R_LIBS_USER", unset = "/workspace/.Rlibs")
dir.create(lib_dir, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib_dir, .libPaths()))

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

argv <- parse_args(args)
if (is.null(argv$year) || is.null(argv$pdf)) {
  stop("Usage: Rscript scripts/extract_pdf_metadata.R --year YYYY --pdf path/to/catalog.pdf")
}

year <- as.integer(argv$year)
pdf_path <- argv$pdf

if (!file.exists(pdf_path)) {
  stop(sprintf("PDF not found: %s", pdf_path))
}

dir_interim <- file.path("data", "interim", sprintf("year=%d", year))
dir_pdf_meta <- file.path(dir_interim, "pdf_metadata")
dir.create(dir_pdf_meta, recursive = TRUE, showWarnings = FALSE)

`%||%` <- function(a, b) if (!is.null(a)) a else b

info <- pdftools::pdf_info(pdf_path)

# Capture a small sample of text (first page) for context if available
sample_text <- NULL
try({
  sample_text <- pdftools::pdf_text(pdf_path)
  if (length(sample_text) > 0) {
    sample_text <- substr(sample_text[[1]], 1, 2000)
  } else {
    sample_text <- NULL
  }
}, silent = TRUE)

metadata <- list(
  year = year,
  file = normalizePath(pdf_path, winslash = "/", mustWork = FALSE),
  pages = info$pages,
  pdf_version = info$pdf_version,
  encrypted = isTRUE(info$encrypted),
  linearized = isTRUE(info$linearized),
  created = as.character(info$created),
  modified = as.character(info$modified),
  author = info$keys$Author %||% NULL,
  title = info$keys$Title %||% NULL,
  subject = info$keys$Subject %||% NULL,
  producer = info$keys$Producer %||% NULL,
  creator = info$keys$Creator %||% NULL,
  sample_text = sample_text
)

jsonlite::write_json(metadata, file.path(dir_pdf_meta, "metadata.json"), pretty = TRUE, auto_unbox = TRUE, null = "null")

cat("Extracted PDF metadata to ", file.path(dir_pdf_meta, "metadata.json"), "\n", sep = "")

