#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

# Ensure user library path is available for package loading
lib_dir <- Sys.getenv("R_LIBS_USER", unset = "~/.Rlibs")
dir.create(lib_dir, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib_dir, .libPaths()))

suppressPackageStartupMessages({
  library(optparse)
  library(dotenv)
  library(processx)
})

option_list <- list(
  make_option(c("-i", "--input"), type = "character", help = "Input PDF file path", metavar = "FILE"),
  make_option(c("-o", "--output_dir"), type = "character", default = "data/marker_output", help = "Output directory for markdown/json [default: %default]"),
  make_option(c("--use_llm"), action = "store_true", default = TRUE, help = "Use LLM to improve results [default: TRUE]"),
  make_option(c("--force_ocr"), action = "store_true", default = TRUE, help = "Force OCR if needed [default: TRUE]"),
  make_option(c("--device"), type = "character", default = NA, help = "TORCH_DEVICE override (e.g., cpu, cuda:0)"),
  make_option(c("--model"), type = "character", default = NA, help = "Marker layout model override (optional)"),
  make_option(c("--env"), type = "character", default = ".env", help = "Path to .env file [default: %default]")
)

parser <- OptionParser(option_list = option_list)
opts <- parse_args(parser)

if (is.null(opts$input)) {
  stop("Usage: Rscript scripts/run_marker_convert.R --input path/to.pdf [--output_dir out] [--use_llm] [--force_ocr]")
}

if (!file.exists(opts$input)) stop(sprintf("Input not found: %s", opts$input))

# Load .env and ensure GEMINI_API_KEY is present for Gemini
if (file.exists(opts$env)) load_dot_env(file = opts$env)

google_key <- Sys.getenv("GEMINI_API_KEY", unset = "")
if (isTRUE(opts$use_llm) && nzchar(google_key) == FALSE) {
  stop("GEMINI_API_KEY is required in .env when --use_llm is set.")
}

dir.create(opts$output_dir, recursive = TRUE, showWarnings = FALSE)

# Build marker CLI command (marker_single per docs)
marker_cmd <- c("marker_single", shQuote(opts$input),
  "--output_dir", shQuote(opts$output_dir)
)

if (isTRUE(opts$use_llm)) {
  marker_cmd <- c(marker_cmd, "--use_llm", "--llm", "gemini")
}
if (isTRUE(opts$force_ocr)) {
  marker_cmd <- c(marker_cmd, "--force_ocr")
}
if (!is.na(opts$device) && nzchar(opts$device)) {
  # TORCH_DEVICE needs to be env var
  Sys.setenv(TORCH_DEVICE = opts$device)
}
if (!is.na(opts$model) && nzchar(opts$model)) {
  marker_cmd <- c(marker_cmd, "--model", shQuote(opts$model))
}

# Environment variables for marker
envs <- c(
  GEMINI_API_KEY = google_key
)

cat("Running:", paste(marker_cmd, collapse = " "), "\n")
res <- run(
  command = marker_cmd[1],
  args = marker_cmd[-1],
  echo = TRUE,
  windows_verbatim_args = TRUE,
  env = envs,
  error_on_status = FALSE
)

status <- res$status
if (!is.null(status) && status != 0) {
  stop(sprintf("marker-pdf failed with status %s", status))
}

cat("Conversion complete. Output directory:\n", normalizePath(opts$output_dir, winslash = "/", mustWork = FALSE), "\n", sep = "")


