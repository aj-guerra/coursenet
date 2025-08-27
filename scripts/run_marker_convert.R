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
  make_option(c("--use_llm"), action = "store_true", default = FALSE, help = "Use LLM to improve results [default: FALSE]"),
  make_option(c("--no_llm"), action = "store_true", default = FALSE, help = "Explicitly disable LLM [default: FALSE]"),
  make_option(c("--force_ocr"), action = "store_true", default = FALSE, help = "Force OCR if needed [default: FALSE]"),
  make_option(c("--no_ocr"), action = "store_true", default = FALSE, help = "Explicitly disable OCR [default: FALSE]"),
  make_option(c("--device"), type = "character", default = NA, help = "TORCH_DEVICE override (e.g., cpu, cuda:0, mps)"),
  make_option(c("--model"), type = "character", default = NA, help = "Marker layout model override (optional)"),
  make_option(c("--layout_batch_size"), type = "integer", default = NA, help = "Layout model batch size [optional]"),
  make_option(c("--detection_batch_size"), type = "integer", default = NA, help = "Detection model batch size [optional]"),
  make_option(c("--recognition_batch_size"), type = "integer", default = NA, help = "Recognition model batch size [optional]"),
  make_option(c("--table_rec_batch_size"), type = "integer", default = NA, help = "Table recognition batch size [optional]"),
  make_option(c("--config_json"), type = "character", default = NA, help = "Path to JSON configuration file [optional]"),
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

# Build marker CLI command (assumes marker_single is in PATH)
marker_cmd <- c("marker_single", shQuote(opts$input),
  "--output_dir", shQuote(opts$output_dir)
)

if (isTRUE(opts$use_llm)) {
  marker_cmd <- c(marker_cmd, "--use_llm", "--llm_service", "marker.services.gemini.GoogleGeminiService", "--gemini_api_key", google_key)
}
if (isTRUE(opts$force_ocr)) {
  marker_cmd <- c(marker_cmd, "--force_ocr")
}

if (!is.na(opts$model) && nzchar(opts$model)) {
  marker_cmd <- c(marker_cmd, "--model", shQuote(opts$model))
}

# Add batch size parameters if specified
if (!is.na(opts$layout_batch_size)) {
  marker_cmd <- c(marker_cmd, "--layout_batch_size", opts$layout_batch_size)
}
if (!is.na(opts$detection_batch_size)) {
  marker_cmd <- c(marker_cmd, "--detection_batch_size", opts$detection_batch_size)
}
if (!is.na(opts$recognition_batch_size)) {
  marker_cmd <- c(marker_cmd, "--recognition_batch_size", opts$recognition_batch_size)
}
if (!is.na(opts$table_rec_batch_size)) {
  marker_cmd <- c(marker_cmd, "--table_rec_batch_size", opts$table_rec_batch_size)
}

# Add config JSON file if specified
if (!is.na(opts$config_json) && nzchar(opts$config_json)) {
  if (!file.exists(opts$config_json)) {
    stop(sprintf("Config JSON file not found: %s", opts$config_json))
  }
  marker_cmd <- c(marker_cmd, "--config_json", shQuote(opts$config_json))
}

# Build final command
cmd_str <- paste(marker_cmd, collapse = " ")
cat("Running:", cmd_str, "\n")

# Set environment variables if needed
if (!is.na(opts$device) && nzchar(opts$device)) {
  Sys.setenv(TORCH_DEVICE = opts$device)
}

# Use system which is simpler for this case
status <- system(cmd_str)

if (status != 0) {
  stop(sprintf("marker_single failed with status %s", status))
}

cat("Conversion complete. Output directory:\n", normalizePath(opts$output_dir, winslash = "/", mustWork = FALSE), "\n", sep = "")


