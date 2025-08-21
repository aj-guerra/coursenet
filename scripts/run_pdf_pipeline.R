#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

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
  stop("Usage: Rscript scripts/run_pdf_pipeline.R --year YYYY --pdf path/to/catalog.pdf")
}

year <- as.integer(argv$year)
pdf_path <- argv$pdf

if (!file.exists(pdf_path)) {
  stop(sprintf("PDF not found: %s", pdf_path))
}

# Configure writable R library for child processes
lib_dir <- Sys.getenv("R_LIBS_USER", unset = "/workspace/.Rlibs")
dir.create(lib_dir, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib_dir, .libPaths()))
Sys.setenv(R_LIBS_USER = lib_dir)

# Function to run a script and capture output
run_script <- function(script_path, args) {
  cmd <- paste("Rscript", script_path, paste(args, collapse = " "))
  cat(sprintf("Running: %s\n", cmd))
  result <- system(cmd, intern = TRUE, ignore.stderr = FALSE)
  if (!is.null(attr(result, "status")) && attr(result, "status") != 0) {
    stop(sprintf("Script failed: %s", script_path))
  }
  cat(paste(result, collapse = "\n"), "\n")
  return(result)
}

# Step 0: Ensure dependencies
cat("=== Step 0: Ensuring R dependencies ===\n")
run_script("scripts/install_r_deps.R", character(0))

# Step 1: Run initial test to set up directory structure and basic document map
cat("\n=== Step 1: Running initial test ===\n")
initial_args <- c("--year", year, "--pdf", pdf_path)
run_script("scripts/run_initial_test.R", initial_args)

# Step 2: Extract PDF metadata and enhance document map
cat("\n=== Step 2: Extracting PDF metadata ===\n")
metadata_args <- c("--year", year, "--pdf", pdf_path)
run_script("scripts/extract_pdf_metadata.R", metadata_args)

# Step 3: Preprocess PDF pages into detailed blocks
cat("\n=== Step 3: Preprocessing PDF pages ===\n")
preprocess_args <- c("--year", year, "--pdf", pdf_path)
run_script("scripts/preprocess_pdf_pages.R", preprocess_args)

# Step 4: Prepare for LLM agent (placeholder for future implementation)
cat("\n=== Step 4: Preparing for LLM agent ===\n")
cat("PDF preprocessing completed. Ready for LLM structure agent.\n")
cat("Next steps:\n")
cat("1. Review extracted metadata in data/interim/year=", year, "/pdf_metadata/\n", sep = "")
cat("2. Review page blocks in data/interim/year=", year, "/pages/\n", sep = "")
cat("3. Review enhanced document map in data/processed/year=", year, "/document_map.json\n", sep = "")
cat("4. Run LLM structure agent with extracted information\n")

# Write pipeline completion log
pipeline_log <- list(
  run_id = sprintf("pdf_pipeline_%d", year),
  inputs = list(pdf = pdf_path),
  steps = list(
    step0 = "install_r_deps",
    step1 = "initial_test",
    step2 = "pdf_metadata_extraction",
    step3 = "pdf_page_preprocessing"
  ),
  outputs = list(
    manifest = file.path("data", "interim", sprintf("year=%d", year), "manifest.json"),
    metadata = file.path("data", "interim", sprintf("year=%d", year), "pdf_metadata", "metadata.json"),
    pages = file.path("data", "interim", sprintf("year=%d", year), "pages"),
    document_map = file.path("data", "processed", sprintf("year=%d", year), "document_map.json")
  ),
  status = "completed",
  timestamp = Sys.time()
)

dir.create(file.path("data", "logs"), recursive = TRUE, showWarnings = FALSE)
log_path <- file.path("data", "logs", sprintf("run_%d_pdf_pipeline.json", year))
jsonlite::write_json(pipeline_log, log_path, pretty = TRUE, auto_unbox = TRUE, null = "null")

cat(sprintf("\nPipeline log: %s\n", log_path))
cat("PDF pipeline completed successfully!\n")

