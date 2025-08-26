#!/usr/bin/env Rscript

# Load required libraries
library(yaml)
library(futile.logger)
library(jsonlite)

# Source utility functions and error handling modules
source(file.path("scripts", "utils_preprocess.R"))
source(file.path("scripts", "pipeline_error_handling.R"))
source(file.path("scripts", "enhanced_dependency_check.R"))

# Initialize error handler and dependency checker
error_handler <- create_error_handler()
dependency_checker <- create_dependency_checker(error_handler = error_handler)

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
argv <- parse_args(args)
if (is.null(argv$year) || is.null(argv$pdf)) {
  error_handler$log_error(
    "Command Line Arguments",
    "Usage: Rscript scripts/run_pdf_pipeline.R --year YYYY --pdf path/to/catalog.pdf"
  )
  stop()
}

year <- as.integer(argv$year)
pdf_path <- argv$pdf

if (!file.exists(pdf_path)) {
  error_handler$log_error("Input Validation", sprintf("PDF not found: %s", pdf_path))
  stop()
}

# Function to run a script with error handling and fallback
safe_run_script <- function(script_path, args, fallback_script = NULL) {
  result <- error_handler$safe_execute_with_fallback(
    function() {
      # Safely quote script path and arguments
      quoted_args <- c(shQuote(script_path), sapply(args, shQuote))
      futile.logger::flog.info(sprintf("Running: Rscript %s", paste(quoted_args, collapse = " ")))
      output <- system2("Rscript", args = quoted_args, stdout = TRUE, stderr = TRUE)
      
      if (!is.null(attr(output, "status")) && attr(output, "status") != 0) {
        stop(sprintf("Script failed with status %d", attr(output, "status")))
      }
      
      cat(paste(output, collapse = "\n"), "\n")
      return(output)
    },
    fallback_func = if (!is.null(fallback_script)) {
      function() {
        futile.logger::flog.warn(sprintf("Attempting fallback to: %s", fallback_script))
        # Safely quote fallback script path and arguments
        quoted_args <- c(shQuote(fallback_script), sapply(args, shQuote))
        output <- system2("Rscript", args = quoted_args, stdout = TRUE, stderr = TRUE)
        if (!is.null(attr(output, "status")) && attr(output, "status") != 0) {
          stop(sprintf("Fallback script failed with status %d", attr(output, "status")))
        }
        return(output)
      }
    } else {
      NULL
    }
  )
  
  if (is.null(result)) {
    error_handler$log_error(
      "Script Execution",
      sprintf("Failed to execute script: %s", script_path)
    )
    stop()
  }
  
  return(result)
}

# Initialize pipeline state
pipeline_state <- list(
  run_id = sprintf("pdf_pipeline_%d", year),
  start_time = Sys.time(),
  steps_completed = character(),
  current_step = NULL
)

# Function to update pipeline state and create checkpoint
update_pipeline_state <- function(step_name, status = "completed") {
  pipeline_state$current_step <- step_name
  if (status == "completed") {
    pipeline_state$steps_completed <- c(pipeline_state$steps_completed, step_name)
  }
  error_handler$create_recovery_checkpoint(step_name, pipeline_state)
}

# Validate configuration and dependencies
futile.logger::flog.info("=== Step 0: Validating configuration and dependencies ===")
if (!error_handler$validate_configuration()) {
  error_handler$log_error("Validation", "Configuration validation failed")
  stop()
}

# Check system requirements and dependencies
system_check <- dependency_checker$validate_system_requirements()
if (!all(sapply(system_check, function(x) x$status))) {
  error_handler$log_error("System Requirements", "System requirements validation failed")
  stop()
}

# Check Tesseract installation
if (!dependency_checker$check_tesseract_installation()) {
  error_handler$log_error("Dependencies", "Tesseract installation check failed")
  stop()
}

# Step 0: Install R dependencies
futile.logger::flog.info("=== Step 1: Installing R dependencies ===")
update_pipeline_state("install_dependencies", "in_progress")
safe_run_script("scripts/install_r_deps.R", character(0))
update_pipeline_state("install_dependencies")

# Step 1: Run initial test
futile.logger::flog.info("=== Step 2: Running initial test ===")
update_pipeline_state("initial_test", "in_progress")
initial_args <- c("--year", year, "--pdf", pdf_path)
safe_run_script("scripts/run_initial_test.R", initial_args)
update_pipeline_state("initial_test")

# Step 2: Extract PDF metadata
futile.logger::flog.info("=== Step 3: Extracting PDF metadata ===")
update_pipeline_state("metadata_extraction", "in_progress")
metadata_args <- c("--year", year, "--pdf", pdf_path)
safe_run_script("scripts/extract_pdf_metadata.R", metadata_args)
update_pipeline_state("metadata_extraction")

# Step 3: Preprocess PDF pages using unified digital/OCR detection
futile.logger::flog.info("=== Step 4: Preprocessing PDF pages (unified digital/OCR detection) ===")
update_pipeline_state("page_preprocessing", "in_progress")
preprocess_args <- c("--year", year, "--pdf", pdf_path)

# Use legacy scripts as fallback if unified preprocessing fails
safe_run_script(
  "scripts/preprocess_pdf_unified.R",
  preprocess_args,
  if (error_handler$config$error_handling$enable_fallback_to_legacy) {
    "scripts/preprocess_pdf_pages.R"
  } else {
    NULL
  }
)
update_pipeline_state("page_preprocessing")

# Validate output files
output_files <- list(
  manifest = file.path("data", "interim", sprintf("year=%d", year), "manifest.json"),
  metadata = file.path("data", "interim", sprintf("year=%d", year), "pdf_metadata", "metadata.json")
)

output_validation_failed <- FALSE
for (file_type in names(output_files)) {
  if (!error_handler$check_output_integrity(output_files[[file_type]], "json")) {
    error_handler$log_error(
      "Output Validation",
      sprintf("Failed to validate %s output: %s", file_type, output_files[[file_type]])
    )
    output_validation_failed <- TRUE
  }
}

if (output_validation_failed) {
  error_handler$log_error("Pipeline", "Output validation failed")
  stop()
}

# Note: document_map.json is generated by a separate step and is not part of unified preprocessing output
document_map_path <- file.path("data", "processed", sprintf("year=%d", year), "document_map.json")
output_files$document_map <- document_map_path

# Generate final pipeline log
pipeline_log <- list(
  run_id = pipeline_state$run_id,
  inputs = list(
    pdf = pdf_path,
    year = year,
    config_version = error_handler$config$schema_version
  ),
  steps = list(
    step0 = "dependency_validation",
    step1 = "install_dependencies",
    step2 = "initial_test",
    step3 = "metadata_extraction",
    step4 = "page_preprocessing"
  ),
  outputs = output_files,
  completed_steps = pipeline_state$steps_completed,
  status = "completed",
  start_time = pipeline_state$start_time,
  end_time = Sys.time(),
  error_report = if (length(error_handler$errors) > 0) {
    error_handler$generate_error_report()
  } else {
    NULL
  }
)

# Write pipeline log
dir.create(file.path("data", "logs"), recursive = TRUE, showWarnings = FALSE)
log_path <- file.path("data", "logs", sprintf("run_%d_pdf_pipeline.json", year))
jsonlite::write_json(pipeline_log, log_path, pretty = TRUE, auto_unbox = TRUE, null = "null")

# Write dependency report with error handling
dep_report_path <- file.path("data", "logs", sprintf("run_%d_dependency_report.txt", year))
tryCatch({
  writeLines(dependency_checker$generate_dependency_report(), dep_report_path)
  futile.logger::flog.info(sprintf("Dependency report: %s", dep_report_path))
}, error = function(e) {
  futile.logger::flog.warn(sprintf("Failed to generate dependency report: %s", e$message))
})

futile.logger::flog.info(sprintf("Pipeline log: %s", log_path))
futile.logger::flog.info("PDF pipeline completed successfully!")

# Print next steps
cat("\nNext steps:\n")
cat("1. Review extracted metadata in data/interim/year=", year, "/pdf_metadata/\n", sep = "")
cat("2. Review page blocks in data/interim/year=", year, "/pages/\n", sep = "")
cat("3. Review enhanced document map in data/processed/year=", year, "/document_map.json\n", sep = "")
cat("4. Review pipeline log and dependency report in data/logs/\n")
cat("5. Run LLM structure agent with extracted information\n")