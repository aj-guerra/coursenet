library(pdftools)
library(tesseract)
library(magick)
library(yaml)
library(jsonlite)

#' Test data generator for PDF processing pipeline
#' @export
TestDataGenerator <- R6::R6Class(
  "TestDataGenerator",
  public = list(
    #' @field config Pipeline configuration
    config = NULL,
    #' @field test_dir Directory for test data
    test_dir = NULL,

    #' Initialize a new TestDataGenerator
    #' @param config_path Path to configuration file
    #' @param test_dir Directory for test data
    initialize = function(config_path = "config/settings.yml",
                         test_dir = "data/test") {
      self$config <- yaml::read_yaml(config_path)
      self$test_dir <- test_dir
      dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)
    },

    #' Use an existing PDF file for testing
    #' @param output_path Output path for the PDF
    #' @return Path to PDF
    generate_sample_digital_pdf = function(output_path = NULL) {
      if (is.null(output_path)) {
        output_path <- file.path(self$test_dir, "sample_digital.pdf")
      }
      
      # Use existing PDF file
      source_pdf <- "data/raw/p411.pdf"
      if (!file.exists(source_pdf)) {
        stop("Test PDF file not found: ", source_pdf)
      }
      
      # Copy file to test directory
      dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
      file.copy(source_pdf, output_path, overwrite = TRUE)
      
      return(output_path)
    },

    #' Use an existing PDF file for testing scanned content
    #' @param output_path Output path for the PDF
    #' @return Path to PDF
    generate_sample_scanned_pdf = function(output_path = NULL) {
      if (is.null(output_path)) {
        output_path <- file.path(self$test_dir, "sample_scanned.pdf")
      }
      
      # Use existing PDF file
      source_pdf <- "data/raw/p411.pdf"
      if (!file.exists(source_pdf)) {
        stop("Test PDF file not found: ", source_pdf)
      }
      
      # Copy file to test directory
      dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
      file.copy(source_pdf, output_path, overwrite = TRUE)
      
      return(output_path)
    },

    #' Use an existing PDF file for testing multilingual content
    #' @param output_path Output path for the PDF
    #' @return Path to PDF
    generate_multilingual_test_pdf = function(output_path = NULL) {
      if (is.null(output_path)) {
        output_path <- file.path(self$test_dir, "sample_multilingual.pdf")
      }
      
      # Use existing PDF file
      source_pdf <- "data/raw/p411.pdf"
      if (!file.exists(source_pdf)) {
        stop("Test PDF file not found: ", source_pdf)
      }
      
      # Copy file to test directory
      dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
      file.copy(source_pdf, output_path, overwrite = TRUE)
      
      return(output_path)
    },

    #' Generate corrupted PDF samples for error handling testing
    #' @return List of paths to corrupted PDFs
    generate_corrupted_pdf_samples = function() {
      corrupted_dir <- file.path(self$test_dir, "corrupted")
      dir.create(corrupted_dir, recursive = TRUE, showWarnings = FALSE)
      
      corrupted_files <- list()
      
      # 1. Empty PDF
      empty_path <- file.path(corrupted_dir, "empty.pdf")
      file.create(empty_path)
      corrupted_files$empty <- empty_path
      
      # 2. Truncated PDF
      trunc_path <- file.path(corrupted_dir, "truncated.pdf")
      sample_pdf <- self$generate_sample_digital_pdf()
      file.copy(sample_pdf, trunc_path)
      con <- file(trunc_path, "wb")
      pdf_content <- readBin(sample_pdf, "raw", file.info(sample_pdf)$size)
      writeBin(pdf_content[1:(length(pdf_content)/2)], con)
      close(con)
      corrupted_files$truncated <- trunc_path
      
      # 3. Invalid content PDF
      invalid_path <- file.path(corrupted_dir, "invalid.pdf")
      writeLines(c("%PDF-1.4", "This is not a valid PDF file"), invalid_path)
      corrupted_files$invalid <- invalid_path
      
      return(corrupted_files)
    },

    #' Use an existing PDF file for testing mixed content
    #' @param output_path Output path for the PDF
    #' @return Path to PDF
    generate_mixed_content_pdf = function(output_path = NULL) {
      if (is.null(output_path)) {
        output_path <- file.path(self$test_dir, "sample_mixed.pdf")
      }
      
      # Use existing PDF file
      source_pdf <- "data/raw/p411.pdf"
      if (!file.exists(source_pdf)) {
        stop("Test PDF file not found: ", source_pdf)
      }
      
      # Copy file to test directory
      dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
      file.copy(source_pdf, output_path, overwrite = TRUE)
      
      return(output_path)
    },

    #' Create test configuration files
    #' @return List of paths to test configuration files
    create_test_configuration_files = function() {
      config_dir <- file.path(self$test_dir, "config")
      dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)
      
      configs <- list()
      
      # 1. Minimal configuration
      minimal_config <- list(
        schema_version = "0.1.0",
        ocr_settings = list(
          default_language = "eng"
        ),
        error_handling = list(
          enable_pipeline_recovery = FALSE
        )
      )
      minimal_path <- file.path(config_dir, "minimal_config.yml")
      yaml::write_yaml(minimal_config, minimal_path)
      configs$minimal <- minimal_path
      
      # 2. Invalid configuration
      invalid_config <- list(
        schema_version = "0.1.0",
        ocr_settings = list(
          default_language = 123  # Invalid type
        )
      )
      invalid_path <- file.path(config_dir, "invalid_config.yml")
      yaml::write_yaml(invalid_config, invalid_path)
      configs$invalid <- invalid_path
      
      # 3. Full test configuration
      full_config <- self$config
      full_config$test_mode <- TRUE
      full_config$ocr_settings$test_language_pack <- "eng"
      full_path <- file.path(config_dir, "full_test_config.yml")
      yaml::write_yaml(full_config, full_path)
      configs$full <- full_path
      
      return(configs)
    },

    #' Set up complete test environment
    #' @return List of test environment details
    setup_test_environment = function() {
      # Create test directory structure
      test_dirs <- c(
        file.path(self$test_dir, "input"),
        file.path(self$test_dir, "output"),
        file.path(self$test_dir, "temp"),
        file.path(self$test_dir, "logs")
      )
      sapply(test_dirs, dir.create, recursive = TRUE, showWarnings = FALSE)
      
      # Generate all test files
      test_files <- list(
        digital_pdf = self$generate_sample_digital_pdf(),
        scanned_pdf = self$generate_sample_scanned_pdf(),
        multilingual_pdf = self$generate_multilingual_test_pdf(),
        mixed_pdf = self$generate_mixed_content_pdf(),
        corrupted = self$generate_corrupted_pdf_samples()
      )
      
      # Generate test configurations
      test_configs <- self$create_test_configuration_files()
      
      # Create test metadata
      test_metadata <- list(
        test_suite_version = "0.1.0",
        generated_at = Sys.time(),
        test_files = test_files,
        test_configs = test_configs,
        test_directories = test_dirs
      )
      
      metadata_path <- file.path(self$test_dir, "test_metadata.json")
      jsonlite::write_json(
        test_metadata,
        metadata_path,
        pretty = TRUE,
        auto_unbox = TRUE
      )
      
      return(test_metadata)
    },

    #' Clean up test environment
    cleanup_test_environment = function() {
      if (dir.exists(self$test_dir)) {
        unlink(self$test_dir, recursive = TRUE)
      }
    },

    #' Validate test data integrity
    #' @param test_metadata Test metadata from setup
    #' @return logical TRUE if all test data is valid
    validate_test_data = function(test_metadata) {
      # Check all files exist
      all_files <- unlist(test_metadata$test_files)
      missing_files <- all_files[!file.exists(all_files)]
      if (length(missing_files) > 0) {
        warning(sprintf("Missing test files: %s", 
                       paste(missing_files, collapse = ", ")))
        return(FALSE)
      }
      
      # Check all directories exist
      missing_dirs <- test_metadata$test_directories[
        !dir.exists(test_metadata$test_directories)
      ]
      if (length(missing_dirs) > 0) {
        warning(sprintf("Missing test directories: %s",
                       paste(missing_dirs, collapse = ", ")))
        return(FALSE)
      }
      
      # Validate PDF files
      pdf_files <- all_files[grepl("\\.pdf$", all_files)]
      for (pdf in pdf_files) {
        if (file.info(pdf)$size == 0) {
          warning(sprintf("Empty PDF file: %s", pdf))
          return(FALSE)
        }
        
        # Skip validation for intentionally corrupted files
        if (!grepl("/corrupted/", pdf)) {
          tryCatch({
            pdf_info(pdf)
          }, error = function(e) {
            warning(sprintf("Invalid PDF file %s: %s", pdf, e$message))
            return(FALSE)
          })
        }
      }
      
      return(TRUE)
    }
  )
)

#' Create a new test data generator instance
#' @param config_path Path to configuration file
#' @param test_dir Directory for test data
#' @export
create_test_data_generator <- function(config_path = "config/settings.yml",
                                     test_dir = "data/test") {
  TestDataGenerator$new(config_path, test_dir)
}
