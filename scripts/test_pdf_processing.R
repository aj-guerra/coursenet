library(testthat)
library(yaml)
library(jsonlite)
library(pdftools)

source("scripts/test_data_generator.R")
source("scripts/pipeline_error_handling.R")
source("scripts/enhanced_dependency_check.R")

#' Test suite for PDF processing pipeline
#' @export
TestPDFProcessing <- R6::R6Class(
  "TestPDFProcessing",
  public = list(
    #' @field config Pipeline configuration
    config = NULL,
    #' @field test_data_generator Test data generator instance
    test_data_generator = NULL,
    #' @field error_handler Pipeline error handler instance
    error_handler = NULL,
    #' @field test_metadata Test environment metadata
    test_metadata = NULL,
    #' @field external_tools External tool dependency check results
    external_tools = NULL,

    #' Initialize a new TestPDFProcessing instance
    #' @param config_path Path to configuration file
    initialize = function(config_path = "config/settings.yml") {
      self$config <- yaml::read_yaml(config_path)
      self$test_data_generator <- create_test_data_generator(config_path)
      self$error_handler <- create_error_handler(config_path)
      
      # Check external tool dependencies
      dependency_checker <- create_dependency_checker(config_path, self$error_handler)
      self$external_tools <- dependency_checker$check_external_tools()
      
      # Set up test environment
      self$test_metadata <- self$test_data_generator$setup_test_environment()
    },

    #' Test digital PDF processing
    test_digital_pdf_processing = function() {
      test_that("Digital PDF processing works correctly", {
        # Skip if required tools are missing
        testthat::skip_if(
          !self$external_tools$qpdf$status,
          "qpdf not available"
        )
        testthat::skip_if(
          !self$external_tools$imagemagick$status,
          "ImageMagick not available"
        )
        digital_pdf <- self$test_metadata$test_files$digital_pdf
        
        # Run pipeline
        output <- tryCatch({
          system2("Rscript", c(
            "scripts/run_pdf_pipeline.R",
            "--year", "2024",
            "--pdf", digital_pdf
          ), stdout = TRUE, stderr = TRUE)
        }, error = function(e) {
          NULL
        })
        result <- !is.null(output) && is.null(attr(output, "status"))
        
        expect_true(result, "Pipeline execution failed")
        
        # Check output files
        output_dir <- file.path("data/interim/year=2024")
        expect_true(dir.exists(output_dir), "Output directory not created")
        
        # Check manifest
        manifest_path <- file.path(output_dir, "manifest.json")
        expect_true(file.exists(manifest_path), "Manifest file not created")
        manifest <- jsonlite::read_json(manifest_path)
        expect_true("pages" %in% names(manifest), "Manifest missing pages section")
        
        # Check page outputs
        pages_dir <- file.path(output_dir, "pages")
        expect_true(dir.exists(pages_dir), "Pages directory not created")
        
        # Verify content extraction
        page_files <- list.files(pages_dir, pattern = "\\.extraction\\.json$")
        expect_gt(length(page_files), 0, "No extraction files found")
        
        # Check first page content
        first_page <- jsonlite::read_json(file.path(pages_dir, page_files[1]))
        expect_true("content" %in% names(first_page), "Page content not extracted")
        
        # Verify specific content patterns
        content <- paste(first_page$content, collapse = " ")
        expect_match(content, "COMPUTER SCIENCE", "Expected content not found")
        expect_match(content, "CS 101", "Course code not found")
      })
    },

    #' Test scanned PDF processing
    test_scanned_pdf_processing = function() {
      test_that("Scanned PDF processing works correctly", {
        # Skip if required tools are missing
        testthat::skip_if(
          !self$external_tools$qpdf$status,
          "qpdf not available"
        )
        testthat::skip_if(
          !self$external_tools$imagemagick$status,
          "ImageMagick not available"
        )
        scanned_pdf <- self$test_metadata$test_files$scanned_pdf
        
        # Run pipeline
        output <- tryCatch({
          system2("Rscript", c(
            "scripts/run_pdf_pipeline.R",
            "--year", "2024",
            "--pdf", scanned_pdf
          ), stdout = TRUE, stderr = TRUE)
        }, error = function(e) {
          NULL
        })
        result <- !is.null(output) && is.null(attr(output, "status"))
        
        expect_true(result, "Pipeline execution failed")
        
        # Check OCR outputs
        output_dir <- file.path("data/interim/year=2024")
        pages_dir <- file.path(output_dir, "pages")
        page_files <- list.files(pages_dir, pattern = "\\.extraction\\.json$")
        
        # Check first page OCR results
        first_page <- jsonlite::read_json(file.path(pages_dir, page_files[1]))
        expect_true("ocr_confidence" %in% names(first_page), "OCR confidence not recorded")
        expect_gte(first_page$ocr_confidence, 
                  self$config$quality_thresholds$min_ocr_confidence,
                  "OCR confidence below threshold")
        
        # Verify specific content patterns
        content <- paste(first_page$content, collapse = " ")
        expect_match(content, "PHYSICS DEPARTMENT", "Expected content not found")
        expect_match(content, "PHYS 101", "Course code not found")
      })
    },

    #' Test mixed content PDF processing
    test_mixed_pdf_processing = function() {
      test_that("Mixed content PDF processing works correctly", {
        # Skip if required tools are missing
        testthat::skip_if(
          !self$external_tools$qpdf$status,
          "qpdf not available"
        )
        testthat::skip_if(
          !self$external_tools$imagemagick$status,
          "ImageMagick not available"
        )
        mixed_pdf <- self$test_metadata$test_files$mixed_pdf
        
        # Run pipeline
        output <- tryCatch({
          system2("Rscript", c(
            "scripts/run_pdf_pipeline.R",
            "--year", "2024",
            "--pdf", mixed_pdf
          ), stdout = TRUE, stderr = TRUE)
        }, error = function(e) {
          NULL
        })
        result <- !is.null(output) && is.null(attr(output, "status"))
        
        expect_true(result, "Pipeline execution failed")
        
        # Check outputs
        output_dir <- file.path("data/interim/year=2024")
        pages_dir <- file.path(output_dir, "pages")
        page_files <- list.files(pages_dir, pattern = "\\.extraction\\.json$")
        
        # Check processing method detection
        for (page_file in page_files) {
          page_data <- jsonlite::read_json(file.path(pages_dir, page_file))
          expect_true("processing_method" %in% names(page_data),
                     "Processing method not recorded")
          expect_true(page_data$processing_method %in% c("digital", "ocr"),
                     "Invalid processing method")
        }
      })
    },

    #' Test error handling
    test_error_handling = function() {
      test_that("Error handling works correctly", {
        # Skip if required tools are missing
        testthat::skip_if(
          !self$external_tools$qpdf$status,
          "qpdf not available"
        )
        testthat::skip_if(
          !self$external_tools$imagemagick$status,
          "ImageMagick not available"
        )
        corrupted_files <- self$test_metadata$test_files$corrupted
        
        # Create a test config that doesn't continue on page errors
        test_config <- self$config
        test_config$error_handling$continue_on_page_errors <- FALSE
        test_config_path <- file.path(self$test_metadata$test_directories[4], "test_config.yml")
        yaml::write_yaml(test_config, test_config_path)
        
        for (file_type in names(corrupted_files)) {
          # Run pipeline with corrupted file and test config
          output <- tryCatch({
            system2("Rscript", c(
              "scripts/run_pdf_pipeline.R",
              "--year", "2024",
              "--pdf", corrupted_files[[file_type]],
              "--config", test_config_path
            ), stdout = TRUE, stderr = TRUE)
          }, error = function(e) {
            NULL
          })
          result <- !is.null(output) && is.null(attr(output, "status"))
          
          # Check error handling
          expect_false(result, sprintf("Pipeline should fail for %s file", file_type))
          
          # Check error logs
          log_file <- file.path("data/logs", "pipeline.log")
          expect_true(file.exists(log_file), "Error log not created")
          
          log_content <- readLines(log_file)
          expect_true(any(grepl("ERROR", log_content)), "Error not logged")
          
          # Verify specific error messages for corrupted files
          expect_true(
            any(grepl("Failed to process page", log_content)) ||
            any(grepl("Invalid PDF structure", log_content)) ||
            any(grepl("Corrupted file", log_content)),
            "Expected error message not found"
          )
        }
      })
    },

    #' Test configuration validation
    test_configuration_validation = function() {
      test_that("Configuration validation works correctly", {
        test_configs <- self$test_metadata$test_configs
        
        # Test minimal config
        minimal_result <- self$error_handler$validate_configuration(test_configs$minimal)
        expect_true(minimal_result, "Minimal configuration validation failed")
        
        # Test invalid config
        invalid_result <- self$error_handler$validate_configuration(test_configs$invalid)
        expect_false(invalid_result, "Invalid configuration validation should fail")
        
        # Test full config
        full_result <- self$error_handler$validate_configuration(test_configs$full)
        expect_true(full_result, "Full configuration validation failed")
      })
    },

    #' Test OCR language detection
    test_ocr_language_detection = function() {
      test_that("OCR language detection works correctly", {
        # Skip if required tools are missing
        testthat::skip_if(
          !self$external_tools$qpdf$status,
          "qpdf not available"
        )
        testthat::skip_if(
          !self$external_tools$imagemagick$status,
          "ImageMagick not available"
        )
        multilingual_pdf <- self$test_metadata$test_files$multilingual_pdf
        
        # Run pipeline
        output <- tryCatch({
          system2("Rscript", c(
            "scripts/run_pdf_pipeline.R",
            "--year", "2024",
            "--pdf", multilingual_pdf
          ), stdout = TRUE, stderr = TRUE)
        }, error = function(e) {
          NULL
        })
        result <- !is.null(output) && is.null(attr(output, "status"))
        
        expect_true(result, "Pipeline execution failed")
        
        # Check language detection results
        output_dir <- file.path("data/interim/year=2024")
        pages_dir <- file.path(output_dir, "pages")
        page_files <- list.files(pages_dir, pattern = "\\.extraction\\.json$")
        
        for (page_file in page_files) {
          page_data <- jsonlite::read_json(file.path(pages_dir, page_file))
          expect_true("detected_language" %in% names(page_data),
                     "Language detection result not recorded")
          expect_true("language_confidence" %in% names(page_data),
                     "Language confidence not recorded")
        }
      })
    },

    #' Run all tests
    #' @return Test results summary
    run_all_tests = function() {
      # Create test reporter
      reporter <- testthat::JunitReporter$new()
      testthat::set_reporter(reporter)
      
      # Run all test functions
      test_functions <- c(
        "test_digital_pdf_processing",
        "test_scanned_pdf_processing",
        "test_mixed_pdf_processing",
        "test_error_handling",
        "test_configuration_validation",
        "test_ocr_language_detection"
      )
      
      results <- list()
      for (test_func in test_functions) {
        cat(sprintf("\nRunning %s...\n", test_func))
        results[[test_func]] <- do.call(test_func, list(self))
      }
      
      # Get test results from reporter
      report <- list(
        n = reporter$n,
        passed = reporter$passed,
        failed = reporter$failed,
        skipped = reporter$skipped,
        warnings = reporter$warnings,
        errors = reporter$errors,
        test_cases = reporter$cases
      )
      
      # Write test results
      report_path <- file.path(self$test_metadata$test_directories[4],
                              "test_results.json")
      jsonlite::write_json(
        list(
          timestamp = Sys.time(),
          results = report,
          test_metadata = self$test_metadata
        ),
        report_path,
        pretty = TRUE,
        auto_unbox = TRUE
      )
      
      # Also write JUnit XML report
      xml_path <- file.path(self$test_metadata$test_directories[4],
                           "test_results.xml")
      reporter$write_xml(xml_path)
      
      return(report)
    },

    #' Clean up after tests
    cleanup = function() {
      self$test_data_generator$cleanup_test_environment()
      
      # Clean up test outputs
      unlink("data/interim/year=2024", recursive = TRUE)
      unlink("data/processed/year=2024", recursive = TRUE)
    }
  )
)

#' Create a new test suite instance
#' @param config_path Path to configuration file
#' @export
create_test_suite <- function(config_path = "config/settings.yml") {
  TestPDFProcessing$new(config_path)
}

#' Main function to run all tests
#' @export
main <- function() {
  # Create and run test suite
  test_suite <- create_test_suite()
  results <- test_suite$run_all_tests()
  
  # Print results summary
  cat("\nTest Results Summary:\n")
  cat("====================\n")
  cat(sprintf("Total Tests: %d\n", results$n))
  cat(sprintf("Passed: %d\n", results$passed))
  cat(sprintf("Failed: %d\n", results$failed))
  cat(sprintf("Skipped: %d\n", results$skipped))
  cat(sprintf("Warnings: %d\n", length(results$warnings)))
  cat(sprintf("Errors: %d\n", length(results$errors)))
  
  # Print test case details if any failures
  if (results$failed > 0 || length(results$errors) > 0) {
    cat("\nFailed Test Cases:\n")
    cat("=================\n")
    for (case in results$test_cases) {
      if (case$status != "success") {
        cat(sprintf("%s: %s\n", case$test, case$message))
      }
    }
  }
  
  # Clean up
  test_suite$cleanup()
  
  # Return exit code
  if (results$failed > 0 || length(results$errors) > 0) {
    quit(status = 1)
  }
}

# Run tests if script is run directly
if (!interactive()) {
  main()
}
