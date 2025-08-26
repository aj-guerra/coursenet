library(yaml)
library(logger)
library(futile.logger)
library(R6)

#' PipelineErrorHandler class for managing error handling and recovery in the PDF processing pipeline
#' @export
PipelineErrorHandler <- R6Class(
  "PipelineErrorHandler",
  public = list(
    #' @field config Pipeline configuration
    config = NULL,
    #' @field errors List of collected errors
    errors = list(),
    #' @field log_file Path to log file
    log_file = NULL,

    #' Initialize a new PipelineErrorHandler
    #' @param config_path Path to configuration file
    initialize = function(config_path = "config/settings.yml") {
      self$config <- yaml::read_yaml(config_path)
      self$setup_logging()
      self$errors <- list()
    },

    #' Set up logging configuration
    setup_logging = function() {
      # Set log level
      log_level <- switch(
        self$config$logging$log_level,
        "DEBUG" = futile.logger::DEBUG,
        "INFO" = futile.logger::INFO,
        "WARN" = futile.logger::WARN,
        "ERROR" = futile.logger::ERROR,
        futile.logger::INFO
      )
      futile.logger::flog.threshold(log_level)
      
      # Reset appenders
      futile.logger::flog.appender(NULL)
      
      # Add file appender if enabled
      if (self$config$logging$log_to_file) {
        log_path <- self$config$logging$log_file_path
        dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
        self$log_file <- log_path
        futile.logger::flog.appender(futile.logger::appender.file(log_path))
      } else {
        self$log_file <- NULL
      }
      
      # Always add console appender for visibility
      futile.logger::flog.appender(futile.logger::appender.console(), name = "console")
    },

    #' Validate pipeline configuration
    #' @param config Optional configuration to validate. If not provided, uses self$config
    #' @return logical TRUE if configuration is valid
    validate_configuration = function(config = NULL) {
      tryCatch({
        # Use provided config or fall back to self$config
        config_to_validate <- if (!is.null(config)) config else self$config
        
        required_sections <- c(
          "ocr_settings", "error_handling", "pipeline_validation",
          "logging", "quality_thresholds"
        )
        
        missing_sections <- required_sections[!required_sections %in% names(config_to_validate)]
        if (length(missing_sections) > 0) {
          self$log_error(
            "Configuration validation failed",
            paste("Missing required sections:", paste(missing_sections, collapse = ", "))
          )
          return(FALSE)
        }
        
        # Validate specific settings
        if (!is.numeric(config_to_validate$quality_thresholds$pipeline_success_threshold) ||
            config_to_validate$quality_thresholds$pipeline_success_threshold < 0 ||
            config_to_validate$quality_thresholds$pipeline_success_threshold > 1) {
          self$log_error(
            "Configuration validation failed",
            "pipeline_success_threshold must be between 0 and 1"
          )
          return(FALSE)
        }
        
        futile.logger::flog.info("Configuration validation successful")
        return(TRUE)
      }, error = function(e) {
        self$log_error("Configuration validation error", e$message)
        return(FALSE)
      })
    },

    #' Execute a function with automatic retry and fallback
    #' @param func Function to execute
    #' @param args List of arguments to pass to the function
    #' @param fallback_func Optional fallback function
    #' @param max_retries Maximum number of retry attempts
    #' @return Result of function execution or NULL on failure
    safe_execute_with_fallback = function(func, args = list(), fallback_func = NULL,
                                        max_retries = NULL) {
      if (is.null(max_retries)) {
        max_retries <- self$config$error_handling$max_retry_attempts
      }
      
      for (attempt in 1:max_retries) {
        tryCatch({
          result <- do.call(func, args)
          futile.logger::flog.info(sprintf("Function executed successfully on attempt %d", attempt))
          return(result)
        }, error = function(e) {
          self$log_error(
            sprintf("Execution failed on attempt %d", attempt),
            e$message
          )
          if (attempt == max_retries) {
            if (!is.null(fallback_func)) {
              futile.logger::flog.info("Attempting fallback execution")
              return(tryCatch(
                do.call(fallback_func, args),
                error = function(e) {
                  self$log_error("Fallback execution failed", e$message)
                  return(NULL)
                }
              ))
            }
          }
          Sys.sleep(self$config$error_handling$retry_delay_seconds)
        })
      }
      return(NULL)
    },

    #' Collect and categorize processing errors
    #' @param error Error object or message
    #' @param category Error category
    #' @param severity Error severity level
    collect_processing_errors = function(error, category = "general", severity = "error") {
      # Check if error is a condition or simpleError
      is_error_obj <- inherits(error, c("condition", "simpleError"))
      
      error_entry <- list(
        timestamp = Sys.time(),
        message = if (is_error_obj) conditionMessage(error) else as.character(error),
        category = category,
        severity = severity,
        stack_trace = if (is_error_obj) sys.calls() else NULL,
        error_class = if (is_error_obj) class(error)[1] else "character"
      )
      
      self$errors[[length(self$errors) + 1]] <- error_entry
      
      if (severity == "error") {
        futile.logger::flog.error(sprintf("[%s] %s", category, error_entry$message))
      } else if (severity == "warning") {
        futile.logger::flog.warn(sprintf("[%s] %s", category, error_entry$message))
      }
    },

    #' Generate comprehensive error report
    #' @return Character string containing the error report
    generate_error_report = function() {
      if (length(self$errors) == 0) {
        return("No errors recorded")
      }
      
      report <- "Pipeline Error Report\n"
      report <- paste0(report, "===================\n\n")
      
      # Group errors by category
      categories <- unique(sapply(self$errors, function(e) e$category))
      for (cat in categories) {
        report <- paste0(report, sprintf("Category: %s\n", cat))
        report <- paste0(report, "-----------------\n")
        
        cat_errors <- self$errors[sapply(self$errors, function(e) e$category == cat)]
        for (error in cat_errors) {
          report <- paste0(report, sprintf(
            "Time: %s\nSeverity: %s\nType: %s\nMessage: %s\n\n",
            format(error$timestamp),
            error$severity,
            error$error_class,
            error$message
          ))
        }
      }
      
      # Add error statistics
      report <- paste0(report, "\nError Statistics\n")
      report <- paste0(report, "---------------\n")
      report <- paste0(report, sprintf("Total Errors: %d\n", length(self$errors)))
      
      severity_counts <- table(sapply(self$errors, function(e) e$severity))
      for (sev in names(severity_counts)) {
        report <- paste0(report, sprintf("%s: %d\n", sev, severity_counts[sev]))
      }
      
      return(report)
    },

    #' Check output file integrity
    #' @param file_path Path to output file
    #' @param expected_format Expected file format
    #' @return logical TRUE if file is valid
    check_output_integrity = function(file_path, expected_format = NULL) {
      if (!file.exists(file_path)) {
        self$log_error("Output integrity check failed", sprintf("File not found: %s", file_path))
        return(FALSE)
      }
      
      tryCatch({
        if (!is.null(expected_format)) {
          if (expected_format == "json") {
            content <- jsonlite::read_json(file_path)
          } else if (expected_format == "yaml") {
            content <- yaml::read_yaml(file_path)
          }
        }
        
        file_size <- file.info(file_path)$size
        if (file_size == 0) {
          self$log_error("Output integrity check failed", sprintf("File is empty: %s", file_path))
          return(FALSE)
        }
        
        futile.logger::flog.info(sprintf("Output integrity check passed for %s", file_path))
        return(TRUE)
      }, error = function(e) {
        self$log_error(
          "Output integrity check failed",
          sprintf("Error reading file %s: %s", file_path, e$message)
        )
        return(FALSE)
      })
    },

    #' Log an error message
    #' @param context Error context
    #' @param message Error message
    log_error = function(context, message) {
      error_msg <- sprintf("[%s] %s", context, message)
      futile.logger::flog.error(error_msg)
      self$collect_processing_errors(message, category = context)
    },

    #' Create a pipeline recovery checkpoint
    #' @param step_name Name of the pipeline step
    #' @param state Current pipeline state
    create_recovery_checkpoint = function(step_name, state) {
      if (!self$config$error_handling$enable_pipeline_recovery) {
        return(NULL)
      }
      
      checkpoint <- list(
        step = step_name,
        timestamp = Sys.time(),
        state = state
      )
      
      # Determine checkpoint directory
      checkpoint_dir <- if (!is.null(self$log_file)) {
        dirname(self$log_file)
      } else {
        file.path("data", "logs")
      }
      dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
      
      checkpoint_file <- file.path(
        checkpoint_dir,
        sprintf("checkpoint_%s.rds", format(Sys.time(), "%Y%m%d_%H%M%S"))
      )
      
      saveRDS(checkpoint, checkpoint_file)
      futile.logger::flog.info(sprintf("Created recovery checkpoint for step: %s", step_name))
      
      return(checkpoint_file)
    },

    #' Restore from a pipeline recovery checkpoint
    #' @param checkpoint_file Path to checkpoint file
    #' @return Restored pipeline state or NULL if recovery fails
    restore_from_checkpoint = function(checkpoint_file) {
      if (!file.exists(checkpoint_file)) {
        self$log_error("Recovery failed", "Checkpoint file not found")
        return(NULL)
      }
      
      tryCatch({
        checkpoint <- readRDS(checkpoint_file)
        futile.logger::flog.info(sprintf("Restored from checkpoint: %s", checkpoint$step))
        return(checkpoint$state)
      }, error = function(e) {
        self$log_error("Recovery failed", sprintf("Error reading checkpoint: %s", e$message))
        return(NULL)
      })
    }
  )
)

#' Create a new pipeline error handler instance
#' @param config_path Path to configuration file
#' @export
create_error_handler <- function(config_path = "config/settings.yml") {
  PipelineErrorHandler$new(config_path)
}
