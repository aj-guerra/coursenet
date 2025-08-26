library(yaml)
library(futile.logger)
library(tesseract)

#' Enhanced dependency checker for PDF processing pipeline
#' @export
DependencyChecker <- R6::R6Class(
  "DependencyChecker",
  public = list(
    #' @field config Pipeline configuration
    config = NULL,
    #' @field error_handler Pipeline error handler
    error_handler = NULL,

    #' Initialize a new DependencyChecker
    #' @param config_path Path to configuration file
    #' @param error_handler Pipeline error handler instance
    initialize = function(config_path = "config/settings.yml", error_handler = NULL) {
      self$config <- yaml::read_yaml(config_path)
      self$error_handler <- error_handler
    },

    #' Check Tesseract installation and configuration
    #' @return logical TRUE if Tesseract is properly configured
    check_tesseract_installation = function() {
      tryCatch({
        # Check if tesseract is available
        tesseract_version <- tesseract::tesseract_info()$version
        if (is.null(tesseract_version)) {
          self$log_error("Tesseract not found")
          return(FALSE)
        }
        
        futile.logger::flog.info(sprintf("Found Tesseract version: %s", tesseract_version))
        
        # Check for required language packs
        required_langs <- c(self$config$ocr_settings$default_language)
        if (self$config$ocr_settings$language_detection_enabled) {
          required_langs <- c(required_langs, self$config$ocr_settings$fallback_language)
        }
        
        available_langs <- tesseract::ocr_languages()
        missing_langs <- setdiff(required_langs, available_langs)
        
        if (length(missing_langs) > 0) {
          self$log_error(sprintf(
            "Missing required Tesseract language packs: %s",
            paste(missing_langs, collapse = ", ")
          ))
          return(FALSE)
        }
        
        futile.logger::flog.info("Tesseract installation check passed")
        return(TRUE)
      }, error = function(e) {
        self$log_error(sprintf("Tesseract check failed: %s", e$message))
        return(FALSE)
      })
    },

    #' Check OCR language support
    #' @return list List of available and missing language packs
    check_ocr_language_support = function() {
      # Define required languages outside tryCatch
      required_langs <- c(self$config$ocr_settings$default_language)
      if (self$config$ocr_settings$language_detection_enabled) {
        # Add common languages that might be needed
        required_langs <- c(required_langs,
                          "eng", "fra", "deu", "spa", "ita",
                          "por", "rus", "chi_sim", "jpn", "kor")
      }

      tryCatch({
        available_langs <- tesseract::ocr_languages()
        missing_langs <- setdiff(required_langs, available_langs)
        
        result <- list(
          available = available_langs,
          missing = missing_langs,
          status = length(missing_langs) == 0
        )
        
        if (length(missing_langs) > 0) {
          self$log_warning(sprintf(
            "Some language packs are missing: %s",
            paste(missing_langs, collapse = ", ")
          ))
        }
        
        return(result)
      }, error = function(e) {
        self$log_error(sprintf("Language support check failed: %s", e$message))
        return(list(
          available = character(0),
          missing = required_langs,
          status = FALSE
        ))
      })
    },

    #' Attempt to install missing Tesseract language packs
    #' @param missing_langs Character vector of missing language codes
    #' @return logical TRUE if all installations were successful
    install_missing_tesseract_languages = function(missing_langs) {
      if (length(missing_langs) == 0) {
        return(TRUE)
      }
      
      success <- TRUE
      for (lang in missing_langs) {
        tryCatch({
          # This is a placeholder - actual installation would depend on the OS
          # and package management system
          cmd <- switch(Sys.info()["sysname"],
                       "Darwin" = sprintf("brew install tesseract-lang-%s", lang),
                       "Linux" = sprintf("sudo apt-get install tesseract-ocr-%s", lang),
                       "Windows" = sprintf("choco install tesseract-lang-%s", lang),
                       NULL)
          
          if (!is.null(cmd)) {
            futile.logger::flog.info(sprintf("Installing language pack: %s", lang))
            system(cmd)
          } else {
            self$log_error(sprintf(
              "Unsupported operating system for automatic language pack installation: %s",
              Sys.info()["sysname"]
            ))
            success <- FALSE
          }
        }, error = function(e) {
          self$log_error(sprintf("Failed to install language pack %s: %s", lang, e$message))
          success <- FALSE
        })
      }
      
      return(success)
    },

    #' Validate system requirements
    #' @return list List of validation results
    validate_system_requirements = function() {
      results <- list()
      
      # Check disk space
      tryCatch({
        free_space <- as.numeric(system("df -m . | tail -1 | awk '{print $4}'", intern = TRUE))
        results$disk_space <- list(
          status = free_space >= self$config$pipeline_validation$required_free_disk_space_mb,
          available_mb = free_space,
          required_mb = self$config$pipeline_validation$required_free_disk_space_mb
        )
        
        if (!results$disk_space$status) {
          self$log_warning(sprintf(
            "Insufficient disk space. Required: %d MB, Available: %d MB",
            results$disk_space$required_mb,
            results$disk_space$available_mb
          ))
        }
      }, error = function(e) {
        results$disk_space <- list(status = FALSE, error = e$message)
        self$log_error(sprintf("Failed to check disk space: %s", e$message))
      })
      
      # Check memory based on OS
      tryCatch({
        os_type <- Sys.info()["sysname"]
        free_mem <- NA_real_
        
        if (identical(os_type, "Linux")) {
          # Linux: use free command
          mem_info <- system("free -m | grep Mem:", intern = TRUE)
          free_mem <- as.numeric(strsplit(mem_info, "\\s+")[[1]][4])
        } else if (identical(os_type, "Darwin")) {
          # macOS: use sysctl
          mem_info <- system("sysctl -n hw.memsize", intern = TRUE)
          total_mem <- as.numeric(mem_info) / (1024 * 1024) # Convert to MB
          vm_stat <- system("vm_stat", intern = TRUE)
          page_size <- as.numeric(sub(".*page size of (\\d+) bytes.*", "\\1", vm_stat[1]))
          free_pages <- as.numeric(sub("^Pages free:\\s+(\\d+)\\.$", "\\1", grep("^Pages free:", vm_stat, value = TRUE)))
          free_mem <- (free_pages * page_size) / (1024 * 1024) # Convert to MB
        } else if (identical(os_type, "Windows")) {
          # Windows: use wmic
          mem_info <- try(system('wmic OS get FreePhysicalMemory /Value', intern = TRUE), silent = TRUE)
          if (!inherits(mem_info, "try-error")) {
            free_line <- grep("FreePhysicalMemory=", mem_info, value = TRUE)
            if (length(free_line) > 0) {
              free_kb <- as.numeric(sub("FreePhysicalMemory=(\\d+)", "\\1", free_line))
              free_mem <- free_kb / 1024 # Convert KB to MB
            }
          }
        }
        
        if (!is.na(free_mem)) {
          results$memory <- list(
            status = free_mem >= 1000,  # Require at least 1GB free memory
            available_mb = free_mem
          )
          
          if (!results$memory$status) {
            self$log_warning(sprintf("Low memory available: %d MB", free_mem))
          }
        } else {
          self$log_warning(sprintf("Could not determine free memory on %s, skipping check", os_type))
          results$memory <- list(
            status = TRUE,  # Assume OK if we can't check
            available_mb = NA_real_,
            warning = "Memory check not supported on this platform"
          )
        }
      }, error = function(e) {
        self$log_warning(sprintf("Memory check failed: %s", e$message))
        results$memory <- list(
          status = TRUE,  # Assume OK if check fails
          available_mb = NA_real_,
          error = e$message
        )
      })
      
      # Check file permissions
      tryCatch({
        test_dirs <- c("data", "data/interim", "data/processed")
        results$permissions <- list(status = TRUE, issues = character())
        
        for (dir in test_dirs) {
          if (!dir.exists(dir)) {
            dir.create(dir, recursive = TRUE, showWarnings = FALSE)
          }
          
          test_file <- file.path(dir, ".test_write")
          if (!tryCatch({
            write("test", test_file)
            unlink(test_file)
            TRUE
          }, error = function(e) FALSE)) {
            results$permissions$status <- FALSE
            results$permissions$issues <- c(
              results$permissions$issues,
              sprintf("Cannot write to directory: %s", dir)
            )
          }
        }
        
        if (!results$permissions$status) {
          self$log_warning(sprintf(
            "Permission issues detected: %s",
            paste(results$permissions$issues, collapse = "; ")
          ))
        }
      }, error = function(e) {
        results$permissions <- list(status = FALSE, error = e$message)
        self$log_error(sprintf("Failed to check permissions: %s", e$message))
      })
      
      return(results)
    },

    #' Check external tool dependencies
    #' @return list List of test results
    check_external_tools = function() {
      results <- list()
      
      # Check pandoc
      tryCatch({
        pandoc_version <- system2("pandoc", "--version", stdout = TRUE, stderr = TRUE)[1]
        results$pandoc <- list(
          status = TRUE,
          version = sub("^pandoc\\s+", "", pandoc_version)
        )
        futile.logger::flog.info(sprintf("Found pandoc version: %s", results$pandoc$version))
      }, error = function(e) {
        results$pandoc <- list(status = FALSE, error = "pandoc not found")
        self$log_error("pandoc not found or not working")
      })
      
      # Check qpdf
      tryCatch({
        qpdf_version <- system2("qpdf", "--version", stdout = TRUE, stderr = TRUE)[1]
        results$qpdf <- list(
          status = TRUE,
          version = sub("^qpdf\\s+version\\s+", "", qpdf_version)
        )
        futile.logger::flog.info(sprintf("Found qpdf version: %s", results$qpdf$version))
      }, error = function(e) {
        results$qpdf <- list(status = FALSE, error = "qpdf not found")
        self$log_error("qpdf not found or not working")
      })
      
      # Check ImageMagick (convert command)
      tryCatch({
        convert_version <- system2("convert", "--version", stdout = TRUE, stderr = TRUE)[1]
        results$imagemagick <- list(
          status = TRUE,
          version = sub("^Version:\\s+ImageMagick\\s+", "", convert_version)
        )
        futile.logger::flog.info(sprintf("Found ImageMagick version: %s", results$imagemagick$version))
      }, error = function(e) {
        results$imagemagick <- list(status = FALSE, error = "ImageMagick not found")
        self$log_error("ImageMagick not found or not working")
      })
      
      return(results)
    },
    
    #' Check PDF processing capabilities
    #' @param sample_file Path to sample PDF file
    #' @return list List of test results
    check_pdf_processing_capabilities = function(sample_file = NULL) {
      if (is.null(sample_file)) {
        sample_file <- system.file("extdata", "sample.pdf", package = "pdftools")
      }
      
      results <- list()
      
      # Check pdftools installation
      tryCatch({
        if (!requireNamespace("pdftools", quietly = TRUE)) {
          self$log_error("pdftools package not installed")
          results$pdftools <- list(status = FALSE, error = "Package not installed")
        } else {
          results$pdftools <- list(status = TRUE)
        }
      }, error = function(e) {
        results$pdftools <- list(status = FALSE, error = e$message)
      })
      
      # Test PDF reading
      if (results$pdftools$status && file.exists(sample_file)) {
        tryCatch({
          text <- pdftools::pdf_text(sample_file)
          results$pdf_read <- list(
            status = length(text) > 0,
            pages = length(text)
          )
        }, error = function(e) {
          results$pdf_read <- list(status = FALSE, error = e$message)
          self$log_error(sprintf("Failed to read PDF: %s", e$message))
        })
      }
      
      return(results)
    },

    #' Generate comprehensive dependency report
    #' @return Character string containing the dependency report
    generate_dependency_report = function() {
      report <- "PDF Processing Pipeline Dependency Report\n"
      report <- paste0(report, "=====================================\n\n")
      
      # System Information
      report <- paste0(report, "System Information:\n")
      report <- paste0(report, "-----------------\n")
      sys_info <- Sys.info()
      report <- paste0(report, sprintf("OS: %s %s\n", sys_info["sysname"], sys_info["release"]))
      report <- paste0(report, sprintf("R Version: %s\n", R.version.string))
      
      # Tesseract Status
      tesseract_check <- self$check_tesseract_installation()
      report <- paste0(report, "\nTesseract Status:\n")
      report <- paste0(report, "----------------\n")
      report <- paste0(report, sprintf("Installation: %s\n", 
                                     if(tesseract_check) "OK" else "Failed"))
      
      # Language Support
      lang_support <- self$check_ocr_language_support()
      report <- paste0(report, "\nOCR Language Support:\n")
      report <- paste0(report, "-------------------\n")
      report <- paste0(report, sprintf("Available Languages: %s\n",
                                     paste(lang_support$available, collapse = ", ")))
      if (length(lang_support$missing) > 0) {
        report <- paste0(report, sprintf("Missing Languages: %s\n",
                                       paste(lang_support$missing, collapse = ", ")))
      }
      
      # System Requirements
      sys_reqs <- self$validate_system_requirements()
      report <- paste0(report, "\nSystem Requirements:\n")
      report <- paste0(report, "------------------\n")
      if (!is.null(sys_reqs$disk_space$available_mb)) {
        report <- paste0(report, sprintf("Free Disk Space: %d MB\n",
                                       sys_reqs$disk_space$available_mb))
      }
      if (!is.null(sys_reqs$memory$available_mb)) {
        report <- paste0(report, sprintf("Free Memory: %d MB\n",
                                       sys_reqs$memory$available_mb))
      }
      
      # PDF Processing
      pdf_caps <- self$check_pdf_processing_capabilities()
      report <- paste0(report, "\nPDF Processing Capabilities:\n")
      report <- paste0(report, "-------------------------\n")
      report <- paste0(report, sprintf("pdftools: %s\n",
                                     if(pdf_caps$pdftools$status) "OK" else "Failed"))
      if (!is.null(pdf_caps$pdf_read)) {
        report <- paste0(report, sprintf("PDF Reading: %s\n",
                                       if(pdf_caps$pdf_read$status) "OK" else "Failed"))
      }
      
      return(report)
    },

    #' Fix common dependency issues
    #' @return logical TRUE if all fixes were successful
    fix_common_dependency_issues = function() {
      success <- TRUE
      
      # Check and install missing R packages
      required_packages <- c("tesseract", "pdftools", "yaml", "futile.logger", "jsonlite")
      missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
      
      if (length(missing_packages) > 0) {
        tryCatch({
          install.packages(missing_packages)
          futile.logger::flog.info(sprintf(
            "Installed missing packages: %s",
            paste(missing_packages, collapse = ", ")
          ))
        }, error = function(e) {
          self$log_error(sprintf("Failed to install packages: %s", e$message))
          success <- FALSE
        })
      }
      
      # Check and create required directories
      required_dirs <- c("data", "data/interim", "data/processed", "data/logs")
      for (dir in required_dirs) {
        if (!dir.exists(dir)) {
          tryCatch({
            dir.create(dir, recursive = TRUE, showWarnings = FALSE)
            futile.logger::flog.info(sprintf("Created directory: %s", dir))
          }, error = function(e) {
            self$log_error(sprintf("Failed to create directory %s: %s", dir, e$message))
            success <- FALSE
          })
        }
      }
      
      # Check and fix Tesseract language packs
      lang_support <- self$check_ocr_language_support()
      if (length(lang_support$missing) > 0) {
        if (!self$install_missing_tesseract_languages(lang_support$missing)) {
          success <- FALSE
        }
      }
      
      return(success)
    },

    #' Log an error message
    #' @param message Error message
    log_error = function(message) {
      if (!is.null(self$error_handler)) {
        self$error_handler$log_error("Dependency Check", message)
      }
      futile.logger::flog.error(sprintf("[Dependency Check] %s", message))
    },

    #' Log a warning message
    #' @param message Warning message
    log_warning = function(message) {
      if (!is.null(self$error_handler)) {
        self$error_handler$collect_processing_errors(message, "Dependency Check", "warning")
      }
      futile.logger::flog.warn(sprintf("[Dependency Check] %s", message))
    }
  )
)

#' Create a new dependency checker instance
#' @param config_path Path to configuration file
#' @param error_handler Pipeline error handler instance
#' @export
create_dependency_checker <- function(config_path = "config/settings.yml",
                                    error_handler = NULL) {
  DependencyChecker$new(config_path, error_handler)
}
