# Enhanced Manifest Builder Module
# Module for building enhanced manifest with comprehensive processing metadata

# Load required libraries
library(jsonlite)
library(dplyr)
library(lubridate)

# Main function to build enhanced manifest
build_enhanced_manifest <- function(processing_results, document_info = NULL, 
                                  processing_settings = NULL, output_directory = ".") {
  
  tryCatch({
    # Initialize processing statistics
    processing_stats <- collect_processing_statistics(processing_results)
    
    # Build the main manifest structure
    enhanced_manifest <- list(
      document_info = build_document_info(document_info, processing_stats),
      processing_summary = build_processing_summary(processing_stats),
      pipeline_metadata = build_pipeline_metadata(processing_settings, processing_stats),
      quality_overview = build_quality_overview(processing_stats),
      file_inventory = build_file_inventory(output_directory, processing_stats)
    )
    
    # Validate the manifest structure
    enhanced_manifest <- validate_enhanced_manifest(enhanced_manifest)
    
    return(enhanced_manifest)
    
  }, error = function(e) {
    warning(paste("Error building enhanced manifest:", e$message))
    return(NULL)
  })
}

# Build document information section
build_document_info <- function(document_info = NULL, processing_stats) {
  
  doc_info <- list(
    catalog_year = as.integer(document_info$catalog_year %||% extract_year_from_filename(document_info$source_file) %||% 2024),
    total_pages = as.integer(processing_stats$total_pages %||% 0),
    pages_processed = as.integer(processing_stats$pages_processed %||% 0),
    pages_failed = as.integer(processing_stats$pages_failed %||% 0),
    file_size = as.integer(document_info$file_size %||% 0)
  )
  
  # Add optional fields
  if (!is.null(document_info$source_file)) {
    doc_info$source_file <- as.character(document_info$source_file)
  }
  
  if (!is.null(document_info$document_title)) {
    doc_info$document_title <- as.character(document_info$document_title)
  }
  
  if (!is.null(document_info$creation_date)) {
    doc_info$creation_date <- format(document_info$creation_date, "%Y-%m-%dT%H:%M:%S%z")
  }
  
  return(doc_info)
}

# Build processing summary section
build_processing_summary <- function(processing_stats) {
  
  processing_summary <- list(
    extraction_methods = build_extraction_method_summary(processing_stats),
    language_detection = build_language_detection_summary(processing_stats),
    processing_statistics = build_processing_statistics_summary(processing_stats)
  )
  
  return(processing_summary)
}

# Build pipeline metadata section
build_pipeline_metadata <- function(processing_settings = NULL, processing_stats) {
  
  pipeline_version <- get_pipeline_version(processing_settings)
  
  metadata <- list(
    schema_version = get_schema_version(),
    processing_started = format(processing_stats$start_time %||% Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    processing_completed = format(processing_stats$end_time %||% Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
  
  # Only include pipeline_version if it's enabled and available
  if (!is.null(pipeline_version)) {
    metadata$pipeline_version <- pipeline_version
  }
  
  # Add settings snapshot
  if (!is.null(processing_settings)) {
    metadata$settings_snapshot <- processing_settings
  }
  
  # Add system information
  metadata$system_info <- build_system_info(processing_stats)
  
  return(metadata)
}

# Build quality overview section
build_quality_overview <- function(processing_stats) {
  
  quality_overview <- list(
    overall_quality = build_overall_quality_summary(processing_stats),
    confidence_overview = build_confidence_overview(processing_stats),
    error_summary = build_error_summary(processing_stats),
    content_analysis = build_content_analysis_summary(processing_stats)
  )
  
  return(quality_overview)
}

# Build file inventory section
build_file_inventory <- function(output_directory, processing_stats) {
  
  # Scan output directory for files
  page_files <- scan_page_files(output_directory, processing_stats)
  summary_files <- scan_summary_files(output_directory)
  
  file_inventory <- list(
    output_directory = normalizePath(output_directory, mustWork = FALSE),
    page_files = page_files,
    summary_files = summary_files,
    total_output_size = calculate_total_output_size(page_files, summary_files),
    file_counts = calculate_file_counts(page_files, summary_files)
  )
  
  return(file_inventory)
}

# Helper functions

# Safe null-coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# Collect processing statistics from results
collect_processing_statistics <- function(processing_results) {
  
  if (is.null(processing_results) || length(processing_results) == 0) {
    return(list(
      total_pages = 0L,
      pages_processed = 0L,
      pages_failed = 0L,
      start_time = Sys.time(),
      end_time = Sys.time(),
      processing_results = NULL
    ))
  }
  
  stats <- list()
  
  # Store the processing results for later use
  stats$processing_results <- processing_results
  
  # Count pages by status
  stats$total_pages <- length(processing_results)
  stats$pages_processed <- sum(sapply(processing_results, function(r) r$success %||% FALSE))
  stats$pages_failed <- stats$total_pages - stats$pages_processed
  
  # Collect extraction methods
  stats$extraction_methods <- sapply(processing_results, function(r) {
    r$extraction_method %||% "unknown"
  })
  
  # Collect language information
  stats$languages <- sapply(processing_results, function(r) {
    if (!is.null(r$language) && !is.null(r$language$detected)) {
      r$language$detected
    } else {
      "unknown"
    }
  })
  
  # Collect quality scores
  stats$quality_scores <- sapply(processing_results, function(r) {
    r$quality_score %||% 0.5
  })
  
  # Collect confidence scores
  stats$confidence_scores <- sapply(processing_results, function(r) {
    if (!is.null(r$overall_confidence) && !is.null(r$overall_confidence$mean)) {
      r$overall_confidence$mean
    } else {
      1.0
    }
  })
  
  # Collect processing times
  stats$processing_times <- sapply(processing_results, function(r) {
    r$processing_time %||% 0
  })
  
  # Collect errors
  stats$errors <- unlist(lapply(processing_results, function(r) r$errors), recursive = FALSE)
  stats$warnings <- unlist(lapply(processing_results, function(r) r$warnings), recursive = FALSE)
  
  # Set timing information
  stats$start_time <- min(sapply(processing_results, function(r) {
    r$start_time %||% Sys.time()
  }), na.rm = TRUE)
  
  stats$end_time <- max(sapply(processing_results, function(r) {
    r$end_time %||% Sys.time()
  }), na.rm = TRUE)
  
  return(stats)
}

# Extract year from filename
extract_year_from_filename <- function(filename) {
  if (is.null(filename)) return(NULL)
  
  # Look for 4-digit year in filename
  year_match <- regexpr("(19|20)\\d{2}", filename)
  if (year_match[1] > 0) {
    year_str <- regmatches(filename, year_match)
    return(as.integer(year_str))
  }
  
  return(NULL)
}

# Build extraction method summary with fallback tracking
build_extraction_method_summary <- function(processing_stats) {
  
  methods <- processing_stats$extraction_methods %||% character(0)
  
  method_counts <- table(methods)
  
  summary <- list(
    digital_pages = as.integer(method_counts["digital"] %||% 0),
    ocr_pages = as.integer(method_counts["ocr"] %||% method_counts["tesseract"] %||% 0),
    mixed_pages = as.integer(method_counts["mixed"] %||% 0),
    failed_pages = as.integer(processing_stats$pages_failed %||% 0)
  )
  
  # Add fallback and mixed method analysis
  if (!is.null(processing_stats$processing_results)) {
    fallback_analysis <- analyze_fallback_usage(processing_stats$processing_results)
    summary$fallback_usage <- fallback_analysis$fallback_usage
    summary$mixed_method_details <- fallback_analysis$mixed_method_details
  }
  
  return(summary)
}

# Analyze fallback method usage across pages
analyze_fallback_usage <- function(processing_results) {
  
  fallback_count <- 0
  mixed_method_count <- 0
  fallback_methods_used <- character(0)
  
  for (result in processing_results) {
    # Check if fallback methods were used
    if (!is.null(result$fallback_methods) && length(result$fallback_methods) > 0) {
      fallback_count <- fallback_count + 1
      fallback_methods_used <- c(fallback_methods_used, result$fallback_methods)
    }
    
    # Check for mixed methods (could be in extraction_method or detected separately)
    if (!is.null(result$extraction_method) && result$extraction_method == "mixed") {
      mixed_method_count <- mixed_method_count + 1
    }
    
    # Also check if processing metadata indicates mixed methods
    if (!is.null(result$processing_metadata$mixed_methods_used) && result$processing_metadata$mixed_methods_used) {
      mixed_method_count <- mixed_method_count + 1
    }
  }
  
  # Count unique fallback methods
  unique_fallbacks <- unique(fallback_methods_used)
  fallback_method_counts <- table(fallback_methods_used)
  
  fallback_usage <- list(
    pages_with_fallbacks = as.integer(fallback_count),
    total_fallback_attempts = as.integer(length(fallback_methods_used)),
    fallback_methods_used = as.character(unique_fallbacks),
    fallback_method_distribution = as.list(fallback_method_counts)
  )
  
  mixed_method_details <- list(
    pages_with_mixed_methods = as.integer(mixed_method_count),
    mixed_method_ratio = as.numeric(mixed_method_count / length(processing_results))
  )
  
  return(list(
    fallback_usage = fallback_usage,
    mixed_method_details = mixed_method_details
  ))
}

# Build language detection summary
build_language_detection_summary <- function(processing_stats) {
  
  languages <- processing_stats$languages %||% character(0)
  
  if (length(languages) == 0) {
    return(list(
      primary_language = "unknown",
      language_distribution = list(),
      mixed_language_pages = 0L,
      fallback_usage = 0L
    ))
  }
  
  # Find primary language
  lang_counts <- table(languages)
  primary_lang <- names(sort(lang_counts, decreasing = TRUE))[1]
  
  # Build language distribution
  lang_dist <- list()
  for (lang in names(lang_counts)) {
    if (lang != "unknown") {
      lang_dist[[lang]] <- list(
        page_count = as.integer(lang_counts[[lang]]),
        confidence = list(mean = 1.0)  # Simplified for now
      )
    }
  }
  
  summary <- list(
    primary_language = primary_lang,
    language_distribution = lang_dist,
    mixed_language_pages = 0L,  # Would need more detailed tracking
    fallback_usage = as.integer(lang_counts["unknown"] %||% 0)
  )
  
  return(summary)
}

# Build processing statistics summary
build_processing_statistics_summary <- function(processing_stats) {
  
  processing_times <- processing_stats$processing_times %||% numeric(0)
  
  summary <- list(
    total_processing_time = sum(processing_times, na.rm = TRUE),
    success_rate = processing_stats$pages_processed / max(processing_stats$total_pages, 1)
  )
  
  if (length(processing_times) > 0) {
    summary$average_page_time <- mean(processing_times, na.rm = TRUE)
  } else {
    summary$average_page_time <- 0
  }
  
  # Add content statistics (simplified)
  summary$pages_with_tables <- 0L  # Would need table detection results
  summary$pages_with_images <- 0L  # Would need image detection results
  
  return(summary)
}

# Get pipeline version (source from enhanced_output_builder)
source(file.path("scripts", "enhanced_output_builder.R"))

# Get schema version
get_schema_version <- function() {
  return("1.0.0")  # This should match the schema versions
}

# Build system information
build_system_info <- function(processing_stats) {
  
  system_info <- list(
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    operating_system = Sys.info()["sysname"]
  )
  
  # Add memory usage if available
  if (!is.null(processing_stats$memory_usage)) {
    system_info$memory_usage <- processing_stats$memory_usage
  }
  
  return(system_info)
}

# Build overall quality summary
build_overall_quality_summary <- function(processing_stats) {
  
  quality_scores <- processing_stats$quality_scores %||% numeric(0)
  
  if (length(quality_scores) == 0) {
    return(list(
      mean_quality = 0.5,
      quality_distribution = list(mean = 0.5, min = 0.5, max = 0.5),
      pages_high_quality = 0L,
      pages_medium_quality = 0L,
      pages_low_quality = 0L
    ))
  }
  
  # Calculate quality distribution
  quality_dist <- list(
    mean = mean(quality_scores, na.rm = TRUE),
    min = min(quality_scores, na.rm = TRUE),
    p25 = quantile(quality_scores, 0.25, na.rm = TRUE, names = FALSE),
    median = median(quality_scores, na.rm = TRUE),
    p75 = quantile(quality_scores, 0.75, na.rm = TRUE, names = FALSE),
    max = max(quality_scores, na.rm = TRUE),
    std_dev = sd(quality_scores, na.rm = TRUE)
  )
  
  # Count pages by quality level
  high_quality <- sum(quality_scores > 0.8, na.rm = TRUE)
  medium_quality <- sum(quality_scores >= 0.5 & quality_scores <= 0.8, na.rm = TRUE)
  low_quality <- sum(quality_scores < 0.5, na.rm = TRUE)
  
  summary <- list(
    mean_quality = quality_dist$mean,
    quality_distribution = quality_dist,
    pages_high_quality = as.integer(high_quality),
    pages_medium_quality = as.integer(medium_quality),
    pages_low_quality = as.integer(low_quality)
  )
  
  return(summary)
}

# Build confidence overview
build_confidence_overview <- function(processing_stats) {
  
  confidence_scores <- processing_stats$confidence_scores %||% numeric(0)
  
  if (length(confidence_scores) == 0) {
    return(list(
      mean_confidence = 1.0,
      confidence_distribution = list(mean = 1.0, min = 1.0, max = 1.0),
      low_confidence_pages = integer(0)
    ))
  }
  
  # Calculate confidence distribution
  conf_dist <- list(
    mean = mean(confidence_scores, na.rm = TRUE),
    min = min(confidence_scores, na.rm = TRUE),
    p25 = quantile(confidence_scores, 0.25, na.rm = TRUE, names = FALSE),
    median = median(confidence_scores, na.rm = TRUE),
    p75 = quantile(confidence_scores, 0.75, na.rm = TRUE, names = FALSE),
    max = max(confidence_scores, na.rm = TRUE),
    std_dev = sd(confidence_scores, na.rm = TRUE)
  )
  
  # Find low confidence pages
  low_conf_pages <- which(confidence_scores < 0.5)
  
  summary <- list(
    mean_confidence = conf_dist$mean,
    confidence_distribution = conf_dist,
    low_confidence_pages = as.integer(low_conf_pages)
  )
  
  return(summary)
}

# Build error summary
build_error_summary <- function(processing_stats) {
  
  errors <- processing_stats$errors %||% list()
  warnings <- processing_stats$warnings %||% list()
  
  # Count errors by type
  error_types <- list()
  if (length(errors) > 0) {
    for (error in errors) {
      error_type <- error$type %||% "unknown"
      if (is.null(error_types[[error_type]])) {
        error_types[[error_type]] <- list(
          count = 0L,
          affected_pages = integer(0),
          recovery_rate = 0.0
        )
      }
      error_types[[error_type]]$count <- error_types[[error_type]]$count + 1L
      
      # Add affected page if available
      if (!is.null(error$page)) {
        error_types[[error_type]]$affected_pages <- c(
          error_types[[error_type]]$affected_pages, 
          as.integer(error$page)
        )
      }
    }
    
    # Calculate recovery rates (simplified)
    for (type in names(error_types)) {
      error_types[[type]]$recovery_rate <- 0.8  # Placeholder
    }
  }
  
  summary <- list(
    total_errors = length(errors),
    total_warnings = length(warnings),
    error_types = error_types
  )
  
  return(summary)
}

# Build content analysis summary
build_content_analysis_summary <- function(processing_stats) {
  
  # Simplified content analysis - would need more detailed processing results
  summary <- list(
    total_text_coverage = 0.8,  # Placeholder
    font_diversity = list(
      unique_fonts_total = 5L,
      unique_sizes_total = 8L,
      font_consistency = 0.9
    ),
    table_analysis = list(
      total_tables = 0L,
      table_detection_confidence = list(mean = 1.0)
    )
  )
  
  return(summary)
}

# Scan page files in output directory using actual processed page numbers
scan_page_files <- function(output_directory, processing_stats) {
  
  page_files <- list()
  
  # Look for different types of page files
  file_types <- c("enhanced", "geometry", "extraction", "llm")
  
  # Get actual processed page numbers from processing_results if available
  processed_page_numbers <- get_processed_page_numbers(processing_stats)
  
  for (page_num in processed_page_numbers) {
    page_str <- sprintf("%04d", page_num)
    
    for (file_type in file_types) {
      # Determine expected filename pattern
      filename <- switch(file_type,
        "enhanced" = paste0("page_", page_str, ".enhanced.json"),
        "geometry" = paste0("page_", page_str, ".geometry.json"),
        "extraction" = paste0("page_", page_str, ".extraction.json"),
        "llm" = paste0("page_", page_str, ".llm.json")
      )
      
      file_path <- file.path(output_directory, filename)
      
      if (file.exists(file_path)) {
        file_info <- file.info(file_path)
        
        page_file <- list(
          page_number = as.integer(page_num),
          file_type = file_type,
          file_path = filename,
          file_size = as.integer(file_info$size),
          created_timestamp = format(file_info$mtime, "%Y-%m-%dT%H:%M:%S%z")
        )
        
        # Add quality score if available - use page number to find score
        if (file_type == "enhanced" && !is.null(processing_stats$quality_scores)) {
          # Find the quality score for this specific page number
          page_index <- which(processed_page_numbers == page_num)
          if (length(page_index) > 0 && page_index <= length(processing_stats$quality_scores)) {
            page_file$quality_score <- processing_stats$quality_scores[page_index]
          }
        }
        
        page_files <- append(page_files, list(page_file))
      }
    }
  }
  
  return(page_files)
}

# Get processed page numbers from processing statistics
get_processed_page_numbers <- function(processing_stats) {
  # Check if we have processing results with actual page numbers
  if (!is.null(processing_stats$processing_results)) {
    page_numbers <- sapply(processing_stats$processing_results, function(result) {
      result$page_number %||% NA
    })
    page_numbers <- page_numbers[!is.na(page_numbers)]
    if (length(page_numbers) > 0) {
      return(sort(unique(page_numbers)))
    }
  }
  
  # Fallback to sequential numbering if processing results not available
  total_pages <- processing_stats$total_pages %||% 0
  if (total_pages > 0) {
    return(seq_len(total_pages))
  }
  
  return(integer(0))
}

# Scan summary files in output directory
scan_summary_files <- function(output_directory) {
  
  summary_files <- list()
  
  # Look for summary files
  summary_file_patterns <- list(
    "manifest" = "manifest.json",
    "quality_report" = "quality_report.json",
    "processing_log" = "processing.log"
  )
  
  for (file_type in names(summary_file_patterns)) {
    filename <- summary_file_patterns[[file_type]]
    file_path <- file.path(output_directory, filename)
    
    if (file.exists(file_path)) {
      file_info <- file.info(file_path)
      
      summary_file <- list(
        file_type = file_type,
        file_path = filename,
        file_size = as.integer(file_info$size),
        created_timestamp = format(file_info$mtime, "%Y-%m-%dT%H:%M:%S%z"),
        description = switch(file_type,
          "manifest" = "Processing manifest with metadata",
          "quality_report" = "Quality assessment report",
          "processing_log" = "Processing execution log"
        )
      )
      
      summary_files <- append(summary_files, list(summary_file))
    }
  }
  
  return(summary_files)
}

# Calculate total output size
calculate_total_output_size <- function(page_files, summary_files) {
  
  total_size <- 0L
  
  # Sum page file sizes
  for (file in page_files) {
    total_size <- total_size + (file$file_size %||% 0L)
  }
  
  # Sum summary file sizes
  for (file in summary_files) {
    total_size <- total_size + (file$file_size %||% 0L)
  }
  
  return(total_size)
}

# Calculate file counts by type
calculate_file_counts <- function(page_files, summary_files) {
  
  counts <- list(
    enhanced_pages = 0L,
    geometry_files = 0L,
    extraction_files = 0L,
    llm_files = 0L
  )
  
  # Count page files by type
  for (file in page_files) {
    file_type <- file$file_type
    
    if (file_type == "enhanced") {
      counts$enhanced_pages <- counts$enhanced_pages + 1L
    } else if (file_type == "geometry") {
      counts$geometry_files <- counts$geometry_files + 1L
    } else if (file_type == "extraction") {
      counts$extraction_files <- counts$extraction_files + 1L
    } else if (file_type == "llm") {
      counts$llm_files <- counts$llm_files + 1L
    }
  }
  
  return(counts)
}

# Validate enhanced manifest structure
validate_enhanced_manifest <- function(enhanced_manifest) {
  
  # Basic validation - ensure required fields are present
  required_fields <- c("document_info", "processing_summary", "pipeline_metadata", 
                      "quality_overview", "file_inventory")
  
  for (field in required_fields) {
    if (is.null(enhanced_manifest[[field]])) {
      enhanced_manifest[[field]] <- list()
    }
  }
  
  return(enhanced_manifest)
}

# Aggregate quality metrics across pages
aggregate_quality_metrics <- function(page_results) {
  
  quality_scores <- sapply(page_results, function(page) {
    page$quality_assessment$overall_quality %||% 0.5
  })
  
  confidence_scores <- sapply(page_results, function(page) {
    page$extraction_details$overall_confidence$mean %||% 1.0
  })
  
  aggregated <- list(
    quality = list(
      mean = mean(quality_scores, na.rm = TRUE),
      distribution = calculate_distribution_stats(quality_scores)
    ),
    confidence = list(
      mean = mean(confidence_scores, na.rm = TRUE),
      distribution = calculate_distribution_stats(confidence_scores)
    )
  )
  
  return(aggregated)
}

# Calculate distribution statistics
calculate_distribution_stats <- function(values) {
  
  if (length(values) == 0) {
    return(list(mean = 0, min = 0, max = 0, std_dev = 0))
  }
  
  list(
    mean = mean(values, na.rm = TRUE),
    min = min(values, na.rm = TRUE),
    p25 = quantile(values, 0.25, na.rm = TRUE, names = FALSE),
    median = median(values, na.rm = TRUE),
    p75 = quantile(values, 0.75, na.rm = TRUE, names = FALSE),
    max = max(values, na.rm = TRUE),
    std_dev = sd(values, na.rm = TRUE)
  )
}

# Track processing errors across pages
track_processing_errors <- function(page_results) {
  
  all_errors <- list()
  
  for (i in seq_along(page_results)) {
    page <- page_results[[i]]
    
    if (!is.null(page$processing_metadata$errors)) {
      for (error in page$processing_metadata$errors) {
        error$page_number <- i
        all_errors <- append(all_errors, list(error))
      }
    }
  }
  
  return(all_errors)
}
