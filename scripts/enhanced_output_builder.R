# Enhanced Output Builder Module
# Dedicated module for building enhanced JSON outputs with comprehensive metadata

# Load required libraries
library(jsonlite)
library(dplyr)
library(lubridate)

# Main function to build enhanced page output
build_enhanced_page_output <- function(extraction_result, semantic_analysis = NULL, font_analysis = NULL, 
                                     table_detection = NULL, column_detection = NULL, page_info = NULL,
                                     processing_settings = NULL, processing_time = NULL, words = NULL, lines = NULL) {
  
  tryCatch({
    # Build the main structure
    enhanced_output <- list(
      page_info = build_page_info(extraction_result, page_info),
      extraction_details = build_extraction_details(extraction_result),
      content_structure = build_content_structure(
        extraction_result, semantic_analysis, table_detection, column_detection, words, lines
      ),
      font_analysis = build_font_analysis_detailed(font_analysis, extraction_result),
      processing_metadata = build_processing_metadata(processing_settings, processing_time, extraction_result),
      quality_assessment = build_quality_assessment(extraction_result, font_analysis, semantic_analysis, processing_settings)
    )
    
    # Validate the structure
    enhanced_output <- validate_enhanced_output(enhanced_output)
    
    return(enhanced_output)
    
  }, error = function(e) {
    warning(paste("Error building enhanced page output:", e$message))
    return(NULL)
  })
}

# Build page information section
build_page_info <- function(extraction_result, page_info = NULL) {
  
  # Extract basic page information
  page_dims <- get_page_dimensions(extraction_result)
  text_coverage <- calculate_text_coverage(extraction_result)
  
  page_info_output <- list(
    page_number = as.integer(extraction_result$page_number %||% 1),
    page_width = as.numeric(page_dims$width %||% 0),
    page_height = as.numeric(page_dims$height %||% 0),
    page_area = as.numeric(page_dims$width * page_dims$height %||% 0),
    text_coverage = as.numeric(text_coverage %||% 0)
  )
  
  # Add rotation if available
  if (!is.null(page_info$rotation)) {
    page_info_output$rotation <- as.numeric(page_info$rotation)
  }
  
  return(page_info_output)
}

# Build extraction details section
build_extraction_details <- function(extraction_result) {
  
  # Determine extraction method
  extraction_method <- determine_extraction_method(extraction_result)
  
  # Build language detection info
  language_info <- build_language_detection(extraction_result)
  
  # Calculate confidence metrics
  confidence_metrics <- calculate_confidence_metrics(extraction_result)
  
  extraction_details <- list(
    extraction_method = extraction_method$primary,
    extraction_success = as.logical(extraction_result$success %||% TRUE),
    overall_confidence = confidence_metrics$overall,
    word_confidence = confidence_metrics$word_distribution,
    line_confidence = confidence_metrics$line_distribution
  )
  
  # Add fallback methods if used
  if (length(extraction_method$fallbacks) > 0) {
    extraction_details$fallback_methods <- extraction_method$fallbacks
  }
  
  # Add language detection if available
  if (!is.null(language_info)) {
    extraction_details$language_detection <- language_info
  }
  
  return(extraction_details)
}

# Build content structure section
build_content_structure <- function(extraction_result, semantic_analysis = NULL, 
                                   table_detection = NULL, column_detection = NULL, words = NULL, lines = NULL) {
  
  content_structure <- list()
  
  # Add words with detailed metadata - prioritize passed words over extraction_result
  words_data <- if (!is.null(words)) words else extraction_result$words
  if (!is.null(words_data)) {
    content_structure$words <- build_word_structures(words_data)
  }
  
  # Add lines with metadata - prioritize passed lines over extraction_result
  lines_data <- if (!is.null(lines)) lines else extraction_result$lines
  if (!is.null(lines_data)) {
    content_structure$lines <- build_line_structures(lines_data)
  }
  
  # Add columns if available
  if (!is.null(column_detection)) {
    # Get page height from extraction_result or passed page_info
    page_height <- NULL
    if (!is.null(extraction_result$page_height)) {
      page_height <- extraction_result$page_height
    } else if (!is.null(extraction_result$page_dimensions$height)) {
      page_height <- extraction_result$page_dimensions$height
    }
    content_structure$columns <- build_column_structures(column_detection, page_height)
  }
  
  # Add semantic blocks if available
  if (!is.null(semantic_analysis)) {
    content_structure$semantic_blocks <- build_semantic_block_structures(semantic_analysis)
  }
  
  # Add tables if available
  if (!is.null(table_detection)) {
    content_structure$tables <- build_table_structures(table_detection)
  }
  
  return(content_structure)
}

# Build detailed font analysis section
build_font_analysis_detailed <- function(font_analysis, extraction_result = NULL) {
  
  if (is.null(font_analysis)) {
    return(list(font_statistics = list(unique_fonts = 0L, unique_sizes = 0L)))
  }
  
  # Handle list format from analyze_font_characteristics
  text_data <- NULL
  font_stats_data <- NULL
  
  if (is.list(font_analysis) && !is.data.frame(font_analysis)) {
    # Extract components from the list structure
    text_data <- font_analysis$text_data
    font_stats_data <- font_analysis$font_stats
  } else if (is.data.frame(font_analysis)) {
    # Legacy data.frame format
    text_data <- font_analysis
  } else {
    # Unexpected format
    text_data <- font_analysis
  }
  
  # Calculate font statistics with NULL checks
  font_stats <- calculate_font_statistics(text_data, font_stats_data)
  
  # Build detailed font information with NULL checks
  font_details <- build_font_details(text_data, font_stats_data)
  
  # Calculate size distribution with NULL checks
  size_dist <- calculate_size_distribution(text_data, font_stats_data)
  
  font_analysis_output <- list(
    font_statistics = font_stats,
    font_details = font_details,
    size_distribution = size_dist
  )
  
  return(font_analysis_output)
}

# Build processing metadata section
build_processing_metadata <- function(processing_settings = NULL, processing_time = NULL, extraction_result = NULL) {
  
  pipeline_version <- get_pipeline_version(processing_settings)
  
  metadata <- list(
    processing_timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
  
  # Only include pipeline_version if it's enabled and available
  if (!is.null(pipeline_version)) {
    metadata$pipeline_version <- pipeline_version
  }
  
  # Add processing time if available
  if (!is.null(processing_time)) {
    metadata$processing_time <- as.numeric(processing_time)
  }
  
  # Add settings if available
  if (!is.null(processing_settings)) {
    metadata$settings_used <- processing_settings
  }
  
  # Add errors and warnings
  if (!is.null(extraction_result$errors)) {
    metadata$errors <- build_processing_errors(extraction_result$errors)
  }
  
  if (!is.null(extraction_result$warnings)) {
    metadata$warnings <- as.character(extraction_result$warnings)
  }
  
  return(metadata)
}

# Build quality assessment section
build_quality_assessment <- function(extraction_result, font_analysis = NULL, semantic_analysis = NULL, settings = NULL) {
  
  # Prioritize extraction_result quality_scores and confidence_metrics
  overall_quality <- get_overall_quality_from_extraction(extraction_result, font_analysis, semantic_analysis)
  text_density <- get_text_density_from_extraction(extraction_result, settings)
  
  # Analyze character distribution (can be computed from extraction)
  char_distribution <- analyze_character_distribution(extraction_result)
  
  # Calculate spatial consistency (fallback computation)
  spatial_consistency <- get_spatial_consistency_from_extraction(extraction_result)
  
  # Calculate font coverage
  font_coverage <- get_font_coverage_from_extraction(extraction_result, font_analysis)
  
  # Build confidence breakdown from confidence_metrics
  confidence_breakdown <- build_confidence_breakdown_from_metrics(extraction_result)
  
  quality_assessment <- list(
    overall_quality = clamp_score(overall_quality),
    text_density = clamp_score(text_density),
    character_distribution = char_distribution,
    spatial_consistency = clamp_score(spatial_consistency),
    font_coverage = clamp_score(font_coverage),
    confidence_breakdown = confidence_breakdown
  )
  
  return(quality_assessment)
}

# Clamp scores to [0,1] range
clamp_score <- function(score) {
  if (is.null(score) || is.na(score)) return(0.5)
  return(max(0, min(1, as.numeric(score))))
}

# Get overall quality from extraction result, fallback to computation
get_overall_quality_from_extraction <- function(extraction_result, font_analysis = NULL, semantic_analysis = NULL) {
  # Check quality_scores first
  if (!is.null(extraction_result$quality_scores$overall_quality)) {
    return(extraction_result$quality_scores$overall_quality)
  }
  
  # Fallback to computation
  return(calculate_overall_quality(extraction_result, font_analysis, semantic_analysis))
}

# Get text density from extraction result, fallback to computation
get_text_density_from_extraction <- function(extraction_result, settings = NULL) {
  # Check quality_scores first
  if (!is.null(extraction_result$quality_scores$text_density)) {
    return(extraction_result$quality_scores$text_density)
  }
  
  # Fallback to computation
  return(calculate_text_density(extraction_result, settings))
}

# Get spatial consistency from extraction result, fallback to computation
get_spatial_consistency_from_extraction <- function(extraction_result) {
  # Check quality_scores first
  if (!is.null(extraction_result$quality_scores$spatial_consistency)) {
    return(extraction_result$quality_scores$spatial_consistency)
  }
  
  # Fallback to computation
  return(calculate_spatial_consistency(extraction_result))
}

# Get font coverage from extraction result, fallback to computation
get_font_coverage_from_extraction <- function(extraction_result, font_analysis = NULL) {
  # Check quality_scores first
  if (!is.null(extraction_result$quality_scores$font_coverage)) {
    return(extraction_result$quality_scores$font_coverage)
  }
  
  # Fallback to computation
  return(calculate_font_coverage(font_analysis, extraction_result))
}

# Build confidence breakdown from confidence_metrics
build_confidence_breakdown_from_metrics <- function(extraction_result) {
  # Use confidence_metrics if available
  if (!is.null(extraction_result$confidence_metrics)) {
    conf_metrics <- extraction_result$confidence_metrics
    
    # Extract breakdown if available
    if (!is.null(conf_metrics$confidence_breakdown)) {
      breakdown <- conf_metrics$confidence_breakdown
      return(list(
        high_confidence_ratio = as.numeric(breakdown$high_confidence_ratio %||% 0.8),
        medium_confidence_ratio = as.numeric(breakdown$medium_confidence_ratio %||% 0.2),
        low_confidence_ratio = as.numeric(breakdown$low_confidence_ratio %||% 0.0)
      ))
    }
    
    # Calculate from word/line distributions
    if (!is.null(conf_metrics$word_distribution) || !is.null(conf_metrics$line_distribution)) {
      # Use existing confidence distribution data
      word_dist <- conf_metrics$word_distribution
      line_dist <- conf_metrics$line_distribution
      
      # Combine word and line confidence data
      all_confidences <- c()
      if (!is.null(word_dist) && !is.null(word_dist$values)) {
        all_confidences <- c(all_confidences, word_dist$values)
      }
      if (!is.null(line_dist) && !is.null(line_dist$values)) {
        all_confidences <- c(all_confidences, line_dist$values)
      }
      
      if (length(all_confidences) > 0) {
        total_count <- length(all_confidences)
        high_count <- sum(all_confidences >= 0.8)
        medium_count <- sum(all_confidences >= 0.5 & all_confidences < 0.8)
        low_count <- sum(all_confidences < 0.5)
        
        return(list(
          high_confidence_ratio = high_count / total_count,
          medium_confidence_ratio = medium_count / total_count,
          low_confidence_ratio = low_count / total_count
        ))
      }
    }
  }
  
  # Fallback to original computation
  return(build_confidence_breakdown(extraction_result))
}

# Helper functions

# Safe null-coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# Get page dimensions from extraction result
get_page_dimensions <- function(extraction_result) {
  list(
    width = extraction_result$page_width %||% extraction_result$width %||% 612,
    height = extraction_result$page_height %||% extraction_result$height %||% 792
  )
}

# Calculate text coverage ratio using union area computation
calculate_text_coverage <- function(extraction_result, settings = NULL) {
  if (is.null(extraction_result$words) || length(extraction_result$words) == 0) {
    return(0.0)
  }
  
  page_dims <- get_page_dimensions(extraction_result)
  page_area <- page_dims$width * page_dims$height
  
  if (page_area == 0) return(0.0)
  
  # Get analysis detail level from settings
  detail_level <- "detailed"
  if (!is.null(settings) && !is.null(settings$quality_reporting$spatial_analysis_detail_level)) {
    detail_level <- settings$quality_reporting$spatial_analysis_detail_level
  }
  
  # Calculate union area based on detail level
  text_area <- switch(detail_level,
    "basic" = calculate_text_area_naive(extraction_result$words),
    "detailed" = calculate_text_area_union_grid(extraction_result$words, page_dims),
    "comprehensive" = calculate_text_area_union_precise(extraction_result$words),
    calculate_text_area_union_grid(extraction_result$words, page_dims)  # Default
  )
  
  return(min(text_area / page_area, 1.0))
}

# Naive area calculation (original method)
calculate_text_area_naive <- function(words) {
  text_area <- 0
  for (word in words) {
    if (!is.null(word$bbox)) {
      bbox <- normalize_bounding_box(word$bbox)
      text_area <- text_area + (bbox$width * bbox$height)
    }
  }
  return(text_area)
}

# Grid-based union area approximation (fast for large documents)
calculate_text_area_union_grid <- function(words, page_dims, grid_resolution = 10) {
  if (length(words) == 0) return(0.0)
  
  # Create a grid to track coverage
  grid_width <- max(50, round(page_dims$width / grid_resolution))
  grid_height <- max(50, round(page_dims$height / grid_resolution))
  
  # Use a logical matrix to track covered grid cells
  grid <- matrix(FALSE, nrow = grid_height, ncol = grid_width)
  
  cell_width <- page_dims$width / grid_width
  cell_height <- page_dims$height / grid_height
  
  # Mark grid cells covered by word bounding boxes
  for (word in words) {
    if (!is.null(word$bbox)) {
      bbox <- normalize_bounding_box(word$bbox)
      
      # Convert bbox to grid coordinates
      x1 <- max(1, floor(bbox$x / cell_width) + 1)
      y1 <- max(1, floor(bbox$y / cell_height) + 1)
      x2 <- min(grid_width, ceiling((bbox$x + bbox$width) / cell_width))
      y2 <- min(grid_height, ceiling((bbox$y + bbox$height) / cell_height))
      
      # Mark covered cells
      if (x1 <= x2 && y1 <= y2) {
        grid[y1:y2, x1:x2] <- TRUE
      }
    }
  }
  
  # Calculate total covered area
  covered_cells <- sum(grid)
  cell_area <- cell_width * cell_height
  
  return(covered_cells * cell_area)
}

# Precise union area calculation using interval merging
calculate_text_area_union_precise <- function(words) {
  if (length(words) == 0) return(0.0)
  
  # Extract and normalize all bounding boxes
  boxes <- list()
  for (word in words) {
    if (!is.null(word$bbox)) {
      bbox <- normalize_bounding_box(word$bbox)
      if (bbox$width > 0 && bbox$height > 0) {
        boxes[[length(boxes) + 1]] <- list(
          x1 = bbox$x,
          y1 = bbox$y,
          x2 = bbox$x + bbox$width,
          y2 = bbox$y + bbox$height
        )
      }
    }
  }
  
  if (length(boxes) == 0) return(0.0)
  
  # Use sweep line algorithm for precise union calculation
  return(calculate_union_area_sweep_line(boxes))
}

# Sweep line algorithm for calculating union of rectangles
calculate_union_area_sweep_line <- function(boxes) {
  if (length(boxes) == 0) return(0.0)
  
  # Create events for sweep line
  events <- list()
  
  for (i in seq_along(boxes)) {
    box <- boxes[[i]]
    # Add start event (entering rectangle)
    events[[length(events) + 1]] <- list(x = box$x1, type = "start", y1 = box$y1, y2 = box$y2, box_id = i)
    # Add end event (leaving rectangle)
    events[[length(events) + 1]] <- list(x = box$x2, type = "end", y1 = box$y1, y2 = box$y2, box_id = i)
  }
  
  # Sort events by x coordinate
  events <- events[order(sapply(events, function(e) e$x))]
  
  total_area <- 0
  active_intervals <- list()
  prev_x <- 0
  
  for (event in events) {
    # Calculate area contribution from previous x to current x
    if (length(active_intervals) > 0 && event$x > prev_x) {
      width <- event$x - prev_x
      height <- calculate_union_height(active_intervals)
      total_area <- total_area + (width * height)
    }
    
    # Update active intervals
    if (event$type == "start") {
      active_intervals[[length(active_intervals) + 1]] <- list(y1 = event$y1, y2 = event$y2, box_id = event$box_id)
    } else {
      # Remove interval
      active_intervals <- active_intervals[sapply(active_intervals, function(iv) iv$box_id != event$box_id)]
    }
    
    prev_x <- event$x
  }
  
  return(total_area)
}

# Calculate union height of overlapping y-intervals
calculate_union_height <- function(intervals) {
  if (length(intervals) == 0) return(0.0)
  
  # Sort intervals by y1
  intervals <- intervals[order(sapply(intervals, function(iv) iv$y1))]
  
  total_height <- 0
  current_start <- intervals[[1]]$y1
  current_end <- intervals[[1]]$y2
  
  for (i in seq_along(intervals)) {
    interval <- intervals[[i]]
    
    if (interval$y1 <= current_end) {
      # Overlapping or adjacent - merge
      current_end <- max(current_end, interval$y2)
    } else {
      # Non-overlapping - add previous segment and start new one
      total_height <- total_height + (current_end - current_start)
      current_start <- interval$y1
      current_end <- interval$y2
    }
  }
  
  # Add final segment
  total_height <- total_height + (current_end - current_start)
  
  return(total_height)
}

# Determine extraction method used
determine_extraction_method <- function(extraction_result) {
  method <- extraction_result$extraction_method %||% "unknown"
  fallbacks <- extraction_result$fallback_methods %||% character(0)
  
  # Check for mixed method detection in metadata
  mixed_detected <- FALSE
  if (!is.null(extraction_result$processing_metadata)) {
    mixed_detected <- extraction_result$processing_metadata$mixed_methods_used %||% FALSE
  }
  
  # Check if both digital and OCR were used (mixed)
  if (mixed_detected || (tolower(method) %in% c("digital", "pdf_data") && 
                        length(fallbacks) > 0 && any(tolower(fallbacks) %in% c("ocr", "tesseract", "pdf_ocr_data")))) {
    primary_method <- "mixed"
  } else {
    # Standardize method names according to schema enum
    primary_method <- switch(tolower(method),
      "digital" = "digital",
      "pdf_data" = "digital",
      "ocr" = "ocr",
      "tesseract" = "ocr", 
      "pdf_ocr_data" = "ocr",
      "mixed" = "mixed",
      # Default to ocr for unknown methods to be conservative
      if (method == "unknown") "ocr" else "digital"
    )
  }
  
  # Normalize fallback methods to schema enums
  normalized_fallbacks <- character(0)
  if (length(fallbacks) > 0) {
    normalized_fallbacks <- sapply(fallbacks, function(fb) {
      switch(tolower(fb),
        "digital" = "digital",
        "pdf_data" = "digital", 
        "ocr" = "ocr",
        "tesseract" = "ocr",
        "pdf_ocr_data" = "ocr",
        "manual" = "manual",
        fb  # Keep as-is if not recognized
      )
    })
    # Remove duplicates and ensure they're valid schema values
    normalized_fallbacks <- unique(normalized_fallbacks[normalized_fallbacks %in% c("digital", "ocr", "manual")])
  }
  
  list(
    primary = primary_method,
    fallbacks = normalized_fallbacks
  )
}

# Build language detection information
build_language_detection <- function(extraction_result) {
  # Check processing_metadata.language first, then other locations
  language_data <- NULL
  
  if (!is.null(extraction_result$processing_metadata$language)) {
    language_data <- extraction_result$processing_metadata$language
  } else if (!is.null(extraction_result$language)) {
    language_data <- extraction_result$language
  }
  
  if (is.null(language_data)) {
    return(NULL)
  }
  
  # Extract detected language and map from Tesseract codes to ISO 639-1
  detected_lang <- language_data$detected %||% language_data$language %||% "eng"
  mapped_lang <- map_tesseract_to_iso639(detected_lang)
  
  lang_info <- list(
    detected_language = mapped_lang,
    language_confidence = as.numeric(language_data$confidence %||% 1.0)
  )
  
  # Add fallback languages if available
  if (!is.null(language_data$alternatives)) {
    fallback_mapped <- sapply(language_data$alternatives, map_tesseract_to_iso639)
    lang_info$fallback_languages <- as.character(fallback_mapped)
  }
  
  if (!is.null(language_data$mixed)) {
    lang_info$mixed_language <- as.logical(language_data$mixed)
  }
  
  return(lang_info)
}

# Map Tesseract language codes to ISO 639-1 codes
map_tesseract_to_iso639 <- function(tesseract_code) {
  if (is.null(tesseract_code) || nchar(tesseract_code) == 0) {
    return("en")
  }
  
  # Mapping from Tesseract codes to ISO 639-1
  mapping <- list(
    "eng" = "en", "spa" = "es", "fra" = "fr", "deu" = "de", "ita" = "it", "por" = "pt",
    "nld" = "nl", "dan" = "da", "nor" = "no", "swe" = "sv", "fin" = "fi", "tur" = "tr",
    "pol" = "pl", "ces" = "cs", "slk" = "sk", "hun" = "hu", "ron" = "ro", "rus" = "ru",
    "ukr" = "uk", "bul" = "bg", "ell" = "el", "srp" = "sr", "hrv" = "hr", "bos" = "bs",
    "slv" = "sl", "est" = "et", "lav" = "lv", "lit" = "lt", "chi_sim" = "zh", 
    "chi_tra" = "zh", "jpn" = "ja", "kor" = "ko", "vie" = "vi", "tha" = "th", 
    "hin" = "hi", "ben" = "bn", "tam" = "ta", "tel" = "te", "guj" = "gu", "pan" = "pa",
    "urd" = "ur", "ara" = "ar", "heb" = "he", "fas" = "fa", "ind" = "id", "msa" = "ms",
    "swa" = "sw", "afr" = "af", "gle" = "ga", "mlt" = "mt", "isl" = "is", "sqi" = "sq",
    "mkd" = "mk", "kat" = "ka", "hye" = "hy", "aze" = "az", "kaz" = "kk", "kir" = "ky",
    "mon" = "mn", "nep" = "ne", "sin" = "si", "amh" = "am", "mya" = "my", "khm" = "km",
    "lao" = "lo", "pus" = "ps"
  )
  
  # Check if it's already ISO 639-1 (2 characters)
  if (nchar(tesseract_code) == 2) {
    return(tesseract_code)
  }
  
  # Look up in mapping
  mapped <- mapping[[tolower(tesseract_code)]]
  if (!is.null(mapped)) {
    return(mapped)
  }
  
  # Default fallback
  return("en")
}

# Calculate confidence metrics and distributions
calculate_confidence_metrics <- function(extraction_result) {
  
  # Extract confidence scores
  word_confidences <- extract_confidence_scores(extraction_result$words, "confidence")
  line_confidences <- extract_confidence_scores(extraction_result$lines, "confidence")
  
  # Calculate overall confidence
  all_confidences <- c(word_confidences, line_confidences)
  overall_conf <- if (length(all_confidences) > 0) {
    list(
      mean = mean(all_confidences, na.rm = TRUE),
      count = length(all_confidences)
    )
  } else {
    # Set mean to NA when count is 0 to avoid inflating confidence
    list(mean = NA_real_, count = 0L)
  }
  
  list(
    overall = overall_conf,
    word_distribution = calculate_confidence_distribution(word_confidences),
    line_distribution = calculate_confidence_distribution(line_confidences)
  )
}

# Extract confidence scores from elements
extract_confidence_scores <- function(elements, conf_field = "confidence") {
  if (is.null(elements) || length(elements) == 0) {
    return(numeric(0))
  }
  
  confidences <- sapply(elements, function(el) {
    conf <- el[[conf_field]]
    if (is.null(conf) || is.na(conf)) return(1.0)
    as.numeric(conf)
  })
  
  return(confidences[!is.na(confidences)])
}

# Calculate confidence distribution statistics
calculate_confidence_distribution <- function(confidences) {
  if (length(confidences) == 0) {
    return(list(
      mean = 1.0,
      min = 1.0,
      p25 = 1.0,
      median = 1.0,
      p75 = 1.0,
      max = 1.0,
      low_confidence_count = 0L,
      total_count = 0L
    ))
  }
  
  # Ensure valid range
  confidences <- pmax(0, pmin(1, confidences))
  
  list(
    mean = mean(confidences, na.rm = TRUE),
    min = min(confidences, na.rm = TRUE),
    p25 = quantile(confidences, 0.25, na.rm = TRUE, names = FALSE),
    median = median(confidences, na.rm = TRUE),
    p75 = quantile(confidences, 0.75, na.rm = TRUE, names = FALSE),
    max = max(confidences, na.rm = TRUE),
    low_confidence_count = sum(confidences < 0.5, na.rm = TRUE),
    total_count = length(confidences)
  )
}

# Build word structures with detailed metadata
build_word_structures <- function(words) {
  if (is.null(words) || length(words) == 0) {
    return(list())
  }
  
  word_structures <- lapply(seq_along(words), function(i) {
    word <- words[[i]]
    
    structure <- list(
      word_id = as.integer(i),
      text = as.character(word$text %||% ""),
      bounding_box = normalize_bounding_box(word$bbox)
    )
    
    # Add optional fields
    if (!is.null(word$line_id)) structure$line_id <- as.integer(word$line_id)
    if (!is.null(word$confidence)) structure$confidence <- as.numeric(word$confidence)
    if (!is.null(word$font_size)) structure$font_size <- as.numeric(word$font_size)
    if (!is.null(word$font_family)) structure$font_family <- as.character(word$font_family)
    if (!is.null(word$font_style)) structure$font_style <- normalize_font_style(word$font_style)
    
    structure
  })
  
  return(word_structures)
}

# Build line structures with metadata
build_line_structures <- function(lines) {
  if (is.null(lines) || length(lines) == 0) {
    return(list())
  }
  
  line_structures <- lapply(seq_along(lines), function(i) {
    line <- lines[[i]]
    
    structure <- list(
      line_id = as.integer(i),
      text = as.character(line$text %||% ""),
      bounding_box = normalize_bounding_box(line$bbox)
    )
    
    # Add optional fields
    if (!is.null(line$word_ids)) structure$word_ids <- as.integer(line$word_ids)
    if (!is.null(line$column_id)) structure$column_id <- as.integer(line$column_id)
    if (!is.null(line$confidence)) structure$confidence <- as.numeric(line$confidence)
    if (!is.null(line$font_size)) structure$font_size <- as.numeric(line$font_size)
    if (!is.null(line$font_family)) structure$font_family <- as.character(line$font_family)
    
    structure
  })
  
  return(line_structures)
}

# Normalize bounding box format
normalize_bounding_box <- function(bbox) {
  if (is.null(bbox)) {
    return(list(x = 0, y = 0, width = 0, height = 0))
  }
  
  # Handle different bbox formats
  if (is.list(bbox)) {
    return(list(
      x = as.numeric(bbox$x %||% bbox$left %||% 0),
      y = as.numeric(bbox$y %||% bbox$top %||% 0),
      width = as.numeric(bbox$width %||% (bbox$right - bbox$left) %||% 0),
      height = as.numeric(bbox$height %||% (bbox$bottom - bbox$top) %||% 0)
    ))
  }
  
  # Handle vector format [x, y, width, height] or [left, top, right, bottom]
  if (is.numeric(bbox) && length(bbox) >= 4) {
    if (length(bbox) == 4 && bbox[3] > bbox[1] && bbox[4] > bbox[2]) {
      # Assume [left, top, right, bottom]
      return(list(
        x = bbox[1],
        y = bbox[2],
        width = bbox[3] - bbox[1],
        height = bbox[4] - bbox[2]
      ))
    } else {
      # Assume [x, y, width, height]
      return(list(
        x = bbox[1],
        y = bbox[2],
        width = bbox[3],
        height = bbox[4]
      ))
    }
  }
  
  return(list(x = 0, y = 0, width = 0, height = 0))
}

# Normalize font style
normalize_font_style <- function(style) {
  if (is.null(style)) return("normal")
  
  style_lower <- tolower(as.character(style))
  
  # Map common style variants
  if (grepl("bold.*italic|italic.*bold", style_lower)) return("bold-italic")
  if (grepl("bold", style_lower)) return("bold")
  if (grepl("italic", style_lower)) return("italic")
  
  return("normal")
}

# Get pipeline version from multiple sources
get_pipeline_version <- function(settings = NULL) {
  # Check if version tracking is enabled in settings
  if (!is.null(settings) && !is.null(settings$metadata_tracking$include_pipeline_version) && 
      !settings$metadata_tracking$include_pipeline_version) {
    return(NULL)
  }
  
  # Try to get version from settings first
  if (!is.null(settings) && !is.null(settings$pipeline_version)) {
    return(as.character(settings$pipeline_version))
  }
  
  # Try to read from VERSION file
  version_file <- "VERSION"
  if (file.exists(version_file)) {
    version_content <- tryCatch({
      readLines(version_file, n = 1, warn = FALSE)
    }, error = function(e) NULL)
    
    if (!is.null(version_content) && length(version_content) > 0 && nchar(trimws(version_content[1])) > 0) {
      return(trimws(version_content[1]))
    }
  }
  
  # Try to get from git if available
  git_version <- get_git_version()
  if (!is.null(git_version)) {
    return(git_version)
  }
  
  # Try to get from settings.yml schema_version
  if (!is.null(settings) && !is.null(settings$schema_version)) {
    return(paste0("schema-", settings$schema_version))
  }
  
  # Fallback to default
  return("1.0.0")
}

# Get version from git if available
get_git_version <- function() {
  git_version <- tryCatch({
    # Try to get git describe
    system("git describe --tags --always 2>/dev/null", intern = TRUE)
  }, error = function(e) NULL, warning = function(w) NULL)
  
  if (!is.null(git_version) && length(git_version) > 0 && nchar(git_version[1]) > 0) {
    return(git_version[1])
  }
  
  # Try to get just the short commit hash
  git_hash <- tryCatch({
    system("git rev-parse --short HEAD 2>/dev/null", intern = TRUE)
  }, error = function(e) NULL, warning = function(w) NULL)
  
  if (!is.null(git_hash) && length(git_hash) > 0 && nchar(git_hash[1]) > 0) {
    return(paste0("git-", git_hash[1]))
  }
  
  return(NULL)
}

# Build processing errors structure
build_processing_errors <- function(errors) {
  if (is.null(errors) || length(errors) == 0) {
    return(list())
  }
  
  lapply(errors, function(error) {
    list(
      error_type = as.character(error$type %||% "unknown"),
      message = as.character(error$message %||% ""),
      recovery_method = as.character(error$recovery %||% ""),
      affected_elements = as.character(error$affected %||% character(0))
    )
  })
}

# Calculate overall quality score
calculate_overall_quality <- function(extraction_result, font_analysis = NULL, semantic_analysis = NULL) {
  scores <- c()
  
  # Text extraction quality
  if (!is.null(extraction_result$confidence)) {
    scores <- c(scores, mean(unlist(extraction_result$confidence), na.rm = TRUE))
  }
  
  # Text density score
  text_density <- calculate_text_density(extraction_result)
  if (!is.na(text_density)) {
    scores <- c(scores, text_density)
  }
  
  # Font coverage score
  font_coverage <- calculate_font_coverage(font_analysis, extraction_result)
  if (!is.na(font_coverage)) {
    scores <- c(scores, font_coverage)
  }
  
  if (length(scores) == 0) return(0.5)
  
  return(mean(scores))
}

# Calculate text density
calculate_text_density <- function(extraction_result, settings = NULL) {
  return(calculate_text_coverage(extraction_result, settings))
}

# Analyze character distribution
analyze_character_distribution <- function(extraction_result) {
  if (is.null(extraction_result$text) && is.null(extraction_result$words)) {
    return(list(
      alphabetic_ratio = 0.0,
      numeric_ratio = 0.0,
      punctuation_ratio = 0.0,
      whitespace_ratio = 0.0
    ))
  }
  
  # Extract all text
  all_text <- ""
  if (!is.null(extraction_result$text)) {
    all_text <- extraction_result$text
  } else if (!is.null(extraction_result$words)) {
    all_text <- paste(sapply(extraction_result$words, function(w) w$text %||% ""), collapse = " ")
  }
  
  if (nchar(all_text) == 0) {
    return(list(
      alphabetic_ratio = 0.0,
      numeric_ratio = 0.0,
      punctuation_ratio = 0.0,
      whitespace_ratio = 0.0
    ))
  }
  
  total_chars <- nchar(all_text)
  
  list(
    alphabetic_ratio = length(gregexpr("[A-Za-z]", all_text)[[1]]) / total_chars,
    numeric_ratio = length(gregexpr("[0-9]", all_text)[[1]]) / total_chars,
    punctuation_ratio = length(gregexpr("[[:punct:]]", all_text)[[1]]) / total_chars,
    whitespace_ratio = length(gregexpr("\\s", all_text)[[1]]) / total_chars
  )
}

# Calculate spatial consistency
calculate_spatial_consistency <- function(extraction_result) {
  # Simplified spatial consistency based on line alignment
  if (is.null(extraction_result$lines) || length(extraction_result$lines) < 2) {
    return(1.0)
  }
  
  # Check vertical alignment of lines
  line_starts <- sapply(extraction_result$lines, function(line) {
    bbox <- normalize_bounding_box(line$bbox)
    bbox$x
  })
  
  if (length(line_starts) == 0) return(1.0)
  
  # Calculate coefficient of variation for line starts
  cv <- sd(line_starts, na.rm = TRUE) / mean(line_starts, na.rm = TRUE)
  
  # Convert to consistency score (lower CV = higher consistency)
  consistency <- 1 / (1 + cv)
  
  return(min(max(consistency, 0), 1))
}

# Calculate font coverage
calculate_font_coverage <- function(font_analysis, extraction_result) {
  if (is.null(font_analysis) || is.null(extraction_result$words)) {
    return(1.0)  # Assume good coverage if no font analysis
  }
  
  # Count words with font information
  words_with_font <- sum(sapply(extraction_result$words, function(w) {
    !is.null(w$font_family) || !is.null(w$font_size)
  }))
  
  total_words <- length(extraction_result$words)
  
  if (total_words == 0) return(1.0)
  
  return(words_with_font / total_words)
}

# Build confidence breakdown
build_confidence_breakdown <- function(extraction_result) {
  # Extract all confidences
  all_confidences <- c()
  
  if (!is.null(extraction_result$words)) {
    word_conf <- extract_confidence_scores(extraction_result$words)
    all_confidences <- c(all_confidences, word_conf)
  }
  
  if (!is.null(extraction_result$lines)) {
    line_conf <- extract_confidence_scores(extraction_result$lines)
    all_confidences <- c(all_confidences, line_conf)
  }
  
  if (length(all_confidences) == 0) {
    return(list(
      high_confidence_ratio = 1.0,
      medium_confidence_ratio = 0.0,
      low_confidence_ratio = 0.0
    ))
  }
  
  total_count <- length(all_confidences)
  high_count <- sum(all_confidences >= 0.8)
  medium_count <- sum(all_confidences >= 0.5 & all_confidences < 0.8)
  low_count <- sum(all_confidences < 0.5)
  
  list(
    high_confidence_ratio = high_count / total_count,
    medium_confidence_ratio = medium_count / total_count,
    low_confidence_ratio = low_count / total_count
  )
}

# Validate enhanced output structure
validate_enhanced_output <- function(enhanced_output) {
  # Ensure all required top-level fields are present
  enhanced_output <- ensure_required_fields(enhanced_output)
  
  # Validate and fix page_info section
  enhanced_output$page_info <- validate_page_info(enhanced_output$page_info)
  
  # Validate and fix extraction_details section
  enhanced_output$extraction_details <- validate_extraction_details(enhanced_output$extraction_details)
  
  # Validate and fix content_structure section
  enhanced_output$content_structure <- validate_content_structure(enhanced_output$content_structure)
  
  # Validate and fix processing_metadata section
  enhanced_output$processing_metadata <- validate_processing_metadata(enhanced_output$processing_metadata)
  
  # Validate and fix quality_assessment section
  enhanced_output$quality_assessment <- validate_quality_assessment(enhanced_output$quality_assessment)
  
  return(enhanced_output)
}

# Ensure required top-level fields are present
ensure_required_fields <- function(enhanced_output) {
  required_fields <- c("page_info", "extraction_details", "content_structure", 
                      "processing_metadata", "quality_assessment")
  
  for (field in required_fields) {
    if (is.null(enhanced_output[[field]])) {
      enhanced_output[[field]] <- list()
    }
  }
  
  return(enhanced_output)
}

# Validate page_info section per schema requirements
validate_page_info <- function(page_info) {
  if (is.null(page_info)) page_info <- list()
  
  # Required fields with defaults
  page_info$page_number <- as.integer(page_info$page_number %||% 1)
  page_info$page_width <- as.numeric(page_info$page_width %||% 0)
  page_info$page_height <- as.numeric(page_info$page_height %||% 0)
  page_info$page_area <- as.numeric(page_info$page_area %||% 0)
  
  # Optional fields with validation
  if (!is.null(page_info$text_coverage)) {
    page_info$text_coverage <- max(0, min(1, as.numeric(page_info$text_coverage)))
  }
  
  if (!is.null(page_info$rotation)) {
    page_info$rotation <- as.numeric(page_info$rotation)
  }
  
  return(page_info)
}

# Validate extraction_details section per schema requirements
validate_extraction_details <- function(extraction_details) {
  if (is.null(extraction_details)) extraction_details <- list()
  
  # Required fields with defaults
  valid_methods <- c("digital", "ocr", "mixed")
  extraction_details$extraction_method <- if (extraction_details$extraction_method %in% valid_methods) {
    extraction_details$extraction_method
  } else {
    "digital"  # Default
  }
  
  extraction_details$extraction_success <- as.logical(extraction_details$extraction_success %||% TRUE)
  
  # Ensure overall_confidence has proper structure
  if (is.null(extraction_details$overall_confidence)) {
    extraction_details$overall_confidence <- list(mean = NA_real_, count = 0L)
  } else {
    oc <- extraction_details$overall_confidence
    if (is.null(oc$mean)) oc$mean <- NA_real_
    if (is.null(oc$count)) oc$count <- 0L
    extraction_details$overall_confidence <- oc
  }
  
  # Validate fallback_methods if present
  if (!is.null(extraction_details$fallback_methods)) {
    valid_fallbacks <- c("digital", "ocr", "manual")
    extraction_details$fallback_methods <- extraction_details$fallback_methods[
      extraction_details$fallback_methods %in% valid_fallbacks
    ]
  }
  
  return(extraction_details)
}

# Validate content_structure section
validate_content_structure <- function(content_structure) {
  if (is.null(content_structure)) content_structure <- list()
  
  # Ensure arrays exist even if empty
  if (is.null(content_structure$words)) content_structure$words <- list()
  if (is.null(content_structure$lines)) content_structure$lines <- list()
  if (is.null(content_structure$columns)) content_structure$columns <- list()
  if (is.null(content_structure$semantic_blocks)) content_structure$semantic_blocks <- list()
  if (is.null(content_structure$tables)) content_structure$tables <- list()
  
  return(content_structure)
}

# Validate processing_metadata section
validate_processing_metadata <- function(processing_metadata) {
  if (is.null(processing_metadata)) processing_metadata <- list()
  
  # Required fields with defaults
  if (is.null(processing_metadata$processing_timestamp)) {
    processing_metadata$processing_timestamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  }
  
  # Validate arrays
  if (is.null(processing_metadata$errors)) processing_metadata$errors <- list()
  if (is.null(processing_metadata$warnings)) processing_metadata$warnings <- list()
  
  return(processing_metadata)
}

# Validate quality_assessment section
validate_quality_assessment <- function(quality_assessment) {
  if (is.null(quality_assessment)) quality_assessment <- list()
  
  # Ensure numeric scores are in valid range [0,1]
  score_fields <- c("overall_quality", "text_density", "spatial_consistency", "font_coverage")
  
  for (field in score_fields) {
    if (!is.null(quality_assessment[[field]])) {
      quality_assessment[[field]] <- max(0, min(1, as.numeric(quality_assessment[[field]])))
    }
  }
  
  # Validate character_distribution structure
  if (is.null(quality_assessment$character_distribution)) {
    quality_assessment$character_distribution <- list(
      alphabetic_ratio = 0.0,
      numeric_ratio = 0.0,
      punctuation_ratio = 0.0,
      whitespace_ratio = 0.0
    )
  } else {
    cd <- quality_assessment$character_distribution
    ratio_fields <- c("alphabetic_ratio", "numeric_ratio", "punctuation_ratio", "whitespace_ratio")
    for (field in ratio_fields) {
      if (!is.null(cd[[field]])) {
        cd[[field]] <- max(0, min(1, as.numeric(cd[[field]])))
      } else {
        cd[[field]] <- 0.0
      }
    }
    quality_assessment$character_distribution <- cd
  }
  
  # Validate confidence_breakdown structure
  if (is.null(quality_assessment$confidence_breakdown)) {
    quality_assessment$confidence_breakdown <- list(
      high_confidence_ratio = 1.0,
      medium_confidence_ratio = 0.0,
      low_confidence_ratio = 0.0
    )
  } else {
    cb <- quality_assessment$confidence_breakdown
    ratio_fields <- c("high_confidence_ratio", "medium_confidence_ratio", "low_confidence_ratio")
    for (field in ratio_fields) {
      if (!is.null(cb[[field]])) {
        cb[[field]] <- max(0, min(1, as.numeric(cb[[field]])))
      }
    }
    quality_assessment$confidence_breakdown <- cb
  }
  
  return(quality_assessment)
}

# Additional helper functions for font analysis
calculate_font_statistics <- function(text_data, font_stats_data = NULL) {
  # Use font_stats_data if available
  if (!is.null(font_stats_data)) {
    return(list(
      unique_fonts = as.integer(font_stats_data$unique_fonts %||% 0L),
      unique_sizes = as.integer(font_stats_data$unique_sizes %||% 0L),
      dominant_font = as.character(font_stats_data$dominant_font %||% ""),
      dominant_size = as.numeric(font_stats_data$dominant_size %||% 0)
    ))
  }
  
  if (is.null(text_data) || (!is.data.frame(text_data) && !is.list(text_data))) {
    return(list(unique_fonts = 0L, unique_sizes = 0L, dominant_font = "", dominant_size = 0))
  }
  
  # Extract font information from data frame or list
  fonts <- character(0)
  sizes <- numeric(0)
  
  if (is.data.frame(text_data)) {
    if ("font_family" %in% names(text_data)) {
      fonts <- unique(text_data$font_family[!is.na(text_data$font_family)])
    }
    if ("font_size" %in% names(text_data)) {
      sizes <- unique(text_data$font_size[!is.na(text_data$font_size)])
    }
  }
  
  # Calculate dominant values
  dominant_font <- ""
  dominant_size <- 0
  
  if (length(fonts) > 0 && is.data.frame(text_data) && "font_family" %in% names(text_data)) {
    font_counts <- table(text_data$font_family)
    dominant_font <- names(sort(font_counts, decreasing = TRUE))[1] %||% ""
  }
  
  if (length(sizes) > 0 && is.data.frame(text_data) && "font_size" %in% names(text_data)) {
    size_counts <- table(text_data$font_size)
    dominant_size <- as.numeric(names(sort(size_counts, decreasing = TRUE))[1] %||% 0)
  }
  
  list(
    unique_fonts = length(fonts),
    unique_sizes = length(sizes),
    dominant_font = dominant_font,
    dominant_size = dominant_size
  )
}

# Build detailed font information
build_font_details <- function(text_data, font_stats_data = NULL) {
  if (is.null(text_data) && is.null(font_stats_data)) {
    return(list())
  }
  
  # Use font_stats_data if available
  if (!is.null(font_stats_data) && !is.null(font_stats_data$font_details)) {
    return(font_stats_data$font_details)
  }
  
  if (!is.data.frame(text_data)) {
    return(list())
  }
  
  # Check required columns exist
  required_cols <- c("font_family", "font_size")
  if (!all(required_cols %in% names(text_data))) {
    return(list())
  }
  
  # Remove rows with missing font information
  valid_rows <- complete.cases(text_data[required_cols])
  if (sum(valid_rows) == 0) {
    return(list())
  }
  
  clean_data <- text_data[valid_rows, ]
  
  # Add font_style if missing
  if (!"font_style" %in% names(clean_data)) {
    clean_data$font_style <- "normal"
  }
  
  # Group by font characteristics using base R to avoid dplyr dependency issues
  font_combinations <- unique(clean_data[c("font_family", "font_size", "font_style")])
  
  font_details <- list()
  total_count <- nrow(clean_data)
  
  for (i in seq_len(nrow(font_combinations))) {
    combo <- font_combinations[i, ]
    
    # Count matching rows
    matches <- (clean_data$font_family == combo$font_family) & 
               (clean_data$font_size == combo$font_size) & 
               (clean_data$font_style == combo$font_style)
    count <- sum(matches)
    
    font_details[[i]] <- list(
      font_family = as.character(combo$font_family),
      font_size = as.numeric(combo$font_size),
      font_style = normalize_font_style(combo$font_style),
      count = as.integer(count),
      coverage = as.numeric(count / total_count)
    )
  }
  
  return(font_details)
}

# Calculate size distribution
calculate_size_distribution <- function(text_data, font_stats_data = NULL) {
  # Use font_stats_data if available
  if (!is.null(font_stats_data) && !is.null(font_stats_data$size_distribution)) {
    return(font_stats_data$size_distribution)
  }
  
  if (is.null(text_data) || !is.data.frame(text_data) || !"font_size" %in% names(text_data)) {
    return(list())
  }
  
  # Remove NA values
  sizes <- text_data$font_size[!is.na(text_data$font_size)]
  
  if (length(sizes) == 0) {
    return(list())
  }
  
  size_counts <- table(sizes)
  total_count <- sum(size_counts)
  
  size_dist <- list()
  for (size in names(size_counts)) {
    size_dist[[size]] <- list(
      count = as.integer(size_counts[[size]]),
      coverage = as.numeric(size_counts[[size]] / total_count)
    )
  }
  
  return(size_dist)
}

# Build column structures with detailed metadata
build_column_structures <- function(column_detection, page_height = NULL) {
  if (is.null(column_detection) || is.null(column_detection$columns) || length(column_detection$columns) == 0) {
    return(list())
  }
  
  columns <- column_detection$columns
  page_height_val <- page_height %||% 792  # Default page height
  
  column_structures <- lapply(seq_along(columns), function(i) {
    col <- columns[[i]]
    
    # Calculate bounding box
    x_start <- if (is.finite(col$x_start)) col$x_start else 0
    x_end <- if (is.finite(col$x_end)) col$x_end else 612  # Default page width
    
    structure <- list(
      column_id = as.integer(col$column_id %||% i),
      bounding_box = list(
        x = x_start,
        y = 0,
        width = x_end - x_start,
        height = page_height_val
      ),
      column_width = x_end - x_start
    )
    
    # Add optional fields if available
    if (!is.null(column_detection$metadata$alignment)) {
      structure$text_alignment <- column_detection$metadata$alignment %||% "left"
    }
    
    # Add line IDs if available
    if (!is.null(col$line_ids)) {
      structure$line_ids <- as.integer(col$line_ids)
    }
    
    structure
  })
  
  return(column_structures)
}

# Build semantic block structures with schema-compliant classifications
build_semantic_block_structures <- function(semantic_analysis) {
  if (is.null(semantic_analysis) || is.null(semantic_analysis$semantic_blocks) || length(semantic_analysis$semantic_blocks) == 0) {
    return(list())
  }
  
  semantic_blocks <- semantic_analysis$semantic_blocks
  
  # Mapping from analysis block types to schema enums
  block_type_mapping <- list(
    "header" = "heading",
    "department" = "heading", 
    "course_code" = "code",
    "body_text" = "body",
    "page_number" = "footer",
    "title" = "title",
    "heading" = "heading",
    "body" = "body",
    "table" = "table",
    "caption" = "caption",
    "footnote" = "footnote",
    "footer" = "footer",
    "list" = "list",
    "code" = "code"
  )
  
  block_structures <- lapply(seq_along(semantic_blocks), function(i) {
    block <- semantic_blocks[[i]]
    
    # Map block type to schema enum
    original_type <- block$block_type %||% "body_text"
    mapped_type <- block_type_mapping[[original_type]] %||% "body"
    
    # Special case: if page_number appears at bottom, classify as footer
    if (original_type == "page_number" && !is.null(block$position) && !is.null(block$position$y)) {
      # If y position is in bottom third of page, classify as footer
      if (block$position$y > 500) {  # Rough bottom third for typical page
        mapped_type <- "footer"
      }
    }
    
    structure <- list(
      block_id = as.integer(i),
      classification = mapped_type,
      bounding_box = normalize_bounding_box(block$position %||% block$bounding_box),
      confidence = as.numeric(block$classification_confidence %||% 0.5)
    )
    
    # Add optional fields
    if (!is.null(block$text)) {
      structure$text <- as.character(block$text)
    }
    
    if (!is.null(block$line_ids)) {
      structure$line_ids <- as.integer(block$line_ids)
    }
    
    structure
  })
  
  return(block_structures)
}

# Build table structures with rows/columns/cells from table detection
build_table_structures <- function(table_detection) {
  if (is.null(table_detection) || is.null(table_detection$tables) || length(table_detection$tables) == 0) {
    return(list())
  }
  
  tables <- table_detection$tables
  
  table_structures <- lapply(seq_along(tables), function(i) {
    table <- tables[[i]]
    
    # Extract basic table info
    table_id <- as.integer(i)
    confidence <- as.numeric(table$confidence %||% 0.5)
    
    # Calculate rows and columns counts
    rows_count <- 0L
    columns_count <- 0L
    cells <- list()
    
    if (!is.null(table$rows) && length(table$rows) > 0) {
      rows_count <- length(table$rows)
      
      # Process each row to extract cells
      for (row_idx in seq_along(table$rows)) {
        row_data <- table$rows[[row_idx]]
        
        if (!is.null(row_data$cells) && length(row_data$cells) > 0) {
          columns_count <- max(columns_count, length(row_data$cells))
          
          for (col_idx in seq_along(row_data$cells)) {
            cell_data <- row_data$cells[[col_idx]]
            
            cell <- list(
              row = as.integer(row_idx),
              column = as.integer(col_idx),
              bounding_box = normalize_bounding_box(cell_data$bounding_box %||% cell_data$bbox),
              rowspan = as.integer(cell_data$rowspan %||% 1),
              colspan = as.integer(cell_data$colspan %||% 1)
            )
            
            # Add text if available
            if (!is.null(cell_data$text)) {
              cell$text <- as.character(cell_data$text)
            }
            
            cells[[length(cells) + 1]] <- cell
          }
        }
      }
    } else if (!is.null(table$columns_detected)) {
      # Fallback: use columns_detected if available
      columns_count <- as.integer(table$columns_detected)
      rows_count <- 1L  # Minimum
    }
    
    structure <- list(
      table_id = table_id,
      bounding_box = normalize_bounding_box(table$bounding_box %||% list(x = 0, y = 0, width = 0, height = 0)),
      rows = as.integer(max(rows_count, 1)),
      columns = as.integer(max(columns_count, 1)),
      confidence = confidence
    )
    
    # Add cells if we have any
    if (length(cells) > 0) {
      structure$cells <- cells
    }
    
    structure
  })
  
  return(table_structures)
}
