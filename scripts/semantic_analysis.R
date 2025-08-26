#!/usr/bin/env Rscript

# Semantic analysis module for text block classification
# Combines font analysis with regex patterns to identify document structure

# Load configuration settings
load_settings <- function() {
  settings_path <- file.path("config", "settings.yml")
  if (!file.exists(settings_path)) {
    warning("Settings file not found, using defaults")
    return(list())
  }
  
  # Use proper YAML parsing
  if (!require("yaml", quietly = TRUE)) {
    warning("yaml package not available, falling back to defaults")
    settings <- list()
  } else {
    settings <- tryCatch({
      yaml::yaml.load_file(settings_path)
    }, error = function(e) {
      warning(sprintf("Failed to parse YAML settings: %s", e$message))
      list()
    })
  }
  
  # Set defaults if not found
  if (is.null(settings$regex_hints)) {
    settings$regex_hints <- list(
      department_header = "^[A-Z&/\\-\\s]{3,}\\s+\\([A-Z]{2,4}\\)$",
      course_header = "^[A-Z]{2,4}\\s+\\d+[A-Z]?\\.?\\s+\\(.+?\\)",
      units_inline = "\\((\\d+(?:-\\d+)?)\\s+units?\\)",
      page_number = "^\\d+$",
      units_pattern = "\\((\\d+(?:-\\d+)?)\\s+units?\\)",
      all_caps_header = "^[A-Z\\s&/\\-]{3,}$"
    )
  }
  
  if (is.null(settings$text_processing)) {
    settings$text_processing <- list(
      ocr_confidence_threshold = 0.6,
      font_size_threshold_ratio = 1.2,
      enable_semantic_analysis = TRUE
    )
  }
  
  if (is.null(settings$semantic_classification)) {
    settings$semantic_classification <- list(
      header_font_size_multiplier = 1.5,
      confidence_weights = list(font = 0.6, regex = 0.4),
      min_classification_confidence = 0.7
    )
  }
  
  settings
}

# Calculate font statistics for a page to establish baselines
calculate_font_statistics <- function(text_df, font_stats = NULL) {
  if (is.null(text_df) || nrow(text_df) == 0) {
    return(list(
      median_size = 12,
      min_size = 10,
      max_size = 14,
      size_range = 4,
      dominant_size = 12,
      has_variation = FALSE
    ))
  }
  
  # Use existing font stats if available
  if (!is.null(font_stats) && !is.null(font_stats$size_distribution)) {
    return(list(
      median_size = font_stats$size_distribution$median %||% 12,
      min_size = font_stats$size_distribution$min %||% 10,
      max_size = font_stats$size_distribution$max %||% 14,
      size_range = (font_stats$size_distribution$max %||% 14) - (font_stats$size_distribution$min %||% 10),
      dominant_size = font_stats$size_distribution$median %||% 12,
      has_variation = font_stats$has_font_variation %||% FALSE
    ))
  }
  
  # Calculate from text_df if font stats not available
  font_sizes <- numeric(0)
  if ("size" %in% names(text_df)) {
    font_sizes <- text_df$size[is.finite(text_df$size) & !is.na(text_df$size)]
  }
  
  if (length(font_sizes) > 0) {
    median_size <- stats::median(font_sizes)
    min_size <- min(font_sizes)
    max_size <- max(font_sizes)
    size_range <- max_size - min_size
    dominant_size <- as.numeric(names(sort(table(font_sizes), decreasing = TRUE))[1])
    has_variation <- length(unique(font_sizes)) > 1
  } else {
    # Default values when no font information is available
    median_size <- 12
    min_size <- 10
    max_size <- 14
    size_range <- 4
    dominant_size <- 12
    has_variation <- FALSE
  }
  
  list(
    median_size = median_size,
    min_size = min_size,
    max_size = max_size,
    size_range = size_range,
    dominant_size = dominant_size,
    has_variation = has_variation
  )
}

# Detect semantic elements in text content
detect_semantic_elements <- function(text_content, settings) {
  if (is.null(text_content) || nchar(trimws(text_content)) == 0) {
    return(list(
      is_course_code = FALSE,
      is_department = FALSE,
      is_page_number = FALSE,
      is_units = FALSE,
      is_all_caps = FALSE,
      extracted_units = NA,
      confidence = 0.0
    ))
  }
  
  text <- trimws(text_content)
  regex_hints <- settings$regex_hints %||% list()
  
  # Check various patterns
  is_course_code <- FALSE
  is_department <- FALSE
  is_page_number <- FALSE
  is_units <- FALSE
  is_all_caps <- FALSE
  extracted_units <- NA
  
  # Course code pattern
  if (!is.null(regex_hints$course_header)) {
    is_course_code <- grepl(regex_hints$course_header, text, perl = TRUE)
  }
  
  # Department header pattern
  if (!is.null(regex_hints$department_header)) {
    is_department <- grepl(regex_hints$department_header, text, perl = TRUE)
  }
  
  # Page number pattern
  if (!is.null(regex_hints$page_number)) {
    is_page_number <- grepl(regex_hints$page_number, text, perl = TRUE)
  }
  
  # Units pattern
  if (!is.null(regex_hints$units_pattern)) {
    units_match <- regmatches(text, regexpr(regex_hints$units_pattern, text, perl = TRUE))
    if (length(units_match) > 0) {
      is_units <- TRUE
      extracted_units <- gsub("\\D", "", units_match[1])
      extracted_units <- if (nchar(extracted_units) > 0) as.numeric(extracted_units) else NA
    }
  }
  
  # All caps header pattern
  if (!is.null(regex_hints$all_caps_header)) {
    is_all_caps <- grepl(regex_hints$all_caps_header, text, perl = TRUE)
  }
  
  # Calculate overall confidence based on pattern matches
  confidence <- 0.0
  if (is_course_code) confidence <- confidence + 0.9
  if (is_department) confidence <- confidence + 0.8
  if (is_page_number) confidence <- confidence + 0.7
  if (is_units) confidence <- confidence + 0.6
  if (is_all_caps) confidence <- confidence + 0.3
  
  # Normalize confidence to 0-1 range
  confidence <- min(1.0, confidence)
  
  list(
    is_course_code = is_course_code,
    is_department = is_department,
    is_page_number = is_page_number,
    is_units = is_units,
    is_all_caps = is_all_caps,
    extracted_units = extracted_units,
    confidence = confidence
  )
}

# Classify individual text blocks based on font and content analysis
classify_text_blocks <- function(words_df, font_stats = NULL, page_dimensions = NULL) {
  if (is.null(words_df) || nrow(words_df) == 0) {
    return(list())
  }
  
  settings <- load_settings()
  font_analysis <- calculate_font_statistics(words_df, font_stats)
  
  # Initialize classification results
  classifications <- list()
  
  for (i in seq_len(nrow(words_df))) {
    word_text <- words_df$text[i]
    
    # Analyze semantic elements
    semantic_info <- detect_semantic_elements(word_text, settings)
    
    # Font-based analysis
    font_confidence <- 0.5  # Default neutral confidence
    font_size_signal <- 0.0
    
    if ("relative_size" %in% names(words_df)) {
      relative_size <- words_df$relative_size[i]
      if (!is.na(relative_size)) {
        font_size_signal <- relative_size
        if (relative_size > (settings$semantic_classification$header_font_size_multiplier %||% 1.5)) {
          font_confidence <- 0.8  # High confidence for large fonts
        } else if (relative_size < 0.8) {
          font_confidence <- 0.3  # Low confidence for small fonts
        }
      }
    }
    
    # OCR confidence analysis
    ocr_confidence <- 1.0  # Default for digital text
    if ("confidence" %in% names(words_df)) {
      ocr_conf_raw <- words_df$confidence[i]
      if (!is.na(ocr_conf_raw)) {
        # Normalize to 0-1 if in 0-100 range
        ocr_confidence <- if (ocr_conf_raw > 1.0) ocr_conf_raw / 100.0 else ocr_conf_raw
        ocr_confidence <- max(0.0, min(1.0, ocr_confidence))  # Clamp to [0,1]
      }
    }
    
    # Determine block type based on combined analysis
    block_type <- "body_text"
    classification_confidence <- 0.5
    
    if (semantic_info$is_page_number) {
      block_type <- "page_number"
      classification_confidence <- semantic_info$confidence * ocr_confidence
    } else if (semantic_info$is_course_code) {
      block_type <- "course_code"
      classification_confidence <- semantic_info$confidence * ocr_confidence
    } else if (semantic_info$is_department) {
      block_type <- "department"
      classification_confidence <- semantic_info$confidence * ocr_confidence
    } else if (font_size_signal > (settings$semantic_classification$header_font_size_multiplier %||% 1.5)) {
      block_type <- "header"
      # Combine font, semantic, and OCR signals with NULL checks
      confidence_weights <- settings$semantic_classification$confidence_weights
      font_weight <- if (!is.null(confidence_weights) && !is.null(confidence_weights$font)) confidence_weights$font else 0.6
      regex_weight <- if (!is.null(confidence_weights) && !is.null(confidence_weights$regex)) confidence_weights$regex else 0.4
      # Include OCR confidence in weighted calculation
      base_confidence <- (font_confidence * font_weight) + (semantic_info$confidence * regex_weight)
      classification_confidence <- base_confidence * ocr_confidence
    } else if (semantic_info$is_all_caps && nchar(trimws(word_text)) > 5) {
      block_type <- "header"
      # Include OCR confidence in all caps header detection
      base_confidence <- min(0.7, semantic_info$confidence + 0.2)
      classification_confidence <- base_confidence * ocr_confidence
    } else {
      block_type <- "body_text"
      # Include OCR confidence for body text
      classification_confidence <- 0.6 * ocr_confidence
    }
    
    # Create classification result
    classification <- list(
      block_id = sprintf("block_%04d", i),
      block_type = block_type,
      classification_confidence = classification_confidence,
      text_content = word_text,
      font_characteristics = list(
        relative_size = if ("relative_size" %in% names(words_df)) words_df$relative_size[i] else 1.0,
        font_style = if ("font_style" %in% names(words_df)) words_df$font_style[i] else "normal",
        is_emphasis = if ("is_emphasis" %in% names(words_df)) words_df$is_emphasis[i] else FALSE
      ),
      semantic_signals = semantic_info,
      position = list(
        x = words_df$x[i],
        y = words_df$y[i],
        width = words_df$width[i],
        height = words_df$height[i]
      )
    )
    
    classifications[[length(classifications) + 1]] <- classification
  }
  
  classifications
}

# Main function to analyze and classify text blocks on a page
analyze_page_semantics <- function(words_df, font_stats = NULL, page_dimensions = NULL) {
  if (is.null(words_df) || nrow(words_df) == 0) {
    return(list(
      semantic_blocks = list(),
      font_analysis = list(
        font_sizes = numeric(0),
        dominant_fonts = character(0),
        size_distribution = list(median = NA, min = NA, max = NA),
        has_font_variation = FALSE
      ),
      processing_metadata = list(
        semantic_analysis_enabled = TRUE,
        classification_method = "font_plus_regex",
        total_blocks = 0,
        blocks_by_type = list()
      )
    ))
  }
  
  # Classify text blocks
  semantic_blocks <- classify_text_blocks(words_df, font_stats, page_dimensions)
  
  # Calculate processing statistics
  total_blocks <- length(semantic_blocks)
  blocks_by_type <- list()
  
  if (total_blocks > 0) {
    block_types <- vapply(semantic_blocks, function(b) b$block_type, character(1))
    type_counts <- table(block_types)
    blocks_by_type <- as.list(type_counts)
  }
  
  list(
    semantic_blocks = semantic_blocks,
    font_analysis = font_stats %||% list(
      font_sizes = numeric(0),
      dominant_fonts = character(0),
      size_distribution = list(median = NA, min = NA, max = NA),
      has_font_variation = FALSE
    ),
    processing_metadata = list(
      semantic_analysis_enabled = TRUE,
      classification_method = "font_plus_regex",
      total_blocks = total_blocks,
      blocks_by_type = blocks_by_type
    )
  )
}
