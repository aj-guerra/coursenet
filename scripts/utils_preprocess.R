#!/usr/bin/env Rscript

# Shared utilities for PDF preprocessing scripts

# Configure R library path and ensure required packages are installed
setup_r_environment <- function() {
  lib_dir <- Sys.getenv("R_LIBS_USER", unset = "/workspace/.Rlibs")
  dir.create(lib_dir, recursive = TRUE, showWarnings = FALSE)
  .libPaths(c(lib_dir, .libPaths()))
  Sys.setenv(R_LIBS_USER = lib_dir)

  required_packages <- c(
    "jsonlite",
    "pdftools",
    "digest",
    "tesseract",
    "cld2",
    "stringr",
    "yaml"
  )
  for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      install.packages(pkg, repos = "https://cloud.r-project.org", lib = lib_dir)
      library(pkg, character.only = TRUE)
    }
  }
}

# Simple key-value argument parser supporting --key value and --flag
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

`%||%` <- function(a, b) if (!is.null(a)) a else b

# Ensure interim directories exist and return paths
setup_directories <- function(year) {
  dir_interim <- file.path("data", "interim", sprintf("year=%d", as.integer(year)))
  dir_pages <- file.path(dir_interim, "pages")
  dir.create(dir_pages, recursive = TRUE, showWarnings = FALSE)
  list(dir_interim = dir_interim, dir_pages = dir_pages)
}

# Compute file checksum (sha256)
file_checksum <- function(file_path) {
  digest::digest(file_path, algo = "sha256", file = TRUE)
}



# Calculate overall confidence metrics from OCR data (expects a data.frame with 'confidence' in 0-100 or 0-1)
calculate_text_confidence <- function(ocr_df, low_threshold = 0.6) {
  if (is.null(ocr_df) || nrow(ocr_df) == 0 || !("confidence" %in% names(ocr_df))) {
    return(list(
      mean_confidence = NA_real_,
      confidence_distribution = list(
        min = NA_real_, p25 = NA_real_, median = NA_real_, p75 = NA_real_, max = NA_real_,
        count = 0L
      ),
      low_confidence_count = 0L
    ))
  }
  conf <- ocr_df$confidence
  # Normalize to 0-1 if values appear to be in 0-100
  if (max(conf, na.rm = TRUE) > 1.0) conf <- conf / 100.0
  conf <- conf[is.finite(conf) & !is.na(conf)]
  if (length(conf) == 0) {
    return(list(
      mean_confidence = NA_real_,
      confidence_distribution = list(
        min = NA_real_, p25 = NA_real_, median = NA_real_, p75 = NA_real_, max = NA_real_,
        count = 0L
      ),
      low_confidence_count = 0L
    ))
  }
  q <- stats::quantile(conf, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE, names = FALSE)
  list(
    mean_confidence = mean(conf, na.rm = TRUE),
    confidence_distribution = list(min = q[[1]], p25 = q[[2]], median = q[[3]], p75 = q[[4]], max = q[[5]], count = length(conf)),
    low_confidence_count = as.integer(sum(conf < low_threshold, na.rm = TRUE))
  )
}

# Compute page-level metrics based on text elements and dimensions
calculate_page_metrics <- function(text_df, page_width, page_height) {
  words_count <- if (!is.null(text_df) && nrow(text_df) > 0) nrow(text_df) else 0L
  chars_count <- if (!is.null(text_df) && nrow(text_df) > 0 && "text" %in% names(text_df)) sum(nchar(text_df$text), na.rm = TRUE) else 0L
  area <- as.numeric(page_width) * as.numeric(page_height)
  char_density <- if (is.finite(area) && area > 0) chars_count / area else NA_real_
  list(words_count = as.integer(words_count), chars_count = as.integer(chars_count), char_density = char_density)
}

# Assess extraction quality using density, character distribution and (if available) confidence
assess_extraction_quality <- function(text_df, page_width, page_height, method = c("digital", "ocr"), confidence_metrics = NULL) {
  method <- match.arg(method)
  metrics <- calculate_page_metrics(text_df, page_width, page_height)

  # Character composition
  all_text <- if (!is.null(text_df) && "text" %in% names(text_df)) paste(text_df$text, collapse = "") else ""
  n_total <- nchar(all_text)
  n_letters <- stringr::str_count(all_text, "[A-Za-z]")
  n_digits <- stringr::str_count(all_text, "[0-9]")
  n_space <- stringr::str_count(all_text, "\u0020")
  denom <- max(1L, n_total)
  composition <- list(
    letters_ratio = as.numeric(n_letters / denom),
    digits_ratio = as.numeric(n_digits / denom),
    whitespace_ratio = as.numeric(n_space / denom)
  )

  # Spatial consistency proxy: variance of element widths normalized by page width
  spatial_consistency <- NA_real_
  if (!is.null(text_df) && nrow(text_df) > 1 && all(c("width") %in% names(text_df))) {
    w <- text_df$width[is.finite(text_df$width)]
    if (length(w) > 1 && is.finite(page_width) && page_width > 0) {
      spatial_consistency <- 1.0 - min(1.0, stats::var(w, na.rm = TRUE) / (page_width ^ 2))
    }
  }

  # Quality score components
  density_score <- if (is.finite(metrics$char_density)) max(0.0, min(1.0, metrics$char_density / 0.02)) else NA_real_
  conf_score <- NA_real_
  if (identical(method, "ocr") && !is.null(confidence_metrics) && !is.na(confidence_metrics$mean_confidence)) {
    conf_score <- as.numeric(confidence_metrics$mean_confidence)
  } else if (identical(method, "digital")) {
    conf_score <- 1.0
  }
  # Aggregate score with simple averaging where available
  score_components <- c(
    density = density_score,
    spatial = spatial_consistency,
    confidence = conf_score
  )
  score_components <- score_components[is.finite(score_components) & !is.na(score_components)]
  overall_quality <- if (length(score_components) > 0) mean(score_components) else NA_real_

  list(
    text_density = metrics$char_density,
    character_distribution = composition,
    spatial_consistency = spatial_consistency,
    overall_quality = overall_quality
  )
}

# Enhanced text processing function that replaces chars_to_words with automatic detection
tokens_to_words <- function(text_df, min_confidence = 0.6, method = NULL) {
  if (is.null(text_df) || nrow(text_df) == 0) return(data.frame())
  
  # Determine input type based on explicit method or detection
  if (!is.null(method)) {
    is_ocr_data <- (method == "ocr" || grepl("ocr", method, ignore.case = TRUE))
  } else {
    # Fallback to confidence column detection
    is_ocr_data <- "confidence" %in% names(text_df)
  }
  
  if (is_ocr_data) {
    # OCR data (word-level tokens) - filter by confidence and retain confidence column
    filtered_df <- filter_low_confidence_tokens(text_df, min_confidence)
    # Keep confidence column if it exists
    keep_cols <- c("text", "x", "y", "width", "height")
    if ("confidence" %in% names(filtered_df)) {
      keep_cols <- c(keep_cols, "confidence")
    }
    return(filtered_df[, keep_cols, drop = FALSE])
  } else {
    # Digital data (character-level) - use existing aggregation logic
    return(chars_to_words_internal(text_df))
  }
}

# Filter OCR tokens below confidence threshold
filter_low_confidence_tokens <- function(ocr_df, threshold = 0.6) {
  if (is.null(ocr_df) || nrow(ocr_df) == 0 || !("confidence" %in% names(ocr_df))) {
    return(ocr_df)
  }
  
  # Normalize confidence to 0-1 if in 0-100 range
  conf <- ocr_df$confidence
  if (max(conf, na.rm = TRUE) > 1.0) conf <- conf / 100.0
  
  # Filter tokens above threshold
  valid_tokens <- is.finite(conf) & !is.na(conf) & conf >= threshold
  ocr_df[valid_tokens, , drop = FALSE]
}

# Internal character aggregation function (original chars_to_words logic)
chars_to_words_internal <- function(d) {
  if (nrow(d) == 0) return(d)
  d <- d[order(-d$y, d$x), , drop = FALSE]
  words <- list()
  current <- NULL
  flush_word <- function() {
    if (!is.null(current) && nchar(current$text) > 0) {
      words[[length(words) + 1]] <<- current
    }
    current <<- NULL
  }
  for (i in seq_len(nrow(d))) {
    ch <- d[i, ]
    if (is.null(current)) {
      current <- list(text = ch$text, x = ch$x, y = ch$y, width = ch$width, height = ch$height)
    } else {
      gap <- ch$x - (current$x + current$width)
      if (isTRUE(ch$space) || gap > max(6, 1.0 * ch$height)) {
        flush_word()
        current <- list(text = ch$text, x = ch$x, y = ch$y, width = ch$width, height = ch$height)
      } else {
        current$text <- paste0(current$text, ch$text)
        current$width <- (ch$x + ch$width) - current$x
        current$height <- max(current$height, ch$height)
        current$y <- max(current$y, ch$y)
      }
    }
  }
  flush_word()
  if (length(words) == 0) return(data.frame())
  data.frame(
    text = vapply(words, function(w) w$text, character(1)),
    x = vapply(words, function(w) w$x, numeric(1)),
    y = vapply(words, function(w) w$y, numeric(1)),
    width = vapply(words, function(w) w$width, numeric(1)),
    height = vapply(words, function(w) w$height, numeric(1)),
    stringsAsFactors = FALSE
  )
}

# Enhanced column detection with multiple fallback methods
detect_columns_enhanced <- function(words_df, page_dimensions = NULL, method = "digital", settings = NULL) {
  if (is.null(words_df) || nrow(words_df) == 0) {
    return(list(
      columns = list(list(column_id = 1L, x_start = -Inf, x_end = Inf)),
      metadata = list(
        method_used = "none",
        detection_confidence = 0.0,
        fallback_triggered = FALSE,
        attempts = 1L
      )
    ))
  }
  
  # Extract settings or use defaults
  min_gap_ratio <- if (!is.null(settings) && !is.null(settings$column_detection$min_gap_ratio)) settings$column_detection$min_gap_ratio else 0.1
  density_smoothing <- if (!is.null(settings) && !is.null(settings$column_detection$density_smoothing)) settings$column_detection$density_smoothing else 0.8
  kmeans_enabled <- if (!is.null(settings) && !is.null(settings$column_detection$kmeans_enabled)) settings$column_detection$kmeans_enabled else TRUE
  force_two_columns_threshold <- if (!is.null(settings) && !is.null(settings$column_detection$force_two_columns_threshold)) settings$column_detection$force_two_columns_threshold else 0.3
  force_kmeans <- isTRUE(settings$column_detection$force_kmeans)
  max_fallback_attempts <- if (!is.null(settings) && !is.null(settings$quality_thresholds$max_fallback_attempts)) settings$quality_thresholds$max_fallback_attempts else 2
  
  page_width <- if (!is.null(page_dimensions) && !is.null(page_dimensions$width)) page_dimensions$width else max(words_df$x + words_df$width, na.rm = TRUE)
  
  # Check if text density exceeds force_two_columns_threshold
  text_density <- if (!is.null(words_df) && nrow(words_df) > 0) {
    total_text_area <- sum(words_df$width * words_df$height, na.rm = TRUE)
    page_area <- page_width * (if (!is.null(page_dimensions) && !is.null(page_dimensions$height)) page_dimensions$height else max(words_df$y + words_df$height, na.rm = TRUE))
    total_text_area / page_area
  } else 0
  
  # If force_kmeans is TRUE, run k-means immediately and return
  if (force_kmeans) {
    result <- try(detect_columns_kmeans(words_df, page_width), silent = TRUE)
    if (!inherits(result, "try-error") && !is.null(result)) {
      return(list(
        columns = result$columns,
        metadata = list(
          method_used = "kmeans",
          detection_confidence = result$confidence,
          fallback_triggered = FALSE,
          attempts = 1L
        )
      ))
    }
  }
  
  # Force two columns if text density exceeds threshold
  if (text_density > force_two_columns_threshold) {
    result <- try(detect_columns_kmeans(words_df, page_width), silent = TRUE)
    if (!inherits(result, "try-error") && !is.null(result) && length(result$columns) == 2) {
      return(list(
        columns = result$columns,
        metadata = list(
          method_used = "kmeans_forced_two_columns",
          detection_confidence = result$confidence,
          fallback_triggered = FALSE,
          attempts = 1L
        )
      ))
    }
  }
  
  attempts <- 1L
  fallback_triggered <- FALSE
  
  # Read configurable threshold
  min_conf <- settings$quality_thresholds$min_column_detection_confidence %||% 0.5
  
  # Method 1: Density-based detection (existing method)
  result <- try(detect_columns_density(words_df, page_width, min_gap_ratio, density_smoothing), silent = TRUE)
  if (!inherits(result, "try-error") && !is.null(result) && result$confidence >= min_conf) {
    return(list(
      columns = result$columns,
      metadata = list(
        method_used = "density",
        detection_confidence = result$confidence,
        fallback_triggered = fallback_triggered,
        attempts = attempts
      )
    ))
  }
  
  attempts <- attempts + 1L
  fallback_triggered <- TRUE
  
  # Method 2: Vertical gaps detection for sparse data
  if (attempts <= max_fallback_attempts) {
    result <- try(detect_vertical_gaps(words_df, page_width, min_gap_ratio), silent = TRUE)
    if (!inherits(result, "try-error") && !is.null(result) && result$confidence >= min_conf) {
      return(list(
        columns = result$columns,
        metadata = list(
          method_used = "vertical_gaps",
          detection_confidence = result$confidence,
          fallback_triggered = fallback_triggered,
          attempts = attempts
        )
      ))
    }
  }
  
  attempts <- attempts + 1L
  
  # Method 3: K-means clustering fallback
  if (kmeans_enabled && attempts <= max_fallback_attempts) {
    result <- try(detect_columns_kmeans(words_df, page_width), silent = TRUE)
    if (!inherits(result, "try-error") && !is.null(result)) {
      return(list(
        columns = result$columns,
        metadata = list(
          method_used = "kmeans",
          detection_confidence = result$confidence,
          fallback_triggered = fallback_triggered,
          attempts = attempts
        )
      ))
    }
  }
  
  # Final fallback: single column
  return(list(
    columns = list(list(column_id = 1L, x_start = -Inf, x_end = Inf)),
    metadata = list(
      method_used = "single_column_fallback",
      detection_confidence = 0.1,
      fallback_triggered = TRUE,
      attempts = attempts
    )
  ))
}

# Density-based column detection (refined existing method)
detect_columns_density <- function(words_df, page_width, min_gap_ratio = 0.1, smoothing = 0.8) {
  centers <- words_df$x + words_df$width / 2
  dens <- density(centers, na.rm = TRUE, adjust = smoothing)
  chg <- diff(sign(diff(dens$y)))
  valleys <- which(chg == 2)
  
  if (length(valleys) == 0) {
    return(list(columns = list(list(column_id = 1L, x_start = -Inf, x_end = Inf)), confidence = 0.2))
  }
  
  splits <- dens$x[valleys]
  split_x <- splits[which.min(dens$y[valleys])]
  left_ratio <- mean(centers <= split_x)
  right_ratio <- mean(centers > split_x)
  sep <- min(abs(split_x - min(centers)), abs(max(centers) - split_x)) / page_width
  
  if (left_ratio > 0.2 && right_ratio > 0.2 && sep > min_gap_ratio) {
    confidence <- min(1.0, (min(left_ratio, right_ratio) * 2) + (sep / min_gap_ratio))
    return(list(
      columns = list(
        list(column_id = 1L, x_start = -Inf, x_end = split_x),
        list(column_id = 2L, x_start = split_x, x_end = Inf)
      ),
      confidence = confidence
    ))
  }
  
  return(list(columns = list(list(column_id = 1L, x_start = -Inf, x_end = Inf)), confidence = 0.2))
}

# Vertical whitespace gap analysis for sparse OCR data
detect_vertical_gaps <- function(words_df, page_width, min_gap_ratio = 0.1) {
  if (nrow(words_df) < 4) {
    return(list(columns = list(list(column_id = 1L, x_start = -Inf, x_end = Inf)), confidence = 0.1))
  }
  
  # Find vertical gaps by analyzing x-position clusters
  centers <- words_df$x + words_df$width / 2
  sorted_centers <- sort(unique(centers))
  
  if (length(sorted_centers) < 2) {
    return(list(columns = list(list(column_id = 1L, x_start = -Inf, x_end = Inf)), confidence = 0.1))
  }
  
  # Find largest gap between text elements
  gaps <- diff(sorted_centers)
  max_gap_idx <- which.max(gaps)
  max_gap <- gaps[max_gap_idx]
  gap_position <- sorted_centers[max_gap_idx] + max_gap / 2
  
  # Check if gap is significant enough
  if (max_gap / page_width > min_gap_ratio) {
    left_count <- sum(centers < gap_position)
    right_count <- sum(centers >= gap_position)
    total_count <- length(centers)
    
    left_ratio <- left_count / total_count
    right_ratio <- right_count / total_count
    
    if (left_ratio > 0.15 && right_ratio > 0.15) {
      confidence <- min(1.0, (max_gap / page_width) / min_gap_ratio * min(left_ratio, right_ratio) * 2)
      return(list(
        columns = list(
          list(column_id = 1L, x_start = -Inf, x_end = gap_position),
          list(column_id = 2L, x_start = gap_position, x_end = Inf)
        ),
        confidence = confidence
      ))
    }
  }
  
  return(list(columns = list(list(column_id = 1L, x_start = -Inf, x_end = Inf)), confidence = 0.1))
}

# K-means clustering for spatial column detection
detect_columns_kmeans <- function(words_df, page_width) {
  if (nrow(words_df) < 6) {
    return(list(columns = list(list(column_id = 1L, x_start = -Inf, x_end = Inf)), confidence = 0.3))
  }
  
  centers <- words_df$x + words_df$width / 2
  set.seed(42)
  km <- try(kmeans(centers, centers = 2, iter.max = 20, nstart = 5), silent = TRUE)
  
  if (inherits(km, "try-error")) {
    return(list(columns = list(list(column_id = 1L, x_start = -Inf, x_end = Inf)), confidence = 0.1))
  }
  
  cluster_centers <- sort(km$centers[, 1])
  if (abs(diff(cluster_centers)) > 0.15 * page_width) {
    split_x <- mean(cluster_centers)
    
    # Calculate confidence based on cluster separation and balance
    separation_score <- abs(diff(cluster_centers)) / page_width
    cluster_sizes <- table(km$cluster)
    balance_score <- min(cluster_sizes) / max(cluster_sizes)
    confidence <- min(1.0, separation_score * 2 + balance_score * 0.5)
    
    return(list(
      columns = list(
        list(column_id = 1L, x_start = -Inf, x_end = split_x),
        list(column_id = 2L, x_start = split_x, x_end = Inf)
      ),
      confidence = confidence
    ))
  }
  
  return(list(columns = list(list(column_id = 1L, x_start = -Inf, x_end = Inf)), confidence = 0.3))
}

# Enhanced words_to_lines with improved cross-column reading order
words_to_lines <- function(words_df, page_height, y_threshold = 8, semantic_blocks = NULL, reading_order = "ltr", preserve_column_grouping = FALSE) {
  if (nrow(words_df) == 0) return(list())
  words_df$visual_y <- page_height - words_df$y
  lines <- list()
  idx <- 1L
  
  # Store semantic blocks for bbox overlap matching
  semantic_blocks_data <- semantic_blocks
  
  for (col in sort(unique(words_df$column_id))) {
    wcol <- words_df[words_df$column_id == col, , drop = FALSE]
    wcol <- wcol[order(wcol$visual_y, wcol$x), , drop = FALSE]
    current_y <- NA_real_
    current_words <- list()
    flush_line <- function() {
      if (length(current_words) > 0) {
        line_text <- paste(vapply(current_words, function(w) w$text, character(1)), collapse = " ")
        
        # Find semantic classification for this line using bbox overlap
        line_block_type <- "body_text"
        line_confidence <- 0.5
        highest_confidence <- 0.0
        
        if (!is.null(semantic_blocks_data) && length(semantic_blocks_data) > 0) {
          # Calculate line bounding box from all words in the line
          line_x1 <- min(vapply(current_words, function(w) w$x, numeric(1)), na.rm = TRUE)
          line_x2 <- max(vapply(current_words, function(w) w$x + w$width, numeric(1)), na.rm = TRUE)
          line_y1 <- min(vapply(current_words, function(w) w$y, numeric(1)), na.rm = TRUE)
          line_y2 <- max(vapply(current_words, function(w) w$y + w$height, numeric(1)), na.rm = TRUE)
          
          # Check each semantic block for overlap with this line
          for (block in semantic_blocks_data) {
            if (!is.null(block$position)) {
              block_x1 <- block$position$x
              block_y1 <- block$position$y
              block_x2 <- block$position$x + block$position$width
              block_y2 <- block$position$y + block$position$height
              
              # Calculate overlap area
              x_overlap <- max(0, min(line_x2, block_x2) - max(line_x1, block_x1))
              y_overlap <- max(0, min(line_y2, block_y2) - max(line_y1, block_y1))
              overlap_area <- x_overlap * y_overlap
              
              # If there's overlap and this block has higher confidence, use it
              if (overlap_area > 0 && block$classification_confidence > highest_confidence) {
                line_block_type <- block$block_type
                line_confidence <- block$classification_confidence
                highest_confidence <- block$classification_confidence
              }
            }
          }
        }
        
        lines[[length(lines) + 1]] <<- list(
          line_id = sprintf("line_%04d", idx),
          column_id = col,
          text = trimws(line_text),
          block_type = line_block_type,
          classification_confidence = line_confidence,
          visual_y = if (length(current_words) > 0) mean(vapply(current_words, function(w) w$visual_y, numeric(1))) else NA_real_,
          column_left_x = min(vapply(current_words, function(w) w$x, numeric(1)), na.rm = TRUE)
        )
        idx <<- idx + 1L
      }
      current_words <<- list()
      current_y <<- NA_real_
    }
    for (i in seq_len(nrow(wcol))) {
      vy <- wcol$visual_y[i]
      if (is.na(current_y) || abs(vy - current_y) <= y_threshold) {
        current_words <- c(current_words, list(list(
          text = wcol$text[i],
          x = wcol$x[i],
          y = wcol$y[i],
          width = wcol$width[i],
          height = wcol$height[i],
          visual_y = vy
        )))
        current_y <- if (is.na(current_y)) vy else (current_y * 0.8 + vy * 0.2)
      } else {
        flush_line()
        current_words <- list(list(
          text = wcol$text[i],
          x = wcol$x[i],
          y = wcol$y[i],
          width = wcol$width[i],
          height = wcol$height[i],
          visual_y = vy
        ))
        current_y <- vy
      }
    }
    flush_line()
  }
  
  # Apply cross-column reading order if not preserving column grouping
  if (!preserve_column_grouping && reading_order == "ltr") {
    lines <- sort_lines_reading_order(lines, words_df$column_id)
  }
  
  # Remove helper fields that were added for sorting
  for (i in seq_along(lines)) {
    lines[[i]]$visual_y <- NULL
    lines[[i]]$column_left_x <- NULL
  }
  
  lines
}

# Sort lines for proper cross-column reading order
sort_lines_reading_order <- function(lines, column_ids) {
  if (length(lines) == 0) return(lines)
  y_tol <- 15
  visy <- sapply(lines, function(l) l$visual_y)
  ord <- order(visy, decreasing = TRUE)
  lines <- lines[ord]
  visy <- visy[ord]
  used <- rep(FALSE, length(lines))
  out <- list()
  for (i in seq_along(lines)) {
    if (used[i]) next
    yi <- visy[i]
    same_row <- which(!used & abs(visy - yi) <= y_tol)
    row <- lines[same_row]
    row <- row[order(sapply(row, function(l) l$column_left_x))]
    out <- c(out, row)
    used[same_row] <- TRUE
  }
  out
}

# Analyze font characteristics from both document-level and glyph-level data
analyze_font_characteristics <- function(text_df, font_df = NULL) {
  if (is.null(text_df) || nrow(text_df) == 0) {
    return(list(
      text_data = text_df,
      font_stats = list(
        font_sizes = numeric(0),
        dominant_fonts = character(0),
        size_distribution = list(median = NA, min = NA, max = NA),
        has_font_variation = FALSE
      )
    ))
  }
  
  # Extract font information from glyph-level fields or document-level data
  font_sizes <- numeric(0)
  font_names <- character(0)
  
  # Try glyph-level font information first
  if ("size" %in% names(text_df)) {
    font_sizes <- text_df$size[is.finite(text_df$size) & !is.na(text_df$size)]
  }
  
  if ("fontname" %in% names(text_df) || "font" %in% names(text_df)) {
    font_col <- if ("fontname" %in% names(text_df)) "fontname" else "font"
    font_names <- text_df[[font_col]][!is.na(text_df[[font_col]])]
  }
  
  # Calculate relative font sizes - compute directly from text_df columns
  enhanced_text_df <- text_df
  if ("size" %in% names(text_df) && any(is.finite(text_df$size) & !is.na(text_df$size))) {
    # Use actual size values from text_df, not filtered vector
    valid_sizes <- text_df$size[is.finite(text_df$size) & !is.na(text_df$size)]
    page_median_size <- stats::median(valid_sizes, na.rm = TRUE)
    
    # Compute per-row directly from text_df$size
    enhanced_text_df$relative_size <- ifelse(
      is.finite(text_df$size) & !is.na(text_df$size),
      text_df$size / page_median_size,
      1.0
    )
    enhanced_text_df$is_emphasis <- ifelse(
      is.finite(text_df$size) & !is.na(text_df$size),
      text_df$size > (page_median_size * 1.2),
      FALSE
    )
  } else {
    enhanced_text_df$relative_size <- rep(1.0, nrow(text_df))
    enhanced_text_df$is_emphasis <- rep(FALSE, nrow(text_df))
  }
  
  # Detect bold/italic from font names - compute directly from text_df columns
  font_col <- NULL
  if ("fontname" %in% names(text_df)) {
    font_col <- "fontname"
  } else if ("font" %in% names(text_df)) {
    font_col <- "font"
  }
  
  if (!is.null(font_col)) {
    # Compute per-row directly from text_df font column
    enhanced_text_df$font_style <- ifelse(
      !is.na(text_df[[font_col]]),
      ifelse(
        grepl("bold|Bold|BOLD", text_df[[font_col]], ignore.case = TRUE), "bold",
        ifelse(grepl("italic|Italic|ITALIC", text_df[[font_col]], ignore.case = TRUE), "italic", "normal")
      ),
      "normal"
    )
  } else {
    enhanced_text_df$font_style <- rep("normal", nrow(text_df))
  }
  
  # Utilize document-level font_df to enhance font analysis
  if (!is.null(font_df) && nrow(font_df) > 0) {
    # Extract document-level font information to supplement glyph-level data
    if ("name" %in% names(font_df)) {
      doc_font_names <- unique(font_df$name[!is.na(font_df$name)])
      if (length(font_names) == 0) {
        # Use document-level fonts when glyph-level info is missing
        font_names <- doc_font_names
      } else {
        # Combine both sources for comprehensive analysis
        font_names <- unique(c(font_names, doc_font_names))
      }
      
      # Backfill font_style when glyph-level font names are missing
      if (is.null(font_col) || any(is.na(enhanced_text_df$font_style))) {
        # Create a lookup for font styles from document-level data
        font_style_lookup <- character(0)
        for (doc_font in doc_font_names) {
          if (grepl("bold|Bold|BOLD", doc_font, ignore.case = TRUE)) {
            font_style_lookup[doc_font] <- "bold"
          } else if (grepl("italic|Italic|ITALIC", doc_font, ignore.case = TRUE)) {
            font_style_lookup[doc_font] <- "italic"
          } else {
            font_style_lookup[doc_font] <- "normal"
          }
        }
        
        # Apply document-level font styles where glyph-level is missing
        if (length(font_style_lookup) > 0) {
          # For missing font styles, use most common document font style
          missing_font_style <- is.na(enhanced_text_df$font_style) | enhanced_text_df$font_style == "normal"
          if (any(missing_font_style)) {
            dominant_doc_style <- names(sort(table(font_style_lookup), decreasing = TRUE))[1]
            if (!is.na(dominant_doc_style)) {
              enhanced_text_df$font_style[missing_font_style] <- dominant_doc_style
            }
          }
        }
      }
    }
  }
  
  # Calculate font statistics including document-level information
  all_font_names <- if (length(font_names) > 0) font_names else character(0)
  dominant_fonts <- if (length(all_font_names) > 0) {
    names(sort(table(all_font_names), decreasing = TRUE))[1:min(3, length(unique(all_font_names)))]
  } else {
    character(0)
  }
  
  font_stats <- list(
    font_sizes = if (length(font_sizes) > 0) sort(unique(font_sizes)) else numeric(0),
    dominant_fonts = dominant_fonts,
    size_distribution = if (length(font_sizes) > 0) {
      list(
        median = stats::median(font_sizes, na.rm = TRUE),
        min = min(font_sizes, na.rm = TRUE),
        max = max(font_sizes, na.rm = TRUE)
      )
    } else {
      list(median = NA, min = NA, max = NA)
    },
    has_font_variation = length(unique(font_sizes)) > 1 || length(unique(all_font_names)) > 1
  )
  
  list(
    text_data = enhanced_text_df,
    font_stats = font_stats
  )
}

# Aggregate font features from glyph-level text_df onto word-level data by bbox overlap
aggregate_font_features_to_words <- function(words_df, enhanced_text_df) {
  if (is.null(words_df) || nrow(words_df) == 0) {
    return(words_df)
  }
  
  if (is.null(enhanced_text_df) || nrow(enhanced_text_df) == 0) {
    # Add default font features if no text_df available
    words_df$relative_size <- rep(1.0, nrow(words_df))
    words_df$font_style <- rep("normal", nrow(words_df))
    words_df$is_emphasis <- rep(FALSE, nrow(words_df))
    return(words_df)
  }
  
  # Initialize font feature columns in words_df
  words_df$relative_size <- rep(1.0, nrow(words_df))
  words_df$font_style <- rep("normal", nrow(words_df))
  words_df$is_emphasis <- rep(FALSE, nrow(words_df))
  
  # For each word, find overlapping glyphs and aggregate their font features
  for (i in seq_len(nrow(words_df))) {
    word_bbox <- list(
      x1 = words_df$x[i],
      y1 = words_df$y[i],
      x2 = words_df$x[i] + words_df$width[i],
      y2 = words_df$y[i] + words_df$height[i]
    )
    
    # Find glyphs that overlap with this word's bounding box
    overlapping_glyphs <- logical(nrow(enhanced_text_df))
    for (j in seq_len(nrow(enhanced_text_df))) {
      glyph_bbox <- list(
        x1 = enhanced_text_df$x[j],
        y1 = enhanced_text_df$y[j],
        x2 = enhanced_text_df$x[j] + enhanced_text_df$width[j],
        y2 = enhanced_text_df$y[j] + enhanced_text_df$height[j]
      )
      
      # Check for bbox overlap using standard rectangle intersection
      x_overlap <- max(0, min(word_bbox$x2, glyph_bbox$x2) - max(word_bbox$x1, glyph_bbox$x1))
      y_overlap <- max(0, min(word_bbox$y2, glyph_bbox$y2) - max(word_bbox$y1, glyph_bbox$y1))
      overlap_area <- x_overlap * y_overlap
      
      # Consider overlapping if there's any intersection
      overlapping_glyphs[j] <- overlap_area > 0
    }
    
    if (any(overlapping_glyphs)) {
      overlapping_data <- enhanced_text_df[overlapping_glyphs, , drop = FALSE]
      
      # Aggregate font features from overlapping glyphs
      if ("relative_size" %in% names(overlapping_data)) {
        valid_sizes <- overlapping_data$relative_size[is.finite(overlapping_data$relative_size) & !is.na(overlapping_data$relative_size)]
        if (length(valid_sizes) > 0) {
          words_df$relative_size[i] <- stats::median(valid_sizes, na.rm = TRUE)
        }
      }
      
      if ("is_emphasis" %in% names(overlapping_data)) {
        emphasis_vals <- overlapping_data$is_emphasis[!is.na(overlapping_data$is_emphasis)]
        if (length(emphasis_vals) > 0) {
          words_df$is_emphasis[i] <- any(emphasis_vals)
        }
      }
      
      if ("font_style" %in% names(overlapping_data)) {
        style_vals <- overlapping_data$font_style[!is.na(overlapping_data$font_style)]
        if (length(style_vals) > 0) {
          # Use most common font style among overlapping glyphs
          style_counts <- table(style_vals)
          words_df$font_style[i] <- names(sort(style_counts, decreasing = TRUE))[1]
        }
      }
    }
  }
  
  words_df
}

# Normalize extraction outputs into a unified structure for both digital and OCR
normalize_extraction_data <- function(page_number, extraction_method, text_df, page_width, page_height, font_df = NULL, language = NULL, parameters = list(), confidence_metrics = NULL) {
  # Ensure required columns exist in text_df
  if (!is.null(text_df) && nrow(text_df) > 0) {
    for (nm in c("text", "x", "y", "width", "height")) if (!nm %in% names(text_df)) text_df[[nm]] <- NA
  }

  # Assess quality
  quality_scores <- assess_extraction_quality(text_df, page_width, page_height, method = extraction_method, confidence_metrics = confidence_metrics)

  # Convert to lists for JSON friendliness
  text_list <- if (!is.null(text_df) && nrow(text_df) > 0) {
    # Keep standard columns plus optional metadata fields
    base <- c("text", "x", "y", "width", "height", "space", "confidence", "fontname", "font", "size")
    keep <- intersect(base, names(text_df))
    split(text_df[, keep, drop = FALSE], seq_len(nrow(text_df)))
  } else list()

  font_list <- if (!is.null(font_df) && nrow(font_df) > 0) split(font_df, seq_len(nrow(font_df))) else list()

  list(
    page_number = as.integer(page_number),
    extraction_method = extraction_method,
    text_data = text_list,
    font_data = font_list,
    confidence_metrics = confidence_metrics %||% list(),
    quality_scores = quality_scores,
    page_dimensions = list(width = page_width, height = page_height),
    processing_metadata = list(timestamp = as.character(Sys.time()), language = language %||% NULL, parameters = parameters),
    error_info = NULL
  )
}

