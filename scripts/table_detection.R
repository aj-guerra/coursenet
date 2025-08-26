#!/usr/bin/env Rscript

# Table detection module for structured course listings
# Analyzes word positions to identify tabular structures in course catalogs

# Main function to detect tables in structured course listings
detect_tables <- function(words_df, lines = NULL, settings = NULL) {
  if (is.null(words_df) || nrow(words_df) == 0) {
    return(list(
      tables = list(),
      processing_metadata = list(
        tables_detected = 0L,
        detection_enabled = TRUE,
        detection_method = "horizontal_alignment"
      )
    ))
  }
  
  # Extract settings or use defaults
  enable_detection <- if (!is.null(settings) && !is.null(settings$table_detection$enable_table_detection)) settings$table_detection$enable_table_detection else TRUE
  min_table_columns <- if (!is.null(settings) && !is.null(settings$table_detection$min_table_columns)) settings$table_detection$min_table_columns else 3L
  min_table_rows <- if (!is.null(settings) && !is.null(settings$table_detection$min_table_rows)) settings$table_detection$min_table_rows else 2L
  alignment_tolerance <- if (!is.null(settings) && !is.null(settings$table_detection$alignment_tolerance)) settings$table_detection$alignment_tolerance else 5
  min_cell_width <- if (!is.null(settings) && !is.null(settings$table_detection$min_cell_width)) settings$table_detection$min_cell_width else 20
  confidence_threshold <- if (!is.null(settings) && !is.null(settings$table_detection$confidence_threshold)) settings$table_detection$confidence_threshold else 0.6
  
  if (!enable_detection) {
    return(list(
      tables = list(),
      processing_metadata = list(
        tables_detected = 0L,
        detection_enabled = FALSE,
        detection_method = "disabled"
      )
    ))
  }
  
  # Analyze horizontal alignment to find potential table columns
  alignment_result <- analyze_horizontal_alignment(words_df, alignment_tolerance, min_cell_width)
  
  if (length(alignment_result$column_clusters$positions) < min_table_columns) {
    return(list(
      tables = list(),
      processing_metadata = list(
        tables_detected = 0L,
        detection_enabled = TRUE,
        detection_method = "horizontal_alignment",
        failure_reason = "insufficient_columns"
      )
    ))
  }
  
  # Validate table structure and extract tables
  validated_tables <- validate_table_structure(
    words_df, 
    alignment_result$column_clusters, 
    min_table_rows, 
    min_table_columns, 
    alignment_tolerance
  )
  
  if (length(validated_tables) == 0) {
    return(list(
      tables = list(),
      processing_metadata = list(
        tables_detected = 0L,
        detection_enabled = TRUE,
        detection_method = "horizontal_alignment",
        failure_reason = "no_valid_tables"
      )
    ))
  }
  
  # Extract table cells and create final table structures
  tables <- list()
  for (i in seq_along(validated_tables)) {
    table_data <- validated_tables[[i]]
    if (table_data$confidence >= confidence_threshold) {
      table_cells <- extract_table_cells(words_df, table_data, alignment_tolerance)
      
      if (!is.null(table_cells) && length(table_cells$rows) >= min_table_rows) {
        tables[[length(tables) + 1]] <- list(
          table_id = sprintf("table_%03d", length(tables) + 1),
          rows = table_cells$rows,
          columns_detected = table_data$columns_detected,
          confidence = table_data$confidence,
          bounding_box = table_cells$bounding_box
        )
      }
    }
  }
  
  list(
    tables = tables,
    processing_metadata = list(
      tables_detected = length(tables),
      detection_enabled = TRUE,
      detection_method = "horizontal_alignment",
      candidates_found = length(validated_tables),
      confidence_threshold = confidence_threshold
    )
  )
}

# Analyze horizontal alignment by clustering x-coordinates of words
analyze_horizontal_alignment <- function(words_df, tolerance = 5, min_cell_width = 20) {
  if (nrow(words_df) == 0) {
    return(list(column_clusters = list(), alignment_confidence = 0.0))
  }
  
  # Get left edges of words for alignment analysis
  x_positions <- words_df$x
  
  # Cluster x-positions that are close together (within tolerance)
  sorted_x <- sort(unique(x_positions))
  clusters <- list()
  current_cluster <- c(sorted_x[1])
  
  for (i in 2:length(sorted_x)) {
    if (sorted_x[i] - max(current_cluster) <= tolerance) {
      current_cluster <- c(current_cluster, sorted_x[i])
    } else {
      if (length(current_cluster) >= 2) {  # Only keep clusters with multiple elements
        clusters[[length(clusters) + 1]] <- current_cluster
      }
      current_cluster <- c(sorted_x[i])
    }
  }
  
  # Add the last cluster if it has enough elements
  if (length(current_cluster) >= 2) {
    clusters[[length(clusters) + 1]] <- current_cluster
  }
  
  # Calculate representative x-position for each cluster (median)
  column_positions <- sapply(clusters, function(cluster) median(cluster))
  
  # Filter clusters that are too close together (less than min_cell_width apart)
  if (length(column_positions) > 1) {
    keep_columns <- logical(length(column_positions))
    keep_columns[1] <- TRUE
    
    for (i in 2:length(column_positions)) {
      if (column_positions[i] - column_positions[i-1] >= min_cell_width) {
        keep_columns[i] <- TRUE
      }
    }
    
    column_positions <- column_positions[keep_columns]
    clusters <- clusters[keep_columns]
  }
  
  # Calculate alignment confidence based on cluster quality
  alignment_confidence <- 0.0
  if (length(clusters) >= 2) {
    # Confidence based on number of words aligned to each position
    cluster_sizes <- sapply(clusters, length)
    size_consistency <- 1.0 - (sd(cluster_sizes) / mean(cluster_sizes))
    column_separation <- if (length(column_positions) > 1) min(diff(sort(column_positions))) else 0
    
    alignment_confidence <- min(1.0, (size_consistency * 0.6) + (min(1.0, column_separation / min_cell_width) * 0.4))
  }
  
  list(
    column_clusters = list(
      positions = column_positions,
      clusters = clusters,
      cluster_sizes = if (length(clusters) > 0) sapply(clusters, length) else integer(0)
    ),
    alignment_confidence = alignment_confidence
  )
}

# Validate table structure and identify table regions
validate_table_structure <- function(words_df, column_clusters, min_rows = 2, min_cols = 3, tolerance = 5) {
  if (length(column_clusters$positions) < min_cols) {
    return(list())
  }
  
  # Group words by y-position to find potential table rows
  y_positions <- words_df$y
  unique_y <- sort(unique(y_positions), decreasing = TRUE)  # Top to bottom
  
  # Cluster y-positions that are close together (same row)
  y_clusters <- list()
  current_y_cluster <- c(unique_y[1])
  
  for (i in 2:length(unique_y)) {
    if (abs(unique_y[i] - min(current_y_cluster)) <= tolerance * 2) {  # Slightly larger tolerance for rows
      current_y_cluster <- c(current_y_cluster, unique_y[i])
    } else {
      if (length(current_y_cluster) >= 1) {
        y_clusters[[length(y_clusters) + 1]] <- current_y_cluster
      }
      current_y_cluster <- c(unique_y[i])
    }
  }
  
  # Add the last cluster
  if (length(current_y_cluster) >= 1) {
    y_clusters[[length(y_clusters) + 1]] <- current_y_cluster
  }
  
  # Calculate representative y-position for each row cluster
  row_positions <- sapply(y_clusters, function(cluster) median(cluster))
  
  # For each row, check how many of the detected columns have content
  table_candidates <- list()
  
  # Analyze potential table regions
  if (length(row_positions) >= min_rows) {
    # Check for consecutive rows that span multiple columns
    for (start_row in 1:(length(row_positions) - min_rows + 1)) {
      for (end_row in (start_row + min_rows - 1):length(row_positions)) {
        if (end_row - start_row + 1 < min_rows) next
        
        # Count how many columns have content in this row range
        rows_with_content <- 0
        total_cells_filled <- 0
        expected_cells <- length(column_clusters$positions) * (end_row - start_row + 1)
        
        for (row_idx in start_row:end_row) {
          row_y <- row_positions[row_idx]
          row_words <- words_df[abs(words_df$y - row_y) <= tolerance * 2, , drop = FALSE]
          
          if (nrow(row_words) > 0) {
            columns_with_content <- 0
            for (col_pos in column_clusters$positions) {
              col_words <- row_words[abs(row_words$x - col_pos) <= tolerance, , drop = FALSE]
              if (nrow(col_words) > 0) {
                columns_with_content <- columns_with_content + 1
                total_cells_filled <- total_cells_filled + 1
              }
            }
            
            if (columns_with_content >= min_cols) {
              rows_with_content <- rows_with_content + 1
            }
          }
        }
        
        # Calculate table confidence
        row_coverage <- rows_with_content / (end_row - start_row + 1)
        cell_fill_rate <- total_cells_filled / expected_cells
        
        if (rows_with_content >= min_rows && row_coverage >= 0.5) {
          confidence <- min(1.0, (row_coverage * 0.6) + (cell_fill_rate * 0.4))
          
          table_candidates[[length(table_candidates) + 1]] <- list(
            start_row = start_row,
            end_row = end_row,
            row_positions = row_positions[start_row:end_row],
            column_positions = column_clusters$positions,
            rows_detected = end_row - start_row + 1,
            columns_detected = length(column_clusters$positions),
            confidence = confidence,
            cells_filled = total_cells_filled,
            expected_cells = expected_cells
          )
        }
      }
    }
  }
  
  # Sort by confidence and return best candidates
  if (length(table_candidates) > 0) {
    confidences <- sapply(table_candidates, function(t) t$confidence)
    table_candidates <- table_candidates[order(confidences, decreasing = TRUE)]
  }
  
  table_candidates
}

# Extract table cells based on detected table structure
extract_table_cells <- function(words_df, table_data, tolerance = 5) {
  if (is.null(table_data) || length(table_data$row_positions) == 0 || length(table_data$column_positions) == 0) {
    return(NULL)
  }
  
  rows <- list()
  
  # Calculate overall table bounding box
  min_x <- min(table_data$column_positions) - tolerance
  max_x <- max(table_data$column_positions) + tolerance
  min_y <- min(table_data$row_positions) - tolerance * 2
  max_y <- max(table_data$row_positions) + tolerance * 2
  
  # Extract cells for each row
  for (row_idx in seq_along(table_data$row_positions)) {
    row_y <- table_data$row_positions[row_idx]
    row_words <- words_df[abs(words_df$y - row_y) <= tolerance * 2, , drop = FALSE]
    
    if (nrow(row_words) > 0) {
      cells <- list()
      
      for (col_idx in seq_along(table_data$column_positions)) {
        col_x <- table_data$column_positions[col_idx]
        
        # Find words in this cell
        cell_words <- row_words[abs(row_words$x - col_x) <= tolerance, , drop = FALSE]
        
        cell_text <- ""
        cell_bbox <- list(x = col_x, y = row_y, width = 0, height = 0)
        
        if (nrow(cell_words) > 0) {
          # Combine text from words in this cell
          cell_text <- paste(cell_words$text, collapse = " ")
          
          # Calculate cell bounding box
          cell_bbox <- list(
            x = min(cell_words$x),
            y = min(cell_words$y),
            width = max(cell_words$x + cell_words$width) - min(cell_words$x),
            height = max(cell_words$y + cell_words$height) - min(cell_words$y)
          )
        }
        
        cells[[col_idx]] <- list(
          cell_id = sprintf("cell_r%d_c%d", row_idx, col_idx),
          text = trimws(cell_text),
          column_index = col_idx,
          bounding_box = cell_bbox
        )
      }
      
      rows[[row_idx]] <- list(
        row_id = sprintf("row_%03d", row_idx),
        cells = cells,
        y_position = row_y
      )
    }
  }
  
  # Filter out empty rows
  non_empty_rows <- list()
  for (row in rows) {
    has_content <- any(sapply(row$cells, function(cell) nchar(cell$text) > 0))
    if (has_content) {
      non_empty_rows[[length(non_empty_rows) + 1]] <- row
    }
  }
  
  list(
    rows = non_empty_rows,
    bounding_box = list(
      x = min_x,
      y = min_y,
      width = max_x - min_x,
      height = max_y - min_y
    )
  )
}
