# Enhanced Preprocessing Capabilities

This document provides comprehensive documentation for the enhanced preprocessing capabilities in the CourseNet system, including improved column detection, cross-column reading order, table detection, and quality-based fallback strategies.

## Overview

The enhanced preprocessing pipeline builds upon the existing unified preprocessing system to provide more robust handling of both digital and OCR-extracted PDF data. Key improvements include:

- **Multi-method column detection** with automatic fallback strategies
- **Improved cross-column reading order** for proper text flow
- **Table detection** for structured course listing data
- **Quality-based fallback mechanisms** for poor extraction quality
- **Configurable detection parameters** for different document types

## Enhanced Column Detection

### Function: `detect_columns_enhanced()`

The enhanced column detection system replaces the original `detect_columns()` function with a robust multi-method approach that adapts to different data types and quality levels.

#### Detection Methods

1. **Density-based Detection (Primary)**
   - Uses kernel density estimation on word center positions
   - Works well for digital PDFs with consistent text distribution
   - Configurable smoothing parameter for density curve analysis
   - Falls back if confidence < 0.5

2. **Vertical Gap Analysis (Fallback 1)**
   - Analyzes whitespace gaps between text elements
   - Optimized for sparse OCR data where density methods fail
   - Finds largest significant vertical gap as column separator
   - Better handling of irregular text distributions

3. **K-means Clustering (Fallback 2)**
   - Spatial clustering of word positions when other methods fail
   - Balances cluster separation and size consistency
   - Final fallback before single-column default
   - Configurable through settings

#### Parameters

```r
detect_columns_enhanced(
  words_df,                    # Word-level data frame
  page_dimensions = NULL,      # List with width/height
  method = "digital",          # "digital" or "ocr"
  settings = NULL              # Configuration settings
)
```

#### Configuration (settings.yml)

```yaml
column_detection:
  min_gap_ratio: 0.1           # Minimum gap as fraction of page width
  density_smoothing: 0.8       # Density curve smoothing factor
  kmeans_enabled: true         # Enable k-means fallback
  force_two_columns_threshold: 0.3  # Text density threshold for forcing 2 columns
```

#### Return Value

```r
list(
  columns = list(              # Column boundary definitions
    list(column_id = 1L, x_start = 0, x_end = 300),
    list(column_id = 2L, x_start = 300, x_end = 600)
  ),
  metadata = list(
    method_used = "density",   # Detection method that succeeded
    detection_confidence = 0.8, # Confidence score (0-1)
    fallback_triggered = FALSE, # Whether fallback was used
    attempts = 1L              # Number of detection attempts
  )
)
```

## Enhanced Reading Order

### Function: `words_to_lines()` (Enhanced)

The enhanced `words_to_lines()` function ensures proper left-to-right, top-to-bottom reading flow across multiple columns, addressing issues with the original per-column processing approach.

#### Key Improvements

- **Cross-column reading order**: Proper page-level text flow
- **Configurable reading direction**: Support for different reading patterns
- **Reading order sorting**: Global line reordering based on visual position
- **Backward compatibility**: Optional preservation of original column grouping

#### Parameters

```r
words_to_lines(
  words_df,                    # Words with column assignments
  page_height,                 # Page height for coordinate conversion
  y_threshold = 8,             # Line grouping tolerance
  semantic_blocks = NULL,      # Semantic analysis results
  reading_order = "ltr",       # "ltr" (left-to-right)
  preserve_column_grouping = FALSE  # Maintain original column grouping
)
```

#### Reading Order Algorithm

1. **Within-column processing**: Group words into lines within each column
2. **Cross-column sorting**: Reorder lines globally by (column_left_x, visual_y)
3. **Row-based grouping**: Handle lines that span multiple columns
4. **Natural flow**: Ensure proper top-to-bottom, left-to-right reading

### Helper Function: `sort_lines_reading_order()`

Implements the cross-column reading order logic:

```r
sort_lines_reading_order(lines, column_ids)
```

- Groups lines by visual y-position (rows)
- Sorts within each row by column position (left to right)
- Maintains natural reading flow across page structure

## Table Detection System

### Module: `table_detection.R`

The table detection system identifies tabular structures in course listing documents by analyzing horizontal alignment patterns and validating table structure consistency.

### Main Function: `detect_tables()`

Comprehensive table detection with configurable parameters:

```r
detect_tables(
  words_df,                    # Word-level data
  lines = NULL,                # Line data (optional)
  settings = NULL              # Configuration settings
)
```

#### Detection Process

1. **Horizontal Alignment Analysis**
   - Clusters x-coordinates of words into potential table columns
   - Identifies repeating column patterns
   - Calculates alignment confidence scores

2. **Table Structure Validation**
   - Confirms consistent row/column structure
   - Validates minimum size requirements
   - Checks cell fill rates and coverage

3. **Cell Extraction**
   - Groups text within detected table cells
   - Creates structured table data with bounding boxes
   - Maintains spatial relationships

#### Configuration (settings.yml)

```yaml
table_detection:
  enable_table_detection: true    # Enable/disable table detection
  min_table_columns: 3           # Minimum columns for valid table
  min_table_rows: 2              # Minimum rows for valid table
  alignment_tolerance: 5         # Pixel tolerance for alignment
  min_cell_width: 20             # Minimum column width
  confidence_threshold: 0.6      # Minimum confidence for table acceptance
```

#### Return Structure

```r
list(
  tables = list(               # Detected table structures
    list(
      table_id = "table_001",
      rows = list(             # Table row data
        list(
          row_id = "row_001",
          cells = list(        # Cell data within row
            list(
              cell_id = "cell_r1_c1",
              text = "Course Code",
              column_index = 1L,
              bounding_box = list(x = 10, y = 100, width = 80, height = 15)
            )
          ),
          y_position = 100
        )
      ),
      columns_detected = 4L,
      confidence = 0.85,
      bounding_box = list(x = 10, y = 100, width = 400, height = 200)
    )
  ),
  processing_metadata = list(
    tables_detected = 1L,
    detection_enabled = TRUE,
    detection_method = "horizontal_alignment"
  )
)
```

### Helper Functions

#### `analyze_horizontal_alignment()`
- Clusters x-coordinates to find column positions
- Calculates alignment confidence based on cluster quality
- Filters columns that are too close together

#### `validate_table_structure()`
- Checks for consistent table patterns
- Validates minimum row and column requirements
- Calculates table confidence scores

#### `extract_table_cells()`
- Creates structured cell data from detected tables
- Maintains spatial relationships and text content
- Generates proper bounding box information

## Quality-Based Fallback Strategies

The enhanced preprocessing system includes intelligent fallback mechanisms that activate when extraction quality is poor or when initial detection methods fail.

### Fallback Triggers

1. **Quality Score Thresholds**
   - Digital PDFs: overall_quality < 0.35
   - OCR data: mean_confidence < 0.55
   - Configurable per extraction type

2. **Structural Indicators**
   - Single column detected with high text density
   - Poor column detection confidence
   - Inconsistent spatial layout

3. **Detection Failures**
   - Primary detection methods fail
   - Low confidence in initial results
   - Sparse or irregular text distribution

### Configuration (settings.yml)

```yaml
fallback_strategies:
  enable_quality_fallback: true        # Enable fallback system
  digital_quality_threshold: 0.35     # Quality threshold for digital PDFs
  ocr_confidence_threshold: 0.55      # Confidence threshold for OCR
  single_column_density_threshold: 0.3 # Density threshold for forcing 2 columns

quality_thresholds:
  min_column_detection_confidence: 0.7 # Minimum confidence for column detection
  max_fallback_attempts: 2            # Maximum fallback attempts
```

### Fallback Process

1. **Quality Assessment**: Evaluate extraction quality scores
2. **Threshold Comparison**: Check against configured thresholds
3. **Fallback Activation**: Retry detection with alternative methods
4. **Result Validation**: Compare fallback results with original
5. **Best Result Selection**: Choose highest confidence result

## Integration with Main Pipeline

### Modified `preprocess_pdf_unified.R`

The main preprocessing script integrates all enhanced features:

```r
# Enhanced column detection with settings
col_result <- detect_columns_enhanced(
  words, 
  page_dimensions = list(width = page_width_local, height = page_height_local),
  method = res$extraction_method,
  settings = settings
)

# Quality-based fallback logic
if (should_apply_fallback(res, settings)) {
  fallback_result <- detect_columns_enhanced(
    words, 
    page_dimensions = ...,
    method = ...,
    settings = enhanced_settings_with_kmeans_forced
  )
  # Use better result
}

# Enhanced line processing with reading order
lines <- words_to_lines(
  words, 
  page_height_local, 
  semantic_blocks = semantic_analysis_result$semantic_blocks,
  reading_order = "ltr"
)

# Table detection
table_result <- detect_tables(words, lines, settings)
```

### Enhanced Output Schema

The geometry JSON output includes new fields:

```json
{
  "tables": [...],                    // Detected table structures
  "column_detection_metadata": {      // Column detection metadata
    "method_used": "density",
    "detection_confidence": 0.8,
    "fallback_triggered": false,
    "attempts": 1
  },
  "processing_metadata": {
    "table_detection_enabled": true,
    "tables_detected": 2,
    "column_detection_method": "density",
    "quality_fallback_used": false
  }
}
```

## Performance Considerations

### Memory Usage
- Enhanced functions maintain similar memory profiles
- Table detection adds modest overhead for cell structure
- Column detection metadata is lightweight

### Processing Time
- Multi-method detection adds minimal overhead due to early success exits
- Table detection scales with document complexity
- Fallback strategies only activate when needed

### Accuracy Improvements
- Reduced false positive column detection
- Better handling of sparse OCR data
- More reliable reading order across document types

## Troubleshooting

### Common Issues

#### Poor Column Detection
- **Symptoms**: Single column detected for multi-column documents
- **Solutions**: 
  - Lower `min_gap_ratio` in settings
  - Enable quality fallback
  - Check text density thresholds

#### Incorrect Reading Order
- **Symptoms**: Text blocks out of sequence
- **Solutions**:
  - Verify column detection accuracy
  - Adjust y_threshold for line grouping
  - Enable cross-column reading order

#### False Table Detection
- **Symptoms**: Non-tabular content detected as tables
- **Solutions**:
  - Increase `confidence_threshold`
  - Adjust `min_table_columns` and `min_table_rows`
  - Fine-tune `alignment_tolerance`

#### Missing Tables
- **Symptoms**: Obvious tables not detected
- **Solutions**:
  - Lower `confidence_threshold`
  - Reduce `min_table_columns`
  - Increase `alignment_tolerance`

### Diagnostic Information

Enhanced metadata provides debugging information:

```r
# Column detection diagnostics
print(column_detection_metadata$method_used)
print(column_detection_metadata$detection_confidence)
print(column_detection_metadata$attempts)

# Table detection diagnostics  
print(table_result$processing_metadata$candidates_found)
print(table_result$processing_metadata$detection_method)
```

## Configuration Examples

### High-Quality Digital PDFs
```yaml
column_detection:
  min_gap_ratio: 0.05
  density_smoothing: 0.9
  
table_detection:
  alignment_tolerance: 3
  confidence_threshold: 0.8
```

### Poor-Quality OCR Data
```yaml
column_detection:
  min_gap_ratio: 0.15
  density_smoothing: 0.6
  
fallback_strategies:
  ocr_confidence_threshold: 0.4
  enable_quality_fallback: true
  
table_detection:
  alignment_tolerance: 8
  confidence_threshold: 0.5
```

### Single-Column Documents
```yaml
column_detection:
  force_two_columns_threshold: 0.8
  
fallback_strategies:
  single_column_density_threshold: 0.5
```

## Future Enhancements

Potential areas for further improvement:

1. **Multi-language Support**: Enhanced reading order for RTL languages
2. **Complex Table Structures**: Support for nested tables and merged cells
3. **Document Type Detection**: Automatic parameter tuning based on document type
4. **Machine Learning Integration**: Trained models for column and table detection
5. **Interactive Tuning**: Web interface for real-time parameter adjustment

## API Reference

### Function Signatures

```r
# Enhanced column detection
detect_columns_enhanced(words_df, page_dimensions = NULL, method = "digital", settings = NULL)

# Helper functions
detect_columns_density(words_df, page_width, min_gap_ratio = 0.1, smoothing = 0.8)
detect_vertical_gaps(words_df, page_width, min_gap_ratio = 0.1)
detect_columns_kmeans(words_df, page_width)

# Enhanced line processing
words_to_lines(words_df, page_height, y_threshold = 8, semantic_blocks = NULL, 
               reading_order = "ltr", preserve_column_grouping = FALSE)
sort_lines_reading_order(lines, column_ids)

# Table detection
detect_tables(words_df, lines = NULL, settings = NULL)
analyze_horizontal_alignment(words_df, tolerance = 5, min_cell_width = 20)
validate_table_structure(words_df, column_clusters, min_rows = 2, min_cols = 3, tolerance = 5)
extract_table_cells(words_df, table_data, tolerance = 5)
```

### Configuration Schema

All configuration options are documented in the settings.yml file with their default values and descriptions. The enhanced preprocessing system maintains backward compatibility while providing extensive customization options for different document types and quality levels.
