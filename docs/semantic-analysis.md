# Semantic Analysis and Enhanced Text Processing

This document describes the semantic analysis and enhanced text processing features added to the coursenet PDF preprocessing pipeline. These features enable intelligent document structure recognition and improved handling of both digital and OCR-processed PDFs.

## Overview

The enhanced preprocessing pipeline combines font analysis with regex pattern matching to automatically classify text blocks and understand document structure. This provides more accurate extraction of course information, department headers, and other semantic elements from academic catalogs.

## Key Features

### 1. Enhanced Text Processing (`tokens_to_words`)

The `tokens_to_words()` function replaces the original `chars_to_words()` with automatic detection of input data type:

**Digital PDF Processing (Character-level)**
- Maintains the original character aggregation logic
- Detects word boundaries using spacing and character gaps
- Handles font variations and special characters

**OCR PDF Processing (Word-level)**
- Filters tokens based on OCR confidence scores
- Configurable confidence threshold (default: 0.6)
- Preserves spatial layout for geometry processing

**Usage:**
```r
# Automatic detection and processing
words <- tokens_to_words(text_df, min_confidence = 0.6)

# For digital PDFs: aggregates characters to words
# For OCR PDFs: filters by confidence and returns tokens
```

### 2. Font Analysis System (`analyze_font_characteristics`)

Analyzes font information from both document-level and glyph-level data to enhance semantic understanding:

**Font Size Analysis**
- Calculates relative font sizes against page median
- Identifies emphasis and header text based on size ratios
- Provides size distribution statistics

**Style Detection**
- Detects bold and italic styles from font names
- Creates style classifications for semantic analysis
- Handles various font naming conventions

**Output Fields:**
- `relative_size`: Font size relative to page median
- `font_style`: "normal", "bold", or "italic"
- `is_emphasis`: Boolean indicating emphasis text

**Usage:**
```r
result <- analyze_font_characteristics(text_df, font_df)
enhanced_text <- result$text_data
font_stats <- result$font_stats
```

### 3. Semantic Block Classification

The semantic analysis system classifies text blocks into the following types:

**Block Types:**
- `header`: Section and chapter headers
- `course_code`: Course identifiers (e.g., "CS 101")
- `department`: Department names and abbreviations
- `body_text`: Regular paragraph content
- `page_number`: Page numbering
- `other`: Unclassified content

**Classification Methods:**
1. **Font-based**: Uses relative font size and style
2. **Regex-based**: Matches content patterns
3. **Combined**: Weighted combination of font and regex signals

### 4. Confidence Scoring

Each classification includes confidence scores (0-1) based on:
- Font characteristics strength
- Regex pattern matches
- Configurable weight combinations
- Overall classification reliability

## Configuration

### Text Processing Settings

```yaml
text_processing:
  ocr_confidence_threshold: 0.6      # Minimum OCR confidence
  font_size_threshold_ratio: 1.2     # Header detection ratio
  enable_semantic_analysis: true     # Enable/disable features
```

### Semantic Classification Settings

```yaml
semantic_classification:
  header_font_size_multiplier: 1.5   # Font size for headers
  bold_keywords: ["DEPARTMENT", "SECTION", "UNIT", "CHAPTER"]
  confidence_weights:
    font: 0.6                        # Font signal weight
    regex: 0.4                       # Regex signal weight
```

### Quality Thresholds

```yaml
quality_thresholds:
  min_ocr_confidence: 0.4            # Minimum acceptable OCR quality
  min_font_coverage: 0.8             # Minimum font information coverage
  min_classification_confidence: 0.7  # Minimum classification confidence
```

### Regex Patterns

```yaml
regex_hints:
  department_header: "^[A-Z&/\\-\\s]{3,}\\s+\\([A-Z]{2,4}\\)$"
  course_header: "^[A-Z]{2,4}\\s+\\d+[A-Z]?\\.?\\s+\\(.+?\\)"
  units_pattern: "\\((\\d+(?:-\\d+)?)\\s+units?\\)"
  page_number: "^\\d+$"
  all_caps_header: "^[A-Z\\s&/\\-]{3,}$"
```

## API Reference

### Main Functions

#### `tokens_to_words(text_df, min_confidence = 0.6)`
Enhanced text processing with automatic type detection.

**Parameters:**
- `text_df`: Input text data frame
- `min_confidence`: OCR confidence threshold (0-1)

**Returns:** Processed words data frame

#### `analyze_font_characteristics(text_df, font_df = NULL)`
Analyzes font properties and enhances text data.

**Parameters:**
- `text_df`: Text data with optional font fields
- `font_df`: Document-level font information

**Returns:** List with `text_data` and `font_stats`

#### `classify_text_blocks(words_df, font_stats = NULL, page_dimensions = NULL)`
Classifies text blocks using font and content analysis.

**Parameters:**
- `words_df`: Enhanced words data frame
- `font_stats`: Font statistics from analysis
- `page_dimensions`: Page width and height

**Returns:** List of classified blocks

#### `analyze_page_semantics(words_df, font_stats = NULL, page_dimensions = NULL)`
Main semantic analysis function combining all features.

**Returns:** Complete semantic analysis results

### Enhanced Output Schema

The enhanced geometry JSON includes new fields:

```json
{
  "font_analysis": {
    "font_sizes": [10, 12, 14, 16],
    "dominant_fonts": ["Arial", "Times"],
    "size_distribution": {
      "median": 12,
      "min": 10,
      "max": 16
    },
    "has_font_variation": true
  },
  "semantic_blocks": [
    {
      "block_id": "block_0001",
      "block_type": "header",
      "classification_confidence": 0.85,
      "text_content": "COMPUTER SCIENCE (CS)",
      "font_characteristics": {
        "relative_size": 1.33,
        "font_style": "bold",
        "is_emphasis": true
      },
      "semantic_signals": {
        "is_department": true,
        "confidence": 0.9
      }
    }
  ],
  "processing_method": "enhanced_semantic"
}
```

## Troubleshooting

### Common Issues

**Poor OCR Quality**
- Increase `ocr_confidence_threshold` for cleaner results
- Check `confidence_metrics` in extraction output
- Consider preprocessing with image enhancement

**Missing Font Information**
- Verify PDF contains embedded fonts
- Check `font_analysis.has_font_variation` flag
- Fall back to content-based classification

**Low Classification Accuracy**
- Adjust `confidence_weights` for your document type
- Add domain-specific patterns to `regex_hints`
- Tune `header_font_size_multiplier` for your fonts

### Quality Metrics

Monitor these indicators for processing quality:

```r
# OCR quality
mean_confidence <- extraction_result$confidence_metrics$mean_confidence

# Font coverage
has_font_info <- length(font_stats$font_sizes) > 0

# Classification confidence
avg_confidence <- mean(sapply(semantic_blocks, function(b) b$classification_confidence))
```

### Performance Considerations

**Processing Speed**
- Semantic analysis adds ~10-20% processing time
- Font analysis is most expensive for large pages
- Disable features with `enable_semantic_analysis: false` if needed

**Memory Usage**
- Enhanced text data increases memory by ~30%
- Font statistics are lightweight
- Semantic blocks scale with text density

## Example Usage

### Basic Processing

```r
# Load enhanced pipeline
source("scripts/utils_preprocess.R")
source("scripts/semantic_analysis.R")

# Process with semantic analysis
result <- extract_digital_pdf(pdf_path, page_num)
text_df <- do.call(rbind, lapply(result$text_data, as.data.frame))

# Enhanced text processing
words <- tokens_to_words(text_df, min_confidence = 0.7)
font_result <- analyze_font_characteristics(text_df, result$font_data)
semantic_result <- analyze_page_semantics(font_result$text_data, font_result$font_stats)
```

### Custom Classification

```r
# Override settings for specific use case
settings <- load_settings()
settings$semantic_classification$header_font_size_multiplier <- 2.0
settings$text_processing$ocr_confidence_threshold <- 0.8

# Apply custom classification
blocks <- classify_text_blocks(words_df, font_stats, page_dims)
```

### Quality Assessment

```r
# Check processing quality
quality_scores <- assess_extraction_quality(text_df, page_width, page_height, "digital")
confidence_metrics <- calculate_text_confidence(text_df)

# Filter high-confidence blocks
high_conf_blocks <- Filter(function(b) b$classification_confidence > 0.8, semantic_blocks)
```

## Integration Notes

### Backward Compatibility

- Original `chars_to_words()` function is maintained as a wrapper
- Existing geometry output structure is preserved
- New fields are optional and don't break existing consumers

### Data Flow

1. **Extraction**: `extract_digital_pdf()` or `extract_scanned_pdf()`
2. **Text Processing**: `tokens_to_words()` with confidence filtering
3. **Font Analysis**: `analyze_font_characteristics()` for enhanced metadata
4. **Semantic Analysis**: `classify_text_blocks()` for content understanding
5. **Geometry Output**: Enhanced JSON with semantic fields

### Future Enhancements

Planned improvements include:
- Machine learning-based classification
- Cross-page context analysis
- Enhanced table detection
- Multi-language pattern support
