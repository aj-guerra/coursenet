# Enhanced JSON Output Format

This document provides comprehensive documentation for the enhanced JSON output format introduced in the CourseNet preprocessing pipeline. The enhanced format consolidates all processing information into well-structured, comprehensive JSON files while maintaining backward compatibility with existing outputs.

## Overview

The enhanced JSON output system consists of two main components:

1. **Enhanced Page Output** (`page_XXXX.enhanced.json`) - Comprehensive page-level processing results
2. **Enhanced Manifest** (`manifest.json`) - Document-level processing metadata and aggregated statistics

## Enhanced Page Output Structure

### Main Sections

Each enhanced page output contains six main sections:

#### 1. Page Info
Basic page metadata and dimensions.

```json
{
  "page_info": {
    "page_number": 1,
    "page_width": 612.0,
    "page_height": 792.0,
    "page_area": 484704.0,
    "rotation": 0,
    "text_coverage": 0.85
  }
}
```

#### 2. Extraction Details
Method, language, quality scores, and confidence metrics for extraction.

```json
{
  "extraction_details": {
    "extraction_method": "digital",
    "extraction_success": true,
    "fallback_methods": [],
    "language_detection": {
      "detected_language": "en",
      "language_confidence": 0.95,
      "fallback_languages": [],
      "mixed_language": false
    },
    "overall_confidence": {
      "mean": 0.92,
      "count": 1247
    },
    "word_confidence": {
      "mean": 0.91,
      "min": 0.45,
      "p25": 0.87,
      "median": 0.93,
      "p75": 0.97,
      "max": 1.0,
      "low_confidence_count": 23,
      "total_count": 1247
    },
    "line_confidence": {
      "mean": 0.94,
      "min": 0.68,
      "p25": 0.89,
      "median": 0.95,
      "p75": 0.98,
      "max": 1.0,
      "low_confidence_count": 2,
      "total_count": 89
    }
  }
}
```

#### 3. Content Structure
Organized text content with detailed metadata.

```json
{
  "content_structure": {
    "words": [
      {
        "word_id": 1,
        "text": "Introduction",
        "bounding_box": {
          "x": 72.0,
          "y": 123.45,
          "width": 89.2,
          "height": 14.0,
          "confidence": 0.95
        },
        "line_id": 1,
        "confidence": 0.95,
        "font_size": 12.0,
        "font_family": "Times-Roman",
        "font_style": "bold"
      }
    ],
    "lines": [
      {
        "line_id": 1,
        "text": "Introduction to Computer Science",
        "bounding_box": {
          "x": 72.0,
          "y": 123.45,
          "width": 234.8,
          "height": 14.0,
          "confidence": 0.94
        },
        "word_ids": [1, 2, 3, 4],
        "column_id": 1,
        "confidence": 0.94,
        "font_size": 12.0,
        "font_family": "Times-Roman"
      }
    ],
    "columns": [
      {
        "column_id": 1,
        "bounding_box": {
          "x": 72.0,
          "y": 72.0,
          "width": 234.0,
          "height": 648.0
        },
        "line_ids": [1, 2, 3],
        "text_alignment": "left",
        "column_width": 234.0
      }
    ],
    "semantic_blocks": [
      {
        "block_id": 1,
        "classification": "heading",
        "bounding_box": {
          "x": 72.0,
          "y": 123.45,
          "width": 234.8,
          "height": 14.0
        },
        "text": "Introduction to Computer Science",
        "line_ids": [1],
        "confidence": 0.89
      }
    ],
    "tables": [
      {
        "table_id": 1,
        "bounding_box": {
          "x": 72.0,
          "y": 200.0,
          "width": 468.0,
          "height": 120.0
        },
        "rows": 4,
        "columns": 3,
        "confidence": 0.87,
        "cells": [
          {
            "row": 1,
            "column": 1,
            "bounding_box": {
              "x": 72.0,
              "y": 200.0,
              "width": 156.0,
              "height": 30.0
            },
            "text": "Course Code",
            "rowspan": 1,
            "colspan": 1
          }
        ]
      }
    ]
  }
}
```

#### 4. Font Analysis
Comprehensive font information with spatial distribution.

```json
{
  "font_analysis": {
    "font_statistics": {
      "unique_fonts": 3,
      "unique_sizes": 5,
      "dominant_font": "Times-Roman",
      "dominant_size": 12.0
    },
    "font_details": [
      {
        "font_family": "Times-Roman",
        "font_size": 12.0,
        "font_style": "normal",
        "count": 987,
        "coverage": 0.79,
        "bounding_boxes": [
          {
            "x": 72.0,
            "y": 150.0,
            "width": 234.0,
            "height": 400.0
          }
        ]
      }
    ],
    "size_distribution": {
      "12.0": {
        "count": 987,
        "coverage": 0.79
      },
      "14.0": {
        "count": 156,
        "coverage": 0.13
      }
    }
  }
}
```

#### 5. Processing Metadata
Pipeline settings, timestamps, and error information.

```json
{
  "processing_metadata": {
    "processing_timestamp": "2024-01-15T14:30:45-08:00",
    "pipeline_version": "1.0.0",
    "processing_time": 2.34,
    "settings_used": {
      "ocr_confidence_threshold": 0.6,
      "enable_semantic_analysis": true
    },
    "errors": [],
    "warnings": []
  }
}
```

#### 6. Quality Assessment
Detailed quality metrics and confidence distributions.

```json
{
  "quality_assessment": {
    "overall_quality": 0.89,
    "text_density": 0.85,
    "character_distribution": {
      "alphabetic_ratio": 0.78,
      "numeric_ratio": 0.12,
      "punctuation_ratio": 0.08,
      "whitespace_ratio": 0.02
    },
    "spatial_consistency": 0.92,
    "font_coverage": 0.98,
    "confidence_breakdown": {
      "high_confidence_ratio": 0.87,
      "medium_confidence_ratio": 0.11,
      "low_confidence_ratio": 0.02
    }
  }
}
```

## Enhanced Manifest Structure

The enhanced manifest provides document-level processing metadata:

### Main Sections

#### 1. Document Info
```json
{
  "document_info": {
    "catalog_year": 2024,
    "source_file": "catalog2024.pdf",
    "file_size": 15728640,
    "total_pages": 450,
    "pages_processed": 448,
    "pages_failed": 2,
    "document_title": "Academic Catalog 2024-2025",
    "creation_date": "2024-08-15T10:00:00Z"
  }
}
```

#### 2. Processing Summary
```json
{
  "processing_summary": {
    "extraction_methods": {
      "digital_pages": 420,
      "ocr_pages": 28,
      "mixed_pages": 0,
      "failed_pages": 2
    },
    "language_detection": {
      "primary_language": "en",
      "language_distribution": {
        "en": {
          "page_count": 445,
          "confidence": {
            "mean": 0.95,
            "min": 0.78,
            "max": 1.0
          }
        },
        "es": {
          "page_count": 3,
          "confidence": {
            "mean": 0.87,
            "min": 0.82,
            "max": 0.91
          }
        }
      },
      "mixed_language_pages": 0,
      "fallback_usage": 2
    },
    "processing_statistics": {
      "total_processing_time": 892.5,
      "average_page_time": 1.98,
      "success_rate": 0.996,
      "pages_with_tables": 156,
      "pages_with_images": 23
    }
  }
}
```

#### 3. Quality Overview
```json
{
  "quality_overview": {
    "overall_quality": {
      "mean_quality": 0.87,
      "quality_distribution": {
        "mean": 0.87,
        "min": 0.34,
        "p25": 0.82,
        "median": 0.89,
        "p75": 0.94,
        "max": 0.99,
        "std_dev": 0.12
      },
      "pages_high_quality": 389,
      "pages_medium_quality": 57,
      "pages_low_quality": 2
    },
    "confidence_overview": {
      "mean_confidence": 0.91,
      "confidence_distribution": {
        "mean": 0.91,
        "min": 0.45,
        "p25": 0.87,
        "median": 0.93,
        "p75": 0.96,
        "max": 1.0,
        "std_dev": 0.08
      },
      "low_confidence_pages": [23, 157, 289]
    }
  }
}
```

## Detailed Bounding Box Format

All bounding boxes throughout the enhanced output use a consistent format:

```json
{
  "bounding_box": {
    "x": 72.0,        // Left coordinate
    "y": 123.45,      // Top coordinate  
    "width": 89.2,    // Width of bounding box
    "height": 14.0,   // Height of bounding box
    "confidence": 0.95 // Optional: confidence in bounding box accuracy
  }
}
```

### Coordinate System
- Origin (0,0) is at the top-left corner of the page
- X increases to the right
- Y increases downward
- All measurements are in points (1/72 inch)

## Confidence Score Structures

### Simple Confidence Score
```json
{
  "mean": 0.92,
  "count": 1247
}
```

### Confidence Distribution
```json
{
  "mean": 0.91,
  "min": 0.45,
  "p25": 0.87,
  "median": 0.93,
  "p75": 0.97,
  "max": 1.0,
  "low_confidence_count": 23,
  "total_count": 1247
}
```

## Extraction Method Tracking

The enhanced output tracks extraction methods comprehensively:

- **digital**: Text extracted from PDF's internal text layer
- **ocr**: Text extracted via Optical Character Recognition
- **mixed**: Combination of digital and OCR methods

Fallback methods are tracked when primary extraction fails.

## Language Detection Metadata

Language detection includes:
- Detected language (ISO 639-1 codes)
- Confidence in detection
- Alternative languages considered
- Mixed language indicators

## Usage Examples

### Reading Enhanced Page Output

```r
# Load enhanced page output
library(jsonlite)
enhanced_page <- fromJSON("page_0001.enhanced.json")

# Access different sections
page_info <- enhanced_page$page_info
extraction_details <- enhanced_page$extraction_details
content <- enhanced_page$content_structure
quality <- enhanced_page$quality_assessment

# Get all words with high confidence
high_conf_words <- content$words[
  sapply(content$words, function(w) w$confidence > 0.8)
]

# Get text lines from first column
column_1_lines <- content$lines[
  sapply(content$lines, function(l) l$column_id == 1)
]
```

### Reading Enhanced Manifest

```r
# Load enhanced manifest
manifest <- fromJSON("manifest.json")

# Get processing summary
proc_summary <- manifest$processing_summary
quality_overview <- manifest$quality_overview

# Find pages that used OCR
ocr_pages <- proc_summary$extraction_methods$ocr_pages

# Get low quality pages
low_quality_pages <- quality_overview$confidence_overview$low_confidence_pages
```

### Quality Assessment

```r
# Assess document quality from manifest
overall_quality <- manifest$quality_overview$overall_quality$mean_quality

# Check extraction method distribution
extraction_methods <- manifest$processing_summary$extraction_methods

# Calculate digital extraction ratio
digital_ratio <- extraction_methods$digital_pages / 
  manifest$document_info$total_pages
```

## Consumer Guidance

### For Text Extraction
- Use `content_structure.lines` for clean, ordered text
- Check `extraction_details.overall_confidence` for reliability
- Consider `quality_assessment.text_density` for content richness

### For Layout Analysis
- Use `content_structure.columns` for column detection
- Check `content_structure.semantic_blocks` for content classification
- Use detailed bounding boxes for precise positioning

### For Quality Assessment
- Check `quality_assessment.overall_quality` for page reliability
- Use `confidence_breakdown` for confidence distribution analysis
- Monitor `extraction_details.fallback_methods` for processing issues

### For Table Processing
- Use `content_structure.tables` for detected tables
- Check table confidence scores before processing
- Validate cell structure against expectations

## Configuration

Enhanced output generation can be configured via `config/settings.yml`:

```yaml
enhanced_output:
  enable_enhanced_json: true
  include_detailed_bounding_boxes: true
  include_confidence_distributions: true
  include_font_analysis_details: true

output_formats:
  generate_legacy_geometry: true  # Backward compatibility
  generate_enhanced_page: true
  generate_extraction_json: true
  generate_llm_json: true

quality_reporting:
  include_quality_breakdown: true
  confidence_histogram_bins: 10
  spatial_analysis_detail_level: "detailed"

metadata_tracking:
  track_processing_times: true
  include_pipeline_version: true
  detailed_error_tracking: true
```

## Backward Compatibility

The enhanced output system maintains full backward compatibility:

- Legacy `geometry.json` files continue to be generated
- Existing `llm.json` and `extraction.json` formats unchanged
- Manifest structure enhanced but retains core fields
- Existing consumers continue to work without modification

## Performance Considerations

### Large Documents
- Enhanced output adds ~15-20% processing overhead
- JSON files are ~2-3x larger than legacy geometry files
- Consider disabling detailed bounding boxes for very large documents

### Memory Usage
- Enhanced output builders cache intermediate results
- Peak memory usage may increase by ~30-40%
- Processing time scales linearly with page count

### Storage Requirements
- Enhanced JSON files average 50-100KB per page
- Manifest files typically under 500KB
- Plan for ~3-4x storage compared to legacy outputs

## Troubleshooting

### Common Issues

**Enhanced Output Not Generated**
- Check `enhanced_output.enable_enhanced_json` setting
- Verify all required modules are sourced
- Check for errors in processing log

**Missing Bounding Boxes**
- Ensure `include_detailed_bounding_boxes` is enabled
- Check extraction method (OCR may have fewer bounding boxes)
- Verify input PDF has valid coordinate information

**Low Confidence Scores**
- Check extraction method (OCR typically has lower confidence)
- Verify language detection is accurate
- Consider adjusting confidence thresholds in settings

**Large File Sizes**
- Disable detailed bounding boxes for size reduction
- Reduce confidence distribution detail level
- Consider selective output generation for specific pages

### Error Recovery

The enhanced output system includes robust error handling:
- Falls back to legacy manifest if enhanced generation fails
- Continues processing even if individual page enhancement fails
- Logs detailed error information for debugging

## Schema Validation

Enhanced outputs conform to JSON schemas:
- `schemas/enhanced_page_output.schema.json`
- `schemas/enhanced_manifest.schema.json`

Use these schemas for validation and documentation generation.

## Future Enhancements

Planned improvements to the enhanced output format:
- Image region detection and metadata
- Advanced table structure recognition
- Multi-language text region identification
- Enhanced semantic classification confidence
- Performance optimizations for large documents
