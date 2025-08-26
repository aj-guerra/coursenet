### Extraction functions and unified result structure

This document describes the new extraction functions and the unified result structure used by the preprocessing pipeline.

### extract_digital_pdf(pdf_path, page_num, language = NULL, parameters = list(), pages_data = NULL, fonts = NULL)

- Purpose: Extract character-level text and coordinates for digital PDFs using `pdf_data()` and collect font information using `pdf_fonts()`.
- Parameters: 
  - `pages_data`: Optional precomputed PDF data to avoid re-parsing the entire document per page
  - `fonts`: Optional precomputed font data to avoid reloading fonts per page
- Returns: A normalized extraction result with fields aligned to `schemas/extraction_result.schema.json`.
- Confidence: Synthetic confidence of 1.0 for digital PDFs.
- Quality: Computed via `assess_extraction_quality()` using text density and spatial consistency.
- Fonts: Document-level fonts are filtered to current page when page information is available.
- Page Dimensions: Uses `pdf_pagesize()` for reliable dimensions, with fallback to text box estimation.

### extract_scanned_pdf(pdf_path, page_num, ocr_lang = "eng", parameters = list())

- Purpose: Perform OCR using `pdf_ocr_data()` to obtain text with coordinates and confidence scores.
- Returns: A normalized extraction result with OCR confidence metrics and quality scores.
- Confidence: Calculated by `calculate_text_confidence()` from OCR token confidences. Confidence values are normalized to 0-1 range if they appear to be in 0-100 range.
- Quality: Assessed via `assess_extraction_quality()` (uses mean confidence where available).
- Page Dimensions: Uses `pdf_pagesize()` for reliable dimensions, with fallback to OCR bounding box estimation.

### Utility functions (in `scripts/utils_preprocess.R`)

- calculate_text_confidence(ocr_df, low_threshold = 0.6):
  - Computes mean confidence, a quantile-based distribution, and low-confidence token count.
- calculate_page_metrics(text_df, page_width, page_height):
  - Computes words/characters count and character density.
- assess_extraction_quality(text_df, page_width, page_height, method = c("digital","ocr"), confidence_metrics = NULL):
  - Aggregates density, spatial proxy, and confidence (for OCR) into `overall_quality`.
- normalize_extraction_data(...):
  - Standardizes outputs across both extraction methods into the unified structure.

### Unified schema

See `schemas/extraction_result.schema.json` for the canonical JSON Schema of the extraction result. Key fields:

- page_number: integer
- extraction_method: "digital" | "ocr"
- text_data: array of text elements with coordinates (and optional `confidence`)
- font_data: array of font metadata (from `pdf_fonts()` where available)
- confidence_metrics: mean, distribution, and low-confidence counts
- quality_scores: text density, character distribution, spatial consistency, overall_quality
- page_dimensions: width/height
- processing_metadata: timestamp, language, and parameters
- error_info: optional error details

### Notes and best practices

- For digital PDFs, font data is included when available at the document level; fonts are filtered per page when page information is available.
- Glyph-level font fields (fontname, font, size) from `pdf_data()` are preserved in `text_data` when present.
- OCR confidence values are automatically normalized to 0-1 range to comply with JSON Schema requirements.
- Use `overall_quality` to flag pages for manual review; for OCR pages, a mean confidence below ~0.6 may indicate low-quality text.
- When `text_data` is empty, downstream geometry outputs will be minimal and should be handled by callers.
- Page dimensions are obtained using `pdf_pagesize()` when available, which is more reliable than estimating from text/OCR bounding boxes.
- Optional metadata fields are preserved in normalization to maintain useful information for downstream processing.


