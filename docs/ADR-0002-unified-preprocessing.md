## ADR-0002: Unified PDF Preprocessing with Automatic OCR Fallback

### Context
Two preprocessing scripts existed with duplicated logic: `scripts/preprocess_pdf_columns.R` (geometry-based via `pdf_data()`) and `scripts/preprocess_pdf_pages.R` (text-based via `pdf_text()`). The pipeline only invoked the columns script. There was no OCR capability for scanned PDFs and no automatic language detection. This duplication increased maintenance cost and limited robustness.

### Decision
Introduce a unified preprocessing script `scripts/preprocess_pdf_unified.R` that:
- Detects whether the PDF is digital or scanned via a light `pdf_text()` heuristic.
- For digital PDFs, uses geometry-based extraction (`pdf_data()`) and existing column/line reconstruction.
- For scanned PDFs or failed geometry extraction, performs OCR using `pdf_ocr_data()` (tesseract), normalizing the output to match the geometry pipeline.
- Automatically detects document language from a sample using `cld2` with fallback to `eng` for OCR.
- Emits the same per-page outputs: `page_####.geometry.json` and `page_####.llm.json`.
- Updates the manifest with additional fields: `pdf_type`, `ocr_language`, `ocr_performed`, and `text_extraction_method`.

Shared utilities were extracted into `scripts/utils_preprocess.R` for argument parsing, environment setup, directory creation, and file checksums.

### Consequences
- Reduced code duplication and improved maintainability via shared utilities.
- More robust handling of scanned PDFs through OCR fallback.
- Automatic language detection improves OCR accuracy for non-English documents.
- Backward-compatible outputs maintained; columns script and pages script remain with deprecation notices to ease transition.
- Pipeline simplified to call a single unified preprocessing step.

### Technical Notes
- `detect_pdf_type()` performs a first-page `pdf_text()` call and applies simple density and length heuristics to classify as "digital" or "scanned".
- OCR uses `pdftools::pdf_ocr_data(language = <code>)`; language derived by `detect_language_sample()` which maps common `cld2` codes (e.g., `en` -> `eng`).
- Column detection and line grouping reuse the established `chars_to_words()`, `detect_columns()`, and `words_to_lines()` patterns.
- New fields in `manifest.json`: `pdf_type`, `ocr_language`, `ocr_performed`, `text_extraction_method`.

### Status
Accepted.


