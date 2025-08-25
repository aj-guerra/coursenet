UCD Catalog Extraction – Starter

Overview
- Pipeline to extract yearly course catalogs and majors into structured JSONL, with validation and QA.

Quickstart
1) Create a `.env` from `.env.template` and set secrets.
2) Install dependencies: `pip install -r requirements.txt`.
3) Place a catalog PDF at `data/raw/year_YYYY_catalog.pdf`.
4) Run initial test: `python scripts/run_initial_test.py --year YYYY --pdf data/raw/year_YYYY_catalog.pdf`.

Project Layout
- `config/`: runtime settings and mappings
- `data/`: raw → interim → processed data folders
- `schemas/`: JSON Schemas for validation
- `prompts/`: agent prompts
- `workflows/`: job manifests
- `scripts/`: orchestration and CLI entry points

Notes
- Outputs are schema-first JSON files; validation is enforced.
- LLM calls are optional; the pipeline falls back to heuristic rules when no API key is provided.

Unified PDF Preprocessing
- The pipeline now uses a unified R script to preprocess PDF pages with automatic detection of digital vs scanned PDFs and OCR fallback.
- Script: `scripts/preprocess_pdf_unified.R`
- Dependencies added: R packages `tesseract` (OCR), `cld2` (language detection), and `stringr` (text utilities) alongside `jsonlite`, `pdftools`, `digest`.
- The unified script will:
  - Detect PDF type via `pdf_text()` heuristics
  - For digital PDFs, extract geometry via `pdf_data()` and detect columns/lines
  - For scanned PDFs or failures, run OCR via `pdf_ocr_data(language)` with auto language detection
  - Emit per-page outputs: `page_####.geometry.json` and `page_####.llm.json` under `data/interim/year=YYYY/pages/`
  - Update `manifest.json` with `pdf_type`, `ocr_language`, `ocr_performed`, and `text_extraction_method`

Pipeline Usage
- Run the end-to-end pipeline:
  - `Rscript scripts/run_pdf_pipeline.R --year YYYY --pdf data/raw/year_YYYY_catalog.pdf`
- Step 3 now shows: "Preprocessing PDF pages (unified digital/OCR detection)" and invokes `scripts/preprocess_pdf_unified.R`.

Deprecations
- `scripts/preprocess_pdf_columns.R` and `scripts/preprocess_pdf_pages.R` are deprecated. They remain for backward compatibility and print a deprecation warning. Prefer `scripts/preprocess_pdf_unified.R`.

