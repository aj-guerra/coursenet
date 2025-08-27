UCD Catalog Extraction – Starter

Overview
- Pipeline to extract yearly course catalogs and majors into structured JSONL, with validation and QA.

Quickstart

Marker-based PDF to Markdown
- This repo includes an R wrapper that calls the `marker-pdf` CLI to convert PDFs to Markdown/JSON with optional LLM enhancement (Gemini) and forced OCR. See Marker docs: [marker README](https://github.com/datalab-to/marker?tab=readme-ov-file#llm-services).

Setup
1) Environment
   - Ensure a `.env` exists with `GOOGLE_API_KEY=...` (Gemini key) if using `--use_llm`.
2) Python CLI (marker-pdf)
   - Install via pip or pipx:
     - `pip install marker-pdf` or `pipx install marker-pdf`
   - Verify install: `marker-pdf --help`.
3) R runtime deps
   - Install minimal R packages used by the wrapper:
     - `Rscript scripts/install_r_deps.R`

Convert a PDF
- Example (outputs to `data/marker_output` by default):
  - `Rscript scripts/run_marker_convert.R --input path/to/file.pdf --output_dir data/marker_output --use_llm --force_ocr`
- Notes:
  - When `--use_llm` is set, the wrapper passes `--llm gemini` to `marker-pdf` and uses `GOOGLE_API_KEY` from `.env`.
  - Set device via `--device cpu` or `--device cuda:0` (maps to `TORCH_DEVICE`).
  - You can override the model via `--model <name>`; otherwise Marker defaults apply.

Project Layout
- `config/`: runtime settings and mappings
- `docs/`: documentation and ADRs
- `workflows/`: job manifests
- `scripts/`: R helper scripts (`install_r_deps.R`, `run_marker_convert.R`)
- `requirements.txt`: minimal Python deps for other tooling (not required for marker)

Notes
- Marker CLI reference: [marker README](https://github.com/datalab-to/marker?tab=readme-ov-file#llm-services)
- LLM usage is optional; if no `GOOGLE_API_KEY` is present, omit `--use_llm`.

