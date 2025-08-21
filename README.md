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

