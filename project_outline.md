Title: UCD Catalog Extraction – Condensed Build Plan (R + ellmer + LLM)

Goal
-  Extract per-year: (1) complete course corpus (IDs, titles, units, descriptions, prerequisites, cross-listings, terms, notes) and (2) undergraduate majors and their requirements.
-  Use LLM agents with schema-first outputs, TOC-guided chunking, deterministic post-processing, and QA.

Project Structure
-  /README.md, /LICENSE, /renv.lock
-  /config/
  - env.yml (no secrets committed; use .env.template)
  - settings.yml (model, temperature, chunk sizes, schema_version, year ranges)
  - mappings.yml (dept aliases, historical renames, unit rules)
-  /data/
  - /raw/ (input PDFs; immutable; year_YYYY_catalog.pdf)
  - /interim/ (page JSON, OCR/layout, manifests)
  - /processed/ (JSONL outputs: courses, majors, requirement blocks, DSL)
  - /patches/ (JSON patches for human fixes)
  - /logs/ (run logs, validation)
-  /schemas/ (*.json JSON Schemas: course, prerequisite, major, requirement_block, requirement_item, document_map, prerequisite_dsl)
-  /prompts/ (structure_agent.txt, course_parser.txt, major_parser.txt; few-shots)
-  /workflows/ (jobs.yml; run descriptions)
-  /reports/ (CSV/HTML summaries, graphs)
-  /docs/ (design notes, ADRs)
-  /scripts/ (orchestration entry points)

Data Contracts (JSONL outputs, JSON Schema validation)
-  Course
  - catalog_year, department_code, course_id, title, units_min, units_max, description
  - prerequisites: array of PrerequisiteExpression
  - corequisites, recommended_prep, cross_listings, offered_terms, repeatability, notes
  - raw_text_span_ids, confidence, schema_version
-  PrerequisiteExpression
  - expression_text (verbatim)
  - normalized_logic (DSL JSON: AND, OR, ONE_OF, N_OF, NOT, MIN_GRADE, STANDING, EXAM)
  - referenced_courses, constraints, confidence
-  Major
  - catalog_year, college, department, major_name, degree_type, tracks/options
  - unit_requirements (totals, UD units, residency, GPA)
  - requirements_blocks: array[RequirementBlock]
  - free_text_policies, raw_text_span_ids, confidence, schema_version
-  RequirementBlock
  - block_type, title, selection_rule (“all”, “choose N”, “X units of …”)
  - items: array[RequirementItem], notes, confidence
-  RequirementItem
  - item_type (course, course_pattern, category, constraint)
  - course_id or pattern, min_units, max_units, constraints, confidence
-  DocumentMap
  - catalog_year, file_id, toc_sections, inferred_sections, normalization_hints, confidence
-  Prerequisite DSL
  - Structured JSON operators/operands; no free-form strings beyond leaf values

Preprocessing (PDF → text+layout)
-  For each year, produce:
  - /data/interim/year=YYYY/manifest.json (file_id checksum, page_count)
  - /data/interim/year=YYYY/pages/page_####.json:
    - blocks: text, reading order, bbox, font size/style, bold/italics
    - ocr_confidence (if scanned)
  - Optional tables: table_####.json
  - Normalization notes (de-hyphenation, column detection)

Structure Agent (Document Map)
-  Input: windows of page_####.json + TOC cues + regex hints from settings.yml
-  Output: /data/processed/year=YYYY/document_map.json (schema-valid)
  - toc_sections (title, page_start/end, level, anchor_type)
  - inferred_sections (dept/majors not in TOC)
  - normalization_hints (dept title → code), confidence, ambiguity flags
-  Prompt: /prompts/structure_agent.txt; few-shots for modern and historical catalogs
-  Context: sliding windows (5–10 pages); strict JSON output

Scheduler and Jobs (workflows)
-  Job types:
  - CourseJobs: per department/course-section slice
  - MajorJobs: per major or degree variant slice
-  /workflows/jobs.yml entries:
  - job_id, type (course_parser|major_parser), inputs (section IDs or page files), outputs (target JSONL), params (model, temperature, retries)
-  Run status logged to /data/logs/run_YYYYMMDD.json

Parser Agents (schema-first)
-  Course Parser
  - Input: department slice text+layout; alias hints
  - Output: /data/processed/year=YYYY/courses-<dept>.jsonl
  - Requirements:
    - Extract headers, units, descriptions, prereq/coreq, cross-listings, offered terms
    - Emit both verbatim prereq text and normalized DSL JSON
    - raw_text_span_ids for every non-null field; confidence; null-with-reason if uncertain
-  Major Parser
  - Input: major section slice
  - Output: majors-<dept>.jsonl, requirement_blocks-<dept>.jsonl
  - Requirements:
    - Degree variants, tracks/options, unit totals
    - Requirement blocks and selection rules
    - Items as course_ids, patterns (e.g., “PHY 1–9”), or categories (“any UD STA except 199”)
    - Conservative parsing; notes + low confidence for ambiguity

Post-processing and Validation (deterministic)
-  Canonicalize department codes via /config/mappings.yml; log corrections
-  Validate course_id pattern; normalize units to units_min/units_max
-  Resolve course references in prerequisites:
  - Pass 1: within-dept implied (e.g., “2A” → “CHE 2A”)
  - Pass 2: cross-dept using same-year global course table; unresolved → /data/logs/unresolved_refs.jsonl
-  Validate DSL against prerequisite_dsl.schema.json
-  Major validation: ensure referenced courses exist; compute derived min units and compare vs stated totals
-  Outputs: cleaned_* JSONL, validation_report.csv, normalization_log.csv

Human-in-the-loop QA
-  Generate issue reports (CSV/HTML) for:
  - Low-confidence fields, unresolved refs, unit mismatches, schema failures
-  Patches (JSON) in /data/patches:
  - target_uid, json_pointer path, new_value, justification, reviewer, timestamp
-  Re-apply patches in post-processing; re-run validation

Storage, IDs, Versioning
-  Stable IDs:
  - course_uid = hash(catalog_year, department_code, course_id)
  - major_uid = hash(catalog_year, college, department, major_name, degree_type)
  - block_uid, item_uid derived from major_uid + indices
-  Versions:
  - schema_version in every record
  - extractor_version for prompts + logic
  - run_id per full pipeline execution
-  Manifests:
  - /data/logs/run_manifest.json (inputs, models, settings snapshot, counts, metrics)
  - /data/processed/dataset_manifest.json (latest approved outputs + checksums)
-  Never overwrite; write run-suffixed outputs; maintain a “latest” pointer

Graph Exports and Analytics
-  Course graph (year=YYYY):
  - nodes.csv (course_uid attrs), edges.csv (prereq → target, with edge_type/DSL ref)
-  Major requirement graph:
  - nodes.csv (blocks, items, courses), edges.csv (“contains”, “select_from”, “satisfies”)
-  Analytics:
  - course_stats.csv, prereq_depth.csv, major_units_check.csv
-  Change logs:
  - deltas.csv (adds/removals/renames), renumbering_candidates.csv

Historical Evolution
-  mappings.yml:
  - department_aliases (by year/range)
  - course_renumberings (approved map), terminology_aliases
-  “Helm” inference step proposes mappings across years; export proposals.csv; human approval → approved_map.json
-  Apply mappings for longitudinal exports

Evaluation and Acceptance
-  Golden sets in /schemas/golden/: golden_courses.jsonl, golden_majors.jsonl (hand-curated)
-  Metrics:
  - schema pass rate, field completeness, reference resolution rate, unit sanity, average confidence
-  Thresholds in settings.yml; only promote run to “latest” if thresholds met
-  /reports/run_summary.md: metrics, top errors, links

Runbook
-  Steps:
  1) Place PDFs in /data/raw/
  2) Preprocess → /data/interim/year=YYYY/pages/*.json + manifest.json
  3) Structure Agent → document_map.json
  4) Build /workflows/jobs.yml
  5) Run Course and Major Parser jobs → JSONL outputs
  6) Post-process + validate → cleaned_* files + logs
  7) QA → patches → re-validate
  8) Promote to “latest” if metrics pass; update dataset_manifest.json
-  Failure handling: retries, quarantine to /data/processed/quarantine/, triage issues.csv
-  Change management: extractor_version bump, ADR, CHANGELOG.md update
-  Security: secrets not committed; .env.template only

LLM Guardrails and Prompting
-  Strict JSON schema outputs; reject if missing raw_text_span_ids for non-null fields
-  Few-shot coverage for historical styles, tables, course ranges, “choose N” patterns
-  Deterministic settings (model name, temperature, top_p, seed) from settings.yml
-  No external knowledge beyond provided slice

Starter Scope
-  One recent year; two departments + one major
-  Implement preprocessing + Structure Agent
-  Implement Course Parser + post-processing + QA
-  Add Major Parser + DSL once courses are stable
-  Backfill more years; then handle historical aliasing

Glossary (for consistency)
-  JSONL: one JSON object per line
-  DSL: structured JSON for prerequisite logic
-  Span IDs: references to page/block locations used to extract fields
-  Patches: JSON corrections applied post-parse without altering raw outputs
