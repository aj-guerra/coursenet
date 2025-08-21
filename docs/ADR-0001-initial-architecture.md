Context
We need a reproducible, schema-first pipeline to extract UCD catalogs.

Decision
- Adopt JSON Schema contracts for core entities (course, major, document_map, prereq DSL).
- Stage data through raw → interim → processed with manifests.
- Orchestrate via simple CLI for initial scope; expand to job system later.

Consequences
- Deterministic validation and QA can be layered early.
- Enables incremental adoption of LLM agents without blocking scaffolding.

