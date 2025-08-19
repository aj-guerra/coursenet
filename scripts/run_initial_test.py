#!/usr/bin/env python3
import argparse
import hashlib
import json
import os
from pathlib import Path
from typing import Dict, Any, List

import orjson
import yaml
from jsonschema import validate, Draft7Validator
from rich import print


def load_yaml(path: Path) -> Dict[str, Any]:
    with path.open("r", encoding="utf-8") as f:
        return yaml.safe_load(f)


def ensure_dirs(year: int) -> None:
    base = Path("data")
    for sub in [
        base / "raw",
        base / "interim" / f"year={year}" / "pages",
        base / "processed" / f"year={year}",
        base / "patches",
        base / "logs",
    ]:
        sub.mkdir(parents=True, exist_ok=True)


def file_checksum(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as f:
        for chunk in iter(lambda: f.read(8192), b""):
            h.update(chunk)
    return h.hexdigest()


def write_manifest(year: int, pdf_path: Path, dest: Path) -> Dict[str, Any]:
    manifest = {
        "catalog_year": year,
        "file": str(pdf_path),
        "file_id": file_checksum(pdf_path),
        "page_count": None,
    }
    dest.parent.mkdir(parents=True, exist_ok=True)
    with dest.open("w", encoding="utf-8") as f:
        json.dump(manifest, f, indent=2)
    return manifest


def dummy_document_map(year: int, file_id: str) -> Dict[str, Any]:
    return {
        "catalog_year": year,
        "file_id": file_id,
        "toc_sections": [],
        "inferred_sections": [],
        "normalization_hints": {},
        "confidence": 0.0,
        "schema_version": "0.1.0",
    }


def validate_json(schema_path: Path, data: Dict[str, Any]) -> List[str]:
    with schema_path.open("r", encoding="utf-8") as f:
        schema = json.load(f)
    validator = Draft7Validator(schema)
    errors = [e.message for e in validator.iter_errors(data)]
    return errors


def main() -> None:
    parser = argparse.ArgumentParser(description="Run initial pipeline test on a single PDF year")
    parser.add_argument("--year", type=int, required=True)
    parser.add_argument("--pdf", type=str, required=True)
    args = parser.parse_args()

    year = args.year
    pdf_path = Path(args.pdf)
    if not pdf_path.exists():
        raise SystemExit(f"PDF not found: {pdf_path}")

    ensure_dirs(year)

    manifest_path = Path("data/interim") / f"year={year}" / "manifest.json"
    manifest = write_manifest(year, pdf_path, manifest_path)

    docmap = dummy_document_map(year, manifest["file_id"])
    docmap_path = Path("data/processed") / f"year={year}" / "document_map.json"
    with docmap_path.open("w", encoding="utf-8") as f:
        json.dump(docmap, f, indent=2)

    errors = validate_json(Path("schemas/document_map.schema.json"), docmap)
    if errors:
        print("[red]Document map validation errors:[/red]")
        for e in errors:
            print(f" - {e}")
        raise SystemExit(1)

    # Write run log
    run_log = {
        "run_id": f"initial_{year}",
        "inputs": {"pdf": str(pdf_path)},
        "outputs": {"document_map": str(docmap_path)},
        "status": "ok",
    }
    log_path = Path("data/logs") / f"run_{year}_initial.json"
    with log_path.open("w", encoding="utf-8") as f:
        json.dump(run_log, f, indent=2)

    print("[green]Initial test completed successfully.[/green]")
    print(f"Document map: {docmap_path}")
    print(f"Run log:      {log_path}")


if __name__ == "__main__":
    main()

