#!/usr/bin/env python3

import argparse
import json
import os
import shlex
import subprocess
import sys
from pathlib import Path


def load_env(env_path: str) -> None:
    if not env_path:
        return
    if not os.path.exists(env_path):
        return
    try:
        from dotenv import load_dotenv  # type: ignore

        load_dotenv(env_path)
    except Exception:
        # Fallback: naive .env loader
        with open(env_path, "r", encoding="utf-8") as f:
            for line in f:
                line = line.strip()
                if not line or line.startswith("#"):
                    continue
                if "=" not in line:
                    continue
                key, value = line.split("=", 1)
                os.environ.setdefault(key.strip(), value.strip())


def build_marker_cmd(
    input_pdf: str,
    output_dir: str,
    use_llm: bool,
    force_ocr: bool,
    device: str | None,
    model: str | None,
    layout_batch_size: int | None,
    detection_batch_size: int | None,
    recognition_batch_size: int | None,
    table_rec_batch_size: int | None,
    config_json: str | None,
) -> list[str]:
    cmd: list[str] = [
        "marker_single",
        input_pdf,
        "--output_dir",
        output_dir,
    ]

    if use_llm:
        gemini_key = os.environ.get("GEMINI_API_KEY", "")
        if not gemini_key:
            raise RuntimeError("GEMINI_API_KEY is required in environment when --use-llm is set.")
        cmd += [
            "--use_llm",
            "--llm_service",
            "marker.services.gemini.GoogleGeminiService",
            "--gemini_api_key",
            gemini_key,
        ]

    if force_ocr:
        cmd += ["--force_ocr"]

    if model:
        cmd += ["--model", model]

    if layout_batch_size is not None:
        cmd += ["--layout_batch_size", str(layout_batch_size)]
    if detection_batch_size is not None:
        cmd += ["--detection_batch_size", str(detection_batch_size)]
    if recognition_batch_size is not None:
        cmd += ["--recognition_batch_size", str(recognition_batch_size)]
    if table_rec_batch_size is not None:
        cmd += ["--table_rec_batch_size", str(table_rec_batch_size)]

    if config_json:
        if not os.path.exists(config_json):
            raise FileNotFoundError(f"Config JSON file not found: {config_json}")
        cmd += ["--config_json", config_json]

    # TORCH_DEVICE via env if provided
    if device:
        os.environ["TORCH_DEVICE"] = device

    return cmd


def main() -> int:
    parser = argparse.ArgumentParser(description="Run marker_single with optional LLM and OCR flags.")
    parser.add_argument("--input", "-i", required=True, help="Input PDF file path")
    parser.add_argument(
        "--output_dir",
        "-o",
        default="data/marker_output",
        help="Output directory for markdown/json [default: %(default)s]",
    )
    parser.add_argument("--use_llm", action="store_true", help="Use LLM to improve results")
    parser.add_argument("--no_llm", action="store_true", help="Explicitly disable LLM")
    parser.add_argument("--force_ocr", action="store_true", help="Force OCR if needed")
    parser.add_argument("--no_ocr", action="store_true", help="Explicitly disable OCR")
    parser.add_argument("--device", type=str, default=None, help="TORCH_DEVICE override (e.g., cpu, cuda:0, mps)")
    parser.add_argument("--model", type=str, default=None, help="Marker layout model override (optional)")
    parser.add_argument("--layout_batch_size", type=int, default=None, help="Layout model batch size (optional)")
    parser.add_argument("--detection_batch_size", type=int, default=None, help="Detection model batch size (optional)")
    parser.add_argument("--recognition_batch_size", type=int, default=None, help="Recognition model batch size (optional)")
    parser.add_argument("--table_rec_batch_size", type=int, default=None, help="Table recognition batch size (optional)")
    parser.add_argument("--config_json", type=str, default=None, help="Path to JSON configuration file (optional)")
    parser.add_argument("--env", type=str, default=".env", help="Path to .env file [default: %(default)s]")

    args = parser.parse_args()

    input_pdf = args.input
    if not os.path.exists(input_pdf):
        print(f"Input not found: {input_pdf}", file=sys.stderr)
        return 1

    load_env(args.env)

    # Handle mutually exclusive toggles
    use_llm = bool(args.use_llm and not args.no_llm)
    force_ocr = bool(args.force_ocr and not args.no_ocr)

    Path(args.output_dir).mkdir(parents=True, exist_ok=True)

    cmd = build_marker_cmd(
        input_pdf=input_pdf,
        output_dir=args.output_dir,
        use_llm=use_llm,
        force_ocr=force_ocr,
        device=args.device,
        model=args.model,
        layout_batch_size=args.layout_batch_size,
        detection_batch_size=args.detection_batch_size,
        recognition_batch_size=args.recognition_batch_size,
        table_rec_batch_size=args.table_rec_batch_size,
        config_json=args.config_json,
    )

    print("Running:", " ".join(shlex.quote(p) for p in cmd))
    try:
        proc = subprocess.run(cmd, check=False)
        if proc.returncode != 0:
            print(f"marker_single failed with status {proc.returncode}", file=sys.stderr)
            return proc.returncode
    except FileNotFoundError:
        print("marker_single not found in PATH. Ensure marker is installed.", file=sys.stderr)
        return 127

    print("Conversion complete. Output directory:", os.path.abspath(args.output_dir))
    return 0


if __name__ == "__main__":
    sys.exit(main())

