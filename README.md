UCD Catalog Extraction – Starter

Overview
- Pipeline to extract yearly course catalogs and majors into structured JSONL, with validation and QA.

Quickstart

Marker-based PDF to Markdown
- This repo now includes a Python wrapper that calls the `marker_single` CLI to convert PDFs to Markdown/JSON with optional LLM enhancement (Gemini) and OCR. An R wrapper remains available but is deprecated.

Setup
1) **Marker Installation**
   - Install marker-pdf: `pip install marker-pdf`
   - Or use our Docker image (see Docker section below)
   - **Important**: Ensure `marker_single` is available in your PATH when running locally
   - Test installation: `marker_single --help`

2) **Environment Variables**
   - Create a `.env` file with `GEMINI_API_KEY=your_key_here` if using `--use_llm`
   - The Python script will automatically load this when LLM features are enabled

Convert a PDF (Python)
- **Basic conversion** (no LLM):
  ```bash
  python scripts/run_marker_convert.py --input data/raw/p411.pdf --output_dir data/marker_output
  ```

- **With LLM enhancement** (requires GEMINI_API_KEY):
  ```bash
  python scripts/run_marker_convert.py --input data/raw/p411.pdf --output_dir data/marker_output --use_llm
  ```

- **With custom batch sizes** (for resource-constrained environments):
  ```bash
  python scripts/run_marker_convert.py --input data/raw/p411.pdf --output_dir data/marker_output \
    --layout_batch_size 1 --detection_batch_size 1 --recognition_batch_size 1 --table_rec_batch_size 1
  ```

- **With configuration file** (advanced settings):
  ```bash
  python scripts/run_marker_convert.py --input data/raw/p411.pdf --output_dir data/marker_output \
    --config_json config/marker_config.json
  ```

Docker (GPU-ready, e.g., Lambda Labs)
- Build the image:
  ```bash
  docker build -t coursenet/marker-runner .
  ```

- Run with GPU and volume mounts:
  ```bash
  docker run --rm --gpus all \
    -v "$PWD/data/raw":/app/data/raw \
    -v "$PWD/data/marker_output":/app/data/marker_output \
    --env-file .env \
    coursenet/marker-runner \
    --input /app/data/raw/p411.pdf --output_dir /app/data/marker_output --use_llm
  ```
  - For CPU-only, add `-e TORCH_DEVICE=cpu` and omit `--gpus all`.

Key Features
- **Flexible Batch Sizes**: Optional batch size controls for resource management
- **LLM Integration**: Optional Gemini LLM enhancement for better results
- **Environment Management**: Automatic .env file loading for API keys
- **Configuration Files**: JSON-based configuration for advanced settings
- **Simple Setup**: Uses standard marker_single CLI with sensible defaults

Configuration Files
- Use `--config_json` to specify advanced marker settings via JSON
- Sample configs provided:
  - `config/marker_config.json` - Basic low-resource settings
  - `config/marker_config_llm.json` - LLM-enabled with resource optimization
- Get all available settings: `marker_single config --help`
- Configuration overrides command-line batch size options
- See [Marker documentation](https://github.com/datalab-to/marker) for complete configuration reference

Available Options
- `--input`: Input PDF file path (required)
- `--output_dir`: Output directory [default: data/marker_output]
- `--use_llm`: Enable LLM processing with Gemini [default: false]
- `--force_ocr`: Force OCR on entire document [default: false]
- `--layout_batch_size`: Layout model batch size [optional]
- `--detection_batch_size`: Detection model batch size [optional]
- `--recognition_batch_size`: Recognition model batch size [optional]
- `--table_rec_batch_size`: Table recognition batch size [optional]
- `--config_json`: Path to JSON configuration file [optional]
- `--device`: PyTorch device (cpu, cuda:0, mps) [optional]

Project Layout
- `config/`: runtime settings and mappings
- `docs/`: documentation and ADRs  
- `workflows/`: job manifests
- `scripts/`: Python helper script (`run_marker_convert.py`), legacy R scripts
- `data/raw/`: Input PDF files
- `data/interim/`: Processed intermediate data
- `requirements.txt`: minimal Python deps for other tooling

Notes
- Batch size parameters are optional and can be used for resource management
- Ensure `marker_single` is properly installed and available in PATH (or use Docker)
- LLM features require a valid GEMINI_API_KEY in `.env` file
- See [Marker documentation](https://github.com/datalab-to/marker) for more details

