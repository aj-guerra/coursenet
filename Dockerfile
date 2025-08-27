FROM pytorch/pytorch:2.3.1-cuda12.1-cudnn8-runtime

ENV DEBIAN_FRONTEND=noninteractive \
    PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

RUN apt-get update && apt-get install -y --no-install-recommends \
    git \
    poppler-utils \
    tesseract-ocr \
    libtesseract-dev \
    curl \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy only requirements first for better caching
COPY requirements.txt /app/requirements.txt

# Install Python deps including marker
RUN pip install --no-cache-dir -r /app/requirements.txt \
    && pip install --no-cache-dir marker-pdf

# Copy project files that are needed at runtime
COPY scripts/ /app/scripts/
COPY config/ /app/config/

# Default runtime env; override at run-time as needed
ENV TORCH_DEVICE=cuda

# Accept args via environment or docker run args by default command
ENTRYPOINT ["python", "/app/scripts/run_marker_convert.py"]
