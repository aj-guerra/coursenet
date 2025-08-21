#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

# Ensure user library path is available for package loading
lib_dir <- Sys.getenv("R_LIBS_USER", unset = "/workspace/.Rlibs")
dir.create(lib_dir, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib_dir, .libPaths()))

# Install additional packages if needed for enhanced PDF extraction
required_packages <- c("jsonlite", "pdftools", "stringr", "digest")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org", lib = lib_dir)
    library(pkg, character.only = TRUE)
  }
}

parse_args <- function(argsv) {
  kv <- list()
  i <- 1
  while (i <= length(argsv)) {
    key <- argsv[i]
    if (startsWith(key, "--")) {
      key <- substring(key, 3)
      if ((i + 1) <= length(argsv) && !startsWith(argsv[i + 1], "--")) {
        kv[[key]] <- argsv[i + 1]
        i <- i + 2
      } else {
        kv[[key]] <- TRUE
        i <- i + 1
      }
    } else {
      i <- i + 1
    }
  }
  kv
}

argv <- parse_args(args)
if (is.null(argv$year) || is.null(argv$pdf)) {
  stop("Usage: Rscript scripts/preprocess_pdf_pages.R --year YYYY --pdf path/to/catalog.pdf")
}

year <- as.integer(argv$year)
pdf_path <- argv$pdf

if (!file.exists(pdf_path)) {
  stop(sprintf("PDF not found: %s", pdf_path))
}

dir_interim <- file.path("data", "interim", sprintf("year=%d", year))
dir_pages <- file.path(dir_interim, "pages")
dir.create(dir_pages, recursive = TRUE, showWarnings = FALSE)

# Function to compute file checksum
file_checksum <- function(file_path) {
  digest::digest(file_path, algo = "sha256", file = TRUE)
}

# Function to extract text blocks with layout information
extract_page_blocks <- function(pdf_path, page_num) {
  tryCatch({
    # Extract text with layout information
    text_data <- pdftools::pdf_text(pdf_path)
    if (length(text_data) < page_num) {
      return(NULL)
    }
    
    page_text <- text_data[[page_num]]
    
    # Split into lines and analyze structure
    lines <- strsplit(page_text, "\n")[[1]]
    
    blocks <- list()
    block_id <- 1
    
    for (i in seq_along(lines)) {
      line <- lines[i]
      if (nchar(trimws(line)) == 0) {
        next
      }
      
      # Analyze line characteristics
      trimmed_line <- trimws(line)
      char_count <- nchar(trimmed_line)
      
      # Estimate font characteristics based on line patterns
      # This is a simplified approach - in a real implementation you'd use pdf_data() for precise metrics
      is_bold <- grepl("^[A-Z\\s]+$", trimmed_line) && char_count > 3 && char_count < 50
      is_header <- grepl("^(Chapter|Section|Part|Unit|Course|Department)", trimmed_line, ignore.case = TRUE)
      is_course_code <- grepl("^[A-Z]{2,4}\\s+\\d+[A-Z]?", trimmed_line)
      is_page_number <- grepl("^\\d+$", trimmed_line)
      
      # Determine block type
      block_type <- "text"
      if (is_bold && is_header) {
        block_type <- "header"
      } else if (is_course_code) {
        block_type <- "course_code"
      } else if (is_page_number) {
        block_type <- "page_number"
      } else if (grepl("^\\s*\\d+\\s*$", trimmed_line)) {
        block_type <- "number"
      }
      
      # Create block structure
      block <- list(
        block_id = sprintf("page_%04d_block_%04d", page_num, block_id),
        text = trimmed_line,
        reading_order = i,
        block_type = block_type,
        char_count = char_count,
        estimated_font_size = ifelse(is_bold, "large", "normal"),
        estimated_style = ifelse(is_bold, "bold", "normal"),
        is_header = is_header,
        is_course_code = is_course_code,
        confidence = 0.8  # Placeholder confidence score
      )
      
      blocks <- c(blocks, list(block))
      block_id <- block_id + 1
    }
    
    return(blocks)
  }, error = function(e) {
    cat(sprintf("Warning: Could not extract blocks from page %d: %s\n", page_num, e$message))
    return(NULL)
  })
}

# Function to detect tables in page text
detect_tables <- function(page_text) {
  # Simple table detection based on patterns
  lines <- strsplit(page_text, "\n")[[1]]
  table_lines <- list()
  
  for (i in seq_along(lines)) {
    line <- lines[i]
    # Look for table-like patterns (multiple columns separated by spaces/tabs)
    if (grepl("\\s{3,}", line) && nchar(trimws(line)) > 20) {
      # Split by multiple spaces to detect columns
      columns <- strsplit(line, "\\s{3,}")[[1]]
      if (length(columns) >= 2) {
        table_lines <- c(table_lines, list(
          line_number = i,
          columns = columns,
          raw_text = line
        ))
      }
    }
  }
  
  if (length(table_lines) > 0) {
    return(list(
      table_id = sprintf("table_%04d", sample(1000:9999, 1)),
      lines = table_lines,
      confidence = 0.7
    ))
  } else {
    return(NULL)
  }
}

# Function to normalize text (de-hyphenation, etc.)
normalize_text <- function(text) {
  # Remove excessive whitespace
  text <- gsub("\\s+", " ", text)
  
  # Basic de-hyphenation (simplified)
  text <- gsub("([a-z])-\\s*\\n\\s*([a-z])", "\\1\\2", text)
  
  # Clean up line breaks
  text <- gsub("\\n\\s*\\n", "\n", text)
  
  return(trimws(text))
}

# Get PDF info
info <- pdftools::pdf_info(pdf_path)
total_pages <- info$pages

cat(sprintf("Processing %d pages from %s\n", total_pages, pdf_path))

# Process each page
page_files <- list()
table_files <- list()

for (page_num in 1:total_pages) {
  cat(sprintf("Processing page %d/%d\n", page_num, total_pages))
  
  # Extract blocks
  blocks <- extract_page_blocks(pdf_path, page_num)
  
  if (!is.null(blocks)) {
    # Get full page text for table detection
    page_text <- pdftools::pdf_text(pdf_path)[[page_num]]
    
    # Detect tables
    table_data <- detect_tables(page_text)
    
    # Create page structure
    page_data <- list(
      page_number = page_num,
      total_pages = total_pages,
      blocks = blocks,
      normalized_text = normalize_text(page_text),
      table_count = ifelse(is.null(table_data), 0, 1),
      processing_timestamp = as.character(Sys.time())
    )
    
    # Write page file
    page_filename <- sprintf("page_%04d.json", page_num)
    page_filepath <- file.path(dir_pages, page_filename)
    jsonlite::write_json(page_data, page_filepath, pretty = TRUE, auto_unbox = TRUE, null = "null")
    
    page_files <- c(page_files, page_filename)
    
    # Write table file if tables detected
    if (!is.null(table_data)) {
      table_filename <- sprintf("table_%04d.json", page_num)
      table_filepath <- file.path(dir_pages, table_filename)
      jsonlite::write_json(table_data, table_filepath, pretty = TRUE, auto_unbox = TRUE, null = "null")
      
      table_files <- c(table_files, table_filename)
    }
  }
}

# Update manifest with detailed information
manifest <- list(
  catalog_year = year,
  file = normalizePath(pdf_path, winslash = "/", mustWork = FALSE),
  file_id = file_checksum(pdf_path),
  page_count = total_pages,
  pages_processed = length(page_files),
  tables_detected = length(table_files),
  page_files = page_files,
  table_files = table_files,
  processing_timestamp = as.character(Sys.time()),
  schema_version = "0.1.0"
)

manifest_path <- file.path(dir_interim, "manifest.json")
jsonlite::write_json(manifest, manifest_path, pretty = TRUE, auto_unbox = TRUE, null = "null")

cat(sprintf("\nPreprocessing completed successfully!\n"))
cat(sprintf("Processed %d pages\n", length(page_files)))
cat(sprintf("Detected %d tables\n", length(table_files)))
cat(sprintf("Manifest: %s\n", manifest_path))
cat(sprintf("Page files: %s\n", dir_pages))
