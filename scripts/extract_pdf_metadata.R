#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

# Ensure user library path is available for package loading
lib_dir <- Sys.getenv("R_LIBS_USER", unset = "/workspace/.Rlibs")
dir.create(lib_dir, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib_dir, .libPaths()))

# Install additional packages if needed for enhanced PDF extraction
required_packages <- c("jsonlite", "pdftools", "pdftools", "stringr")
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
  stop("Usage: Rscript scripts/extract_pdf_metadata.R --year YYYY --pdf path/to/catalog.pdf")
}

year <- as.integer(argv$year)
pdf_path <- argv$pdf

if (!file.exists(pdf_path)) {
  stop(sprintf("PDF not found: %s", pdf_path))
}

dir_interim <- file.path("data", "interim", sprintf("year=%d", year))
dir_pdf_meta <- file.path(dir_interim, "pdf_metadata")
dir.create(dir_pdf_meta, recursive = TRUE, showWarnings = FALSE)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# Function to extract bookmarks from PDF
extract_bookmarks <- function(pdf_path) {
  tryCatch({
    # Use pdftools to get bookmarks if available
    bookmarks <- pdftools::pdf_toc(pdf_path)
    if (length(bookmarks) > 0 && !is.null(bookmarks)) {
      return(bookmarks)
    } else {
      return(NULL)
    }
  }, error = function(e) {
    cat("Warning: Could not extract bookmarks:", e$message, "\n")
    return(NULL)
  })
}

# Function to extract table of contents from text analysis
extract_toc_from_text <- function(pdf_path, max_pages = 50) {
  tryCatch({
    # Get first few pages to look for TOC
    text_pages <- pdftools::pdf_text(pdf_path)
    toc_pages <- text_pages[1:min(max_pages, length(text_pages))]
    
    toc_entries <- list()
    page_numbers <- list()
    
    # Look for common TOC patterns
    for (i in seq_along(toc_pages)) {
      page_text <- toc_pages[[i]]
      lines <- strsplit(page_text, "\n")[[1]]
      
      for (line in lines) {
        # Look for patterns like "Chapter X" or "Section X" followed by page numbers
        if (grepl("^(Chapter|Section|Part|Unit)\\s+[IVX0-9]+", line, ignore.case = TRUE) ||
            grepl("\\s+\\d+$", line) && grepl("[A-Z]", line)) {
          toc_entries <- c(toc_entries, trimws(line))
          # Try to extract page number
          page_match <- regexpr("\\s+\\d+$", line)
          if (page_match > 0) {
            page_num <- as.integer(substr(line, page_match + 1, nchar(line)))
            page_numbers <- c(page_numbers, page_num)
          } else {
            page_numbers <- c(page_numbers, i)
          }
        }
      }
    }
    
    if (length(toc_entries) > 0) {
      return(list(
        entries = toc_entries,
        page_numbers = page_numbers,
        source = "text_analysis"
      ))
    } else {
      return(NULL)
    }
  }, error = function(e) {
    cat("Warning: Could not extract TOC from text:", e$message, "\n")
    return(NULL)
  })
}

# Function to extract document structure from headers
extract_document_structure <- function(pdf_path, sample_pages = 20) {
  tryCatch({
    text_pages <- pdftools::pdf_text(pdf_path)
    sample_text <- text_pages[1:min(sample_pages, length(text_pages))]
    
    structure_elements <- list()
    
    for (i in seq_along(sample_text)) {
      page_text <- sample_text[[i]]
      lines <- strsplit(page_text, "\n")[[1]]
      
      for (line in lines) {
        line <- trimws(line)
        if (nchar(line) > 0) {
          # Look for potential headers (all caps, short lines, etc.)
          if (nchar(line) < 100 && 
              (grepl("^[A-Z\\s]+$", line) || 
               grepl("^(Chapter|Section|Part|Unit|Course|Department)", line, ignore.case = TRUE))) {
            structure_elements <- c(structure_elements, list(
              text = line,
              page = i,
              type = "potential_header"
            ))
          }
        }
      }
    }
    
    return(structure_elements)
  }, error = function(e) {
    cat("Warning: Could not extract document structure:", e$message, "\n")
    return(NULL)
  })
}

info <- pdftools::pdf_info(pdf_path)

# Extract enhanced metadata
bookmarks <- extract_bookmarks(pdf_path)
toc <- extract_toc_from_text(pdf_path)
structure <- extract_document_structure(pdf_path)

# Capture a small sample of text (first page) for context if available
sample_text <- NULL
try({
  sample_text <- pdftools::pdf_text(pdf_path)
  if (length(sample_text) > 0) {
    sample_text <- substr(sample_text[[1]], 1, 2000)
  } else {
    sample_text <- NULL
  }
}, silent = TRUE)

# Enhanced metadata structure
metadata <- list(
  year = year,
  file = normalizePath(pdf_path, winslash = "/", mustWork = FALSE),
  pages = info$pages,
  pdf_version = info$pdf_version,
  encrypted = isTRUE(info$encrypted),
  linearized = isTRUE(info$linearized),
  created = as.character(info$created),
  modified = as.character(info$modified),
  author = info$keys$Author %||% NULL,
  title = info$keys$Title %||% NULL,
  subject = info$keys$Subject %||% NULL,
  producer = info$keys$Producer %||% NULL,
  creator = info$keys$Creator %||% NULL,
  sample_text = sample_text,
  navigation = list(
    bookmarks = bookmarks,
    table_of_contents = toc,
    document_structure = structure
  )
)

# Write main metadata
jsonlite::write_json(metadata, file.path(dir_pdf_meta, "metadata.json"), pretty = TRUE, auto_unbox = TRUE, null = "null")

# Write separate detailed files for navigation elements
if (!is.null(bookmarks)) {
  jsonlite::write_json(bookmarks, file.path(dir_pdf_meta, "bookmarks.json"), pretty = TRUE, auto_unbox = TRUE, null = "null")
  cat("Extracted bookmarks to ", file.path(dir_pdf_meta, "bookmarks.json"), "\n", sep = "")
}

if (!is.null(toc)) {
  jsonlite::write_json(toc, file.path(dir_pdf_meta, "table_of_contents.json"), pretty = TRUE, auto_unbox = TRUE, null = "null")
  cat("Extracted table of contents to ", file.path(dir_pdf_meta, "table_of_contents.json"), "\n", sep = "")
}

if (!is.null(structure) && length(structure) > 0) {
  jsonlite::write_json(structure, file.path(dir_pdf_meta, "document_structure.json"), pretty = TRUE, auto_unbox = TRUE, null = "null")
  cat("Extracted document structure to ", file.path(dir_pdf_meta, "document_structure.json"), "\n", sep = "")
}

cat("Extracted enhanced PDF metadata to ", file.path(dir_pdf_meta, "metadata.json"), "\n", sep = "")

