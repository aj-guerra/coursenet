#!/usr/bin/env Rscript

warning("DEPRECATION: 'preprocess_pdf_columns.R' is deprecated. Use 'scripts/preprocess_pdf_unified.R' instead for unified digital/OCR processing.")

args <- commandArgs(trailingOnly = TRUE)

# Ensure user library path is available for package loading
lib_dir <- Sys.getenv("R_LIBS_USER", unset = "/workspace/.Rlibs")
dir.create(lib_dir, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib_dir, .libPaths()))

# Dependencies
required_packages <- c("jsonlite", "pdftools", "digest")
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

`%||%` <- function(a, b) if (!is.null(a)) a else b

argv <- parse_args(args)
if (is.null(argv$year) || is.null(argv$pdf)) {
  stop("Usage: Rscript scripts/preprocess_pdf_columns.R --year YYYY --pdf path/to/catalog.pdf [--pages 411,412]")
}

year <- as.integer(argv$year)
pdf_path <- argv$pdf
pages_arg <- if (!is.null(argv$pages)) argv$pages else NULL

if (!file.exists(pdf_path)) {
  stop(sprintf("PDF not found: %s", pdf_path))
}

dir_interim <- file.path("data", "interim", sprintf("year=%d", year))
dir_pages <- file.path(dir_interim, "pages")
dir.create(dir_pages, recursive = TRUE, showWarnings = FALSE)

file_checksum <- function(file_path) digest::digest(file_path, algo = "sha256", file = TRUE)

# Word reconstruction from char-level data
chars_to_words <- function(d) {
  if (nrow(d) == 0) return(d)
  d <- d[order(-d$y, d$x), , drop = FALSE]
  words <- list()
  current <- NULL
  
  flush_word <- function() {
    if (!is.null(current) && nchar(current$text) > 0) {
      words[[length(words) + 1]] <<- current
    }
    current <<- NULL
  }
  
  for (i in seq_len(nrow(d))) {
    ch <- d[i, ]
    if (is.null(current)) {
      current <- list(
        text = ch$text,
        x = ch$x, y = ch$y, width = ch$width, height = ch$height
      )
    } else {
      gap <- ch$x - (current$x + current$width)
      # Widen gap tolerance to better merge spaced headings
      if (isTRUE(ch$space) || gap > max(6, 1.0 * ch$height)) {
        flush_word()
        current <- list(
          text = ch$text,
          x = ch$x, y = ch$y, width = ch$width, height = ch$height
        )
      } else {
        current$text <- paste0(current$text, ch$text)
        current$width <- (ch$x + ch$width) - current$x
        current$height <- max(current$height, ch$height)
        current$y <- max(current$y, ch$y)
      }
    }
  }
  flush_word()
  if (length(words) == 0) return(data.frame())
  data.frame(
    text = vapply(words, function(w) w$text, character(1)),
    x = vapply(words, function(w) w$x, numeric(1)),
    y = vapply(words, function(w) w$y, numeric(1)),
    width = vapply(words, function(w) w$width, numeric(1)),
    height = vapply(words, function(w) w$height, numeric(1)),
    stringsAsFactors = FALSE
  )
}

# Column detection via valley in x-density or kmeans fallback
detect_columns <- function(words_df) {
  if (nrow(words_df) == 0) return(list(list(column_id = 1L, x_start = -Inf, x_end = Inf)))
  centers <- words_df$x + words_df$width / 2
  page_width <- max(words_df$x + words_df$width, na.rm = TRUE)
  dens <- density(centers, na.rm = TRUE)
  chg <- diff(sign(diff(dens$y)))
  valleys <- which(chg == 2)
  splits <- dens$x[valleys]
  col_boundaries <- NULL
  if (length(splits) > 0) {
    split_x <- splits[which.min(dens$y[valleys])]
    left_ratio <- mean(centers <= split_x)
    right_ratio <- mean(centers > split_x)
    sep <- min(abs(split_x - min(centers)), abs(max(centers) - split_x)) / page_width
    if (left_ratio > 0.2 && right_ratio > 0.2 && sep > 0.1) {
      col_boundaries <- list(
        list(column_id = 1L, x_start = -Inf, x_end = split_x),
        list(column_id = 2L, x_start = split_x, x_end = Inf)
      )
    }
  }
  if (is.null(col_boundaries)) {
    set.seed(42)
    km <- try(kmeans(centers, centers = 2, iter.max = 20), silent = TRUE)
    if (!inherits(km, "try-error")) {
      cts <- sort(km$centers[, 1])
      if (abs(diff(cts)) > 0.2 * page_width) {
        split_x <- mean(cts)
        col_boundaries <- list(
          list(column_id = 1L, x_start = -Inf, x_end = split_x),
          list(column_id = 2L, x_start = split_x, x_end = Inf)
        )
      }
    }
  }
  if (is.null(col_boundaries)) {
    col_boundaries <- list(list(column_id = 1L, x_start = -Inf, x_end = Inf))
  }
  col_boundaries
}

assign_columns <- function(words_df, cols) {
  if (nrow(words_df) == 0) return(words_df)
  words_df$column_id <- 1L
  for (i in seq_len(nrow(words_df))) {
    cx <- words_df$x[i] + words_df$width[i] / 2
    which_col <- 1L
    for (c in cols) {
      if (cx > c$x_start && cx <= c$x_end) { which_col <- c$column_id; break }
    }
    words_df$column_id[i] <- which_col
  }
  words_df
}

# Group words into lines within each column using visual top-to-bottom order
words_to_lines <- function(words_df, page_height, y_threshold = 8) {
  if (nrow(words_df) == 0) return(list())
  words_df$visual_y <- page_height - words_df$y
  lines <- list()
  idx <- 1L
  for (col in sort(unique(words_df$column_id))) {
    wcol <- words_df[words_df$column_id == col, , drop = FALSE]
    wcol <- wcol[order(wcol$visual_y, wcol$x), , drop = FALSE]
    current_y <- NA_real_
    current_words <- list()
    flush_line <- function() {
      if (length(current_words) > 0) {
        line_text <- paste(vapply(current_words, function(w) w$text, character(1)), collapse = " ")
        lines[[length(lines) + 1]] <<- list(
          line_id = sprintf("line_%04d", idx),
          column_id = col,
          text = trimws(line_text)
        )
        idx <<- idx + 1L
      }
      current_words <<- list()
      current_y <<- NA_real_
    }
    for (i in seq_len(nrow(wcol))) {
      vy <- wcol$visual_y[i]
      if (is.na(current_y) || abs(vy - current_y) <= y_threshold) {
        current_words <- c(current_words, list(list(text = wcol$text[i])))
        current_y <- if (is.na(current_y)) vy else (current_y * 0.8 + vy * 0.2)
      } else {
        flush_line()
        current_words <- list(list(text = wcol$text[i]))
        current_y <- vy
      }
    }
    flush_line()
  }
  lines
}

process_page <- function(pdf_path, page_num) {
  pages <- pdftools::pdf_data(pdf_path)
  if (length(pages) < page_num) return(NULL)
  d <- pages[[page_num]]
  if (is.null(d) || nrow(d) == 0) return(NULL)
  for (nm in c("x","y","width","height","space","text")) if (!nm %in% names(d)) d[[nm]] <- NA
  d$space[is.na(d$space)] <- FALSE
  page_width <- max(d$x + d$width, na.rm = TRUE)
  page_height <- max(d$y, na.rm = TRUE)
  
  words <- chars_to_words(d)
  if (nrow(words) == 0) return(NULL)
  cols <- detect_columns(words)
  words <- assign_columns(words, cols)
  lines <- words_to_lines(words, page_height)
  
  list(
    page_number = page_num,
    words = words,
    lines = lines,
    columns = cols,
    page_width = page_width,
    page_height = page_height
  )
}

info <- pdftools::pdf_info(pdf_path)
all_pages <- seq_len(info$pages)
if (!is.null(pages_arg)) {
  sel <- as.integer(strsplit(pages_arg, ",")[[1]])
  sel <- sel[sel %in% all_pages]
  if (length(sel) == 0) stop("No valid pages specified in --pages")
  all_pages <- sel
}

cat(sprintf("Geometry-based preprocessing on %d page(s): %s\n", length(all_pages), paste(head(all_pages, 10), collapse = ", "))) 

page_files <- list()

for (page_num in all_pages) {
  cat(sprintf("Page %d ... ", page_num))
  flush.console()
  res <- try(process_page(pdf_path, page_num), silent = TRUE)
  if (inherits(res, "try-error") || is.null(res)) {
    cat("skip\n")
    next
  }
  
  # Raw geometry file
  geometry_json <- list(
    page_number = res$page_number,
    total_pages = info$pages,
    columns_detected = length(res$columns),
    columns = res$columns,
    blocks = res$lines,
    page_width = res$page_width,
    page_height = res$page_height,
    method = "pdf_geometry",
    processing_timestamp = as.character(Sys.time())
  )
  geometry_filename <- sprintf("page_%04d.geometry.json", page_num)
  geometry_filepath <- file.path(dir_pages, geometry_filename)
  jsonlite::write_json(geometry_json, geometry_filepath, pretty = TRUE, auto_unbox = TRUE, null = "null")
  
  # Concise LLM-ready file: per-column text lines, descending line_id per column
  lines_df <- res$lines
  num_cols <- length(res$columns)
  columns_text <- vector("list", length = num_cols)
  if (length(lines_df) > 0) {
    get_line_num <- function(id) { as.integer(gsub("[^0-9]", "", id)) }
    for (col_idx in seq_len(num_cols)) {
      col_lines <- lines_df[ vapply(lines_df, function(l) l$column_id == col_idx, logical(1)) ]
      if (length(col_lines) > 1) {
        ord <- order(vapply(col_lines, function(l) get_line_num(l$line_id), integer(1)), decreasing = TRUE)
        col_lines <- col_lines[ord]
      }
      columns_text[[col_idx]] <- unname(lapply(col_lines, function(l) l$text))
    }
  }
  
  llm_json <- list(
    page_number = res$page_number,
    columns_detected = num_cols,
    columns_text = columns_text
  )
  llm_filename <- sprintf("page_%04d.llm.json", page_num)
  llm_filepath <- file.path(dir_pages, llm_filename)
  jsonlite::write_json(llm_json, llm_filepath, pretty = TRUE, auto_unbox = TRUE, null = "null")
  
  page_files <- c(page_files, geometry_filename, llm_filename)
  cat("done\n")
}

# Update manifest minimally
manifest_path <- file.path(dir_interim, "manifest.json")
manifest <- list(
  catalog_year = year,
  file = normalizePath(pdf_path, winslash = "/", mustWork = FALSE),
  file_id = file_checksum(pdf_path),
  page_count = info$pages,
  pages_processed = length(page_files),
  processing_timestamp = as.character(Sys.time()),
  extractor = list(name = "pdf_geometry")
)
jsonlite::write_json(manifest, manifest_path, pretty = TRUE, auto_unbox = TRUE, null = "null")

cat(sprintf("Completed geometry preprocessing. Files written: %d. Manifest: %s\n", length(page_files), manifest_path))
