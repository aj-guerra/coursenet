#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

source(file.path("scripts", "utils_preprocess.R"))
source(file.path("scripts", "extraction_functions.R"))
source(file.path("scripts", "semantic_analysis.R"))
source(file.path("scripts", "table_detection.R"))
source(file.path("scripts", "enhanced_output_builder.R"))
source(file.path("scripts", "enhanced_manifest_builder.R"))

setup_r_environment()

argv <- parse_args(args)
if (is.null(argv$year) || is.null(argv$pdf)) {
	stop("Usage: Rscript scripts/preprocess_pdf_unified.R --year YYYY --pdf path/to/catalog.pdf [--pages 1,2,3]")
}

year <- as.integer(argv$year)
pdf_path <- argv$pdf
pages_arg <- if (!is.null(argv$pages)) argv$pages else NULL

if (!file.exists(pdf_path)) {
	stop(sprintf("PDF not found: %s", pdf_path))
}

dirs <- setup_directories(year)
dir_interim <- dirs$dir_interim
dir_pages <- dirs$dir_pages

# Determine if PDF is digital (glyphs present) or scanned (images only), prioritizing pdf_data glyphs
detect_pdf_type <- function(pdf_path, pages_data = NULL) {
	# Prefer glyph presence via pdf_data on first 1-3 pages
	pages_data_local <- pages_data
	if (is.null(pages_data_local)) {
		pages_data_local <- try(pdftools::pdf_data(pdf_path), silent = TRUE)
	}
	if (!inherits(pages_data_local, "try-error") && !is.null(pages_data_local)) {
		max_pages <- min(3L, length(pages_data_local))
		for (i in seq_len(max_pages)) {
			pg <- pages_data_local[[i]]
			if (!is.null(pg) && nrow(pg) > 0) {
				# Consider digital if any glyph text exists
				if ("text" %in% names(pg) && sum(nchar(pg$text), na.rm = TRUE) > 0) return("digital")
			}
		}
	}
	# Fallback to simple pdf_text presence (non-empty)
	txt <- try(pdftools::pdf_text(pdf_path)[1], silent = TRUE)
	if (!inherits(txt, "try-error") && !is.null(txt) && nchar(gsub("\\s+", "", txt)) > 0) {
		return("digital")
	}
	"scanned"
}

# Map cld2 language codes to tesseract traineddata codes; fallback to eng
map_to_tesseract_lang <- function(text_sample, default = "eng") {
	if (is.null(text_sample) || nchar(text_sample) == 0) return(default)
	detected <- try(cld2::detect_language(text_sample, plain_text = TRUE), silent = TRUE)
	if (inherits(detected, "try-error") || is.null(detected) || nchar(detected) == 0) return(default)
	code2 <- tolower(detected)
	mapping <- list(
		en = "eng", es = "spa", fr = "fra", de = "deu", it = "ita", pt = "por",
		"pt-pt" = "por", "pt-br" = "por", nl = "nld", da = "dan", no = "nor", nb = "nor", nn = "nor",
		sv = "swe", fi = "fin", tr = "tur", pl = "pol", cs = "ces", sk = "slk", hu = "hun", ro = "ron",
		ru = "rus", uk = "ukr", bg = "bul", el = "ell", sr = "srp", hr = "hrv", bs = "bos", sl = "slv",
		et = "est", lv = "lav", lt = "lit", zh = "chi_sim", "zh-cn" = "chi_sim", "zh-tw" = "chi_tra",
		ja = "jpn", ko = "kor", vi = "vie", th = "tha", hi = "hin", bn = "ben", ta = "tam", te = "tel",
		gu = "guj", pa = "pan", ur = "urd", ar = "ara", he = "heb", fa = "fas", id = "ind", ms = "msa",
		sw = "swa", af = "afr", ga = "gle", mt = "mlt", is = "isl", sq = "sqi", mk = "mkd", ka = "kat",
		hy = "hye", az = "aze", kk = "kaz", ky = "kir", mn = "mon", ne = "nep", si = "sin", am = "amh",
		my = "mya", km = "khm", lo = "lao", ps = "pus"
	)
	if (!is.null(mapping[[code2]])) return(mapping[[code2]])
	default
}

# Ensure OCR language is installed; fallback to eng and warn if missing
ensure_ocr_language <- function(lang_code, fallback = "eng") {
	available <- try(tesseract::tesseract_info()$available, silent = TRUE)
	if (inherits(available, "try-error") || is.null(available)) {
		warning("Unable to query tesseract languages; defaulting to 'eng'.")
		return(fallback)
	}
	if (lang_code %in% available) return(lang_code)
	warning(sprintf("OCR language '%s' not installed. Falling back to '%s'.", lang_code, fallback))
	fallback
}

# Functions for geometry-based processing (now using enhanced tokens_to_words from utils)
# Legacy chars_to_words function maintained for backward compatibility
chars_to_words <- function(d) {
  # Delegate to the new tokens_to_words function
  return(tokens_to_words(d))
}

detect_columns_legacy <- function(words_df) {
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

words_to_lines_legacy <- function(words_df, page_height, y_threshold = 8, semantic_blocks = NULL) {
  if (nrow(words_df) == 0) return(list())
  words_df$visual_y <- page_height - words_df$y
  lines <- list()
  idx <- 1L
  
  # Store semantic blocks for bbox overlap matching
  semantic_blocks_data <- semantic_blocks
  
  for (col in sort(unique(words_df$column_id))) {
    wcol <- words_df[words_df$column_id == col, , drop = FALSE]
    wcol <- wcol[order(wcol$visual_y, wcol$x), , drop = FALSE]
    current_y <- NA_real_
    current_words <- list()
    flush_line <- function() {
      if (length(current_words) > 0) {
        line_text <- paste(vapply(current_words, function(w) w$text, character(1)), collapse = " ")
        
        # Find semantic classification for this line using bbox overlap
        line_block_type <- "body_text"
        line_confidence <- 0.5
        highest_confidence <- 0.0
        
        if (!is.null(semantic_blocks_data) && length(semantic_blocks_data) > 0) {
          # Calculate line bounding box from all words in the line
          line_x1 <- min(vapply(current_words, function(w) w$x, numeric(1)), na.rm = TRUE)
          line_x2 <- max(vapply(current_words, function(w) w$x + w$width, numeric(1)), na.rm = TRUE)
          line_y1 <- min(vapply(current_words, function(w) w$y, numeric(1)), na.rm = TRUE)
          line_y2 <- max(vapply(current_words, function(w) w$y + w$height, numeric(1)), na.rm = TRUE)
          
          # Check each semantic block for overlap with this line
          for (block in semantic_blocks_data) {
            if (!is.null(block$position)) {
              block_x1 <- block$position$x
              block_y1 <- block$position$y
              block_x2 <- block$position$x + block$position$width
              block_y2 <- block$position$y + block$position$height
              
              # Calculate overlap area
              x_overlap <- max(0, min(line_x2, block_x2) - max(line_x1, block_x1))
              y_overlap <- max(0, min(line_y2, block_y2) - max(line_y1, block_y1))
              overlap_area <- x_overlap * y_overlap
              
              # If there's overlap and this block has higher confidence, use it
              if (overlap_area > 0 && block$classification_confidence > highest_confidence) {
                line_block_type <- block$block_type
                line_confidence <- block$classification_confidence
                highest_confidence <- block$classification_confidence
              }
            }
          }
        }
        
        lines[[length(lines) + 1]] <<- list(
          line_id = sprintf("line_%04d", idx),
          column_id = col,
          text = trimws(line_text),
          block_type = line_block_type,
          classification_confidence = line_confidence
        )
        idx <<- idx + 1L
      }
      current_words <<- list()
      current_y <<- NA_real_
    }
    for (i in seq_len(nrow(wcol))) {
      vy <- wcol$visual_y[i]
      if (is.na(current_y) || abs(vy - current_y) <= y_threshold) {
        current_words <- c(current_words, list(list(
          text = wcol$text[i],
          x = wcol$x[i],
          y = wcol$y[i],
          width = wcol$width[i],
          height = wcol$height[i]
        )))
        current_y <- if (is.na(current_y)) vy else (current_y * 0.8 + vy * 0.2)
      } else {
        flush_line()
        current_words <- list(list(
          text = wcol$text[i],
          x = wcol$x[i],
          y = wcol$y[i],
          width = wcol$width[i],
          height = wcol$height[i]
        ))
        current_y <- vy
      }
    }
    flush_line()
  }
  lines
}

# Normalize OCR result to pseudo-char data frame expected by chars_to_words
ocr_to_char_df <- function(ocr_page) {
	# pdf_ocr_data returns a data.frame similar to pdf_data with x, y, width, height, word, space
	if (is.null(ocr_page) || nrow(ocr_page) == 0) return(data.frame())
	d <- ocr_page
	# harmonize names
	if ("word" %in% names(d)) names(d)[names(d) == "word"] <- "text"
	if (!"space" %in% names(d)) d$space <- FALSE
	d[, c("x", "y", "width", "height", "space", "text")]
}

 

## Replaced legacy page processors with dedicated extraction functions in scripts/extraction_functions.R

info <- pdftools::pdf_info(pdf_path)
all_pages <- seq_len(info$pages)
if (!is.null(pages_arg)) {
	sel <- as.integer(strsplit(pages_arg, ",")[[1]])
	sel <- sel[sel %in% all_pages]
	if (length(sel) == 0) stop("No valid pages specified in --pages")
	all_pages <- sel
}

# Precompute pdf_data and fonts once for efficiency
pages_data <- try(pdftools::pdf_data(pdf_path), silent = TRUE)
if (inherits(pages_data, "try-error")) pages_data <- NULL

# Precompute fonts once for the entire document
fonts <- try(pdftools::pdf_fonts(pdf_path), silent = TRUE)
if (inherits(fonts, "try-error")) fonts <- NULL

type_detected <- detect_pdf_type(pdf_path, pages_data = pages_data)
first_text <- try(pdftools::pdf_text(pdf_path)[1], silent = TRUE)
detected_lang <- map_to_tesseract_lang(if (inherits(first_text, "try-error")) "" else first_text)
ocr_lang <- ensure_ocr_language(detected_lang)
ocr_performed <- FALSE

cat(sprintf("Unified preprocessing on %d page(s). Type: %s, OCR lang: %s\n", length(all_pages), type_detected, ocr_lang))

page_files <- list()
pages_success <- integer(0)
extraction_counts <- list(digital = 0L, ocr = 0L)
processing_results <- list()  # Store processing results for enhanced manifest

# Load settings to check enhanced output configuration
settings <- load_settings()

# Check enhanced output flags
enhanced_output_enabled <- if (!is.null(settings$enhanced_output$enable_enhanced_json)) {
	settings$enhanced_output$enable_enhanced_json
} else {
	TRUE  # Default to enabled
}

generate_legacy_geometry <- if (!is.null(settings$output_formats$generate_legacy_geometry)) {
	settings$output_formats$generate_legacy_geometry
} else {
	TRUE  # Default to enabled
}

generate_enhanced_page <- if (!is.null(settings$output_formats$generate_enhanced_page)) {
	settings$output_formats$generate_enhanced_page
} else {
	enhanced_output_enabled  # Use enhanced_output_enabled as fallback
}

generate_extraction_json <- if (!is.null(settings$output_formats$generate_extraction_json)) {
	settings$output_formats$generate_extraction_json
} else {
	TRUE  # Default to enabled
}

generate_llm_json <- if (!is.null(settings$output_formats$generate_llm_json)) {
	settings$output_formats$generate_llm_json
} else {
	TRUE  # Default to enabled
}

for (page_num in all_pages) {
	# Capture start time for realistic processing timing
	page_start_time <- Sys.time()
	cat(sprintf("Page %d ... ", page_num))
	flush.console()
	res <- NULL
	# Gate extraction strategy based on detected PDF type for efficiency
	if (identical(type_detected, "scanned")) {
		# Configure OCR parameters from settings
		ocr_params <- list(
			source = "pdf_ocr_data",
			retry_attempts = settings$ocr_settings$ocr_retry_attempts %||% 2,
			timeout_seconds = settings$ocr_settings$tesseract_timeout_seconds %||% 300,
			fallback_language = settings$ocr_settings$fallback_language %||% "eng"
		)
		
		ext <- try(extract_scanned_pdf(
			pdf_path,
			page_num,
			ocr_lang = ocr_lang,
			parameters = ocr_params
		), silent = TRUE)
		
		if (!(inherits(ext, "try-error") || is.null(ext))) {
			ocr_performed <- TRUE
			extraction_counts$ocr <- extraction_counts$ocr + 1L
		}
	} else {
		# Configure OCR parameters from settings
		ocr_params <- list(
			source = "pdf_ocr_data",
			retry_attempts = settings$ocr_settings$ocr_retry_attempts %||% 2,
			timeout_seconds = settings$ocr_settings$tesseract_timeout_seconds %||% 300,
			fallback_language = settings$ocr_settings$fallback_language %||% "eng"
		)
		
		# Try digital extraction first, with OCR fallback for robustness
		ext <- try(extract_digital_pdf(
			pdf_path,
			page_num,
			language = detected_lang,
			parameters = list(source = "pdf_data"),
			pages_data = pages_data,
			fonts = fonts
		), silent = TRUE)
		
		if (inherits(ext, "try-error") || is.null(ext)) {
			ext <- try(extract_scanned_pdf(
				pdf_path,
				page_num,
				ocr_lang = ocr_lang,
				parameters = ocr_params
			), silent = TRUE)
			
			if (!(inherits(ext, "try-error") || is.null(ext))) {
				ocr_performed <- TRUE
				extraction_counts$ocr <- extraction_counts$ocr + 1L
			}
		} else {
			extraction_counts$digital <- extraction_counts$digital + 1L
		}
	}
	res <- ext
	if (inherits(res, "try-error") || is.null(res)) {
		cat("skip\n")
		next
	}

	# Build geometry/LLM outputs from normalized extraction result
	text_df <- if (length(res$text_data) > 0) do.call(rbind, lapply(res$text_data, function(x) as.data.frame(x, stringsAsFactors = FALSE))) else data.frame()
	page_width_local <- res$page_dimensions$width
	page_height_local <- res$page_dimensions$height
	
	# Initialize semantic analysis variables and define safe defaults
	ocr_threshold <- settings$text_processing$ocr_confidence_threshold %||% 0.6
	font_stats <- list(font_sizes = numeric(0))
	words_with_features <- data.frame()
	semantic_analysis_result <- NULL
	enhanced_words <- data.frame()
	
	if (!is.null(text_df) && nrow(text_df) > 0) {
		if (!"space" %in% names(text_df)) text_df$space <- FALSE
		
		# Use enhanced tokens_to_words function with configured threshold and extraction method
		words <- tokens_to_words(text_df, min_confidence = ocr_threshold, method = res$extraction_method)
		if (nrow(words) == 0) words <- text_df[, c("text","x","y","width","height")]
		
		# Analyze font characteristics from glyph-level data
		font_analysis_result <- analyze_font_characteristics(text_df, res$font_data)
		enhanced_text_df <- font_analysis_result$text_data
		font_stats <- font_analysis_result$font_stats
		
		# Aggregate font features from enhanced text_df onto words by bbox overlap
		words_with_features <- aggregate_font_features_to_words(words, enhanced_text_df)
		
		# Apply semantic analysis using words with aggregated features
		semantic_analysis_result <- analyze_page_semantics(
			words_with_features, 
			font_stats, 
			list(width = page_width_local, height = page_height_local)
		)
		
		# Continue with enhanced column detection and line processing
		col_result <- detect_columns_enhanced(
			words, 
			page_dimensions = list(width = page_width_local, height = page_height_local),
			method = res$extraction_method,
			settings = settings
		)
		cols <- col_result$columns
		column_detection_metadata <- col_result$metadata
		
		# Check for quality-based fallback strategy
		fallback_triggered <- FALSE
		if (!is.null(settings$fallback_strategies$enable_quality_fallback) && settings$fallback_strategies$enable_quality_fallback) {
			should_fallback <- FALSE
			
			# Check quality thresholds based on extraction method
			if (res$extraction_method == "digital") {
				digital_threshold <- settings$fallback_strategies$digital_quality_threshold %||% 0.35
				if (!is.null(res$quality_scores$overall_quality) && res$quality_scores$overall_quality < digital_threshold) {
					should_fallback <- TRUE
				}
			} else if (res$extraction_method == "ocr") {
				ocr_threshold <- settings$fallback_strategies$ocr_confidence_threshold %||% 0.55
				if (!is.null(res$confidence_metrics$mean_confidence) && res$confidence_metrics$mean_confidence < ocr_threshold) {
					should_fallback <- TRUE
				}
			}
			
			# Check for single column detection with high text density
			single_column_threshold <- settings$fallback_strategies$single_column_density_threshold %||% 0.3
			if (length(cols) == 1 && !is.null(res$quality_scores$text_density) && res$quality_scores$text_density > single_column_threshold) {
				should_fallback <- TRUE
			}
			
			# Apply fallback if needed
			if (should_fallback && column_detection_metadata$method_used != "kmeans") {
				fallback_settings <- settings
				fallback_settings$column_detection$force_kmeans <- TRUE
				fallback_result <- detect_columns_enhanced(
					words,
					page_dimensions = list(width = page_width_local, height = page_height_local),
					method = res$extraction_method,
					settings = fallback_settings
				)
				if (fallback_result$metadata$detection_confidence > column_detection_metadata$detection_confidence) {
					cols <- fallback_result$columns
					column_detection_metadata <- fallback_result$metadata
					column_detection_metadata$fallback_triggered <- TRUE
					fallback_triggered <- TRUE
				}
			}
		}
		
		words <- assign_columns(words, cols)
		lines <- words_to_lines(words, page_height_local, semantic_blocks = semantic_analysis_result$semantic_blocks, reading_order = "ltr")
		
		# Add table detection
		table_result <- detect_tables(words, lines, settings)
		
		# Generate enhanced output if enabled
		enhanced_output <- NULL
		if (enhanced_output_enabled && generate_enhanced_page) {
			# Calculate processing time for this point
			current_time <- Sys.time()
			current_processing_time <- as.numeric(difftime(current_time, page_start_time, units = "secs"))
			
			enhanced_output <- build_enhanced_page_output(
				extraction_result = res,
				semantic_analysis = semantic_analysis_result,
				font_analysis = font_analysis_result,
				table_detection = table_result,
				column_detection = list(columns = cols, metadata = column_detection_metadata),
				page_info = list(
					page_number = page_num,
					page_width = page_width_local,
					page_height = page_height_local
				),
				processing_settings = settings,
				processing_time = current_processing_time,
				words = words,
				lines = lines
			)
		}
	} else {
		words <- data.frame()
		cols <- list(list(column_id = 1L, x_start = -Inf, x_end = Inf))
		lines <- list()
		semantic_analysis_result <- analyze_page_semantics(
			data.frame(),
			list(font_sizes = numeric(0), dominant_fonts = character(0), size_distribution = list(median = NA, min = NA, max = NA), has_font_variation = FALSE),
			list(width = page_width_local, height = page_height_local)
		)
		column_detection_metadata <- list(
			method_used = "none",
			detection_confidence = 0.0,
			fallback_triggered = FALSE,
			attempts = 0L
		)
		table_result <- list(
			tables = list(),
			processing_metadata = list(
				tables_detected = 0L,
				detection_enabled = FALSE,
				detection_method = "disabled"
			)
		)
		
		# Generate enhanced output for empty page if enabled
		enhanced_output <- NULL
		if (enhanced_output_enabled && generate_enhanced_page) {
			# Calculate processing time for this point
			current_time <- Sys.time()
			current_processing_time <- as.numeric(difftime(current_time, page_start_time, units = "secs"))
			
			enhanced_output <- build_enhanced_page_output(
				extraction_result = res,
				semantic_analysis = semantic_analysis_result,
				font_analysis = NULL,
				table_detection = table_result,
				column_detection = list(columns = cols, metadata = column_detection_metadata),
				page_info = list(
					page_number = page_num,
					page_width = page_width_local,
					page_height = page_height_local
				),
				processing_settings = settings,
				processing_time = current_processing_time
			)
		}
	}

	# Sanitize column bounds to finite values for JSON consumers
	sanitized_cols <- lapply(cols, function(c) {
		list(
			column_id = c$column_id,
			x_start = if (is.finite(c$x_start)) c$x_start else 0,
			x_end = if (is.finite(c$x_end)) c$x_end else page_width_local
		)
	})

	# Construct processing_metadata from settings and semantic analysis result
	processing_metadata <- list(
		semantic_analysis_enabled = !is.null(semantic_analysis_result) && length(semantic_analysis_result$semantic_blocks) > 0,
		ocr_confidence_threshold = ocr_threshold,
		font_analysis_available = !is.null(font_stats) && length(font_stats$font_sizes) > 0,
		classification_method = if (!is.null(semantic_analysis_result) && !is.null(semantic_analysis_result$processing_metadata)) {
			semantic_analysis_result$processing_metadata$classification_method %||% "font_plus_regex"
		} else {
			"basic_geometry"
		},
		table_detection_enabled = !is.null(table_result) && table_result$processing_metadata$detection_enabled,
		tables_detected = if (!is.null(table_result)) table_result$processing_metadata$tables_detected else 0L,
		column_detection_method = column_detection_metadata$method_used,
		quality_fallback_used = column_detection_metadata$fallback_triggered %||% FALSE
	)
	
	# Write geometry file if enabled
	geometry_filename <- NULL
	if (generate_legacy_geometry) {
		geometry_json <- list(
			page_number = res$page_number,
			total_pages = info$pages,
			columns_detected = length(sanitized_cols),
			columns = sanitized_cols,
			blocks = lines,
			page_width = page_width_local,
			page_height = page_height_local,
			method = paste0("pdf_geometry+", res$extraction_method),
			processing_timestamp = as.character(Sys.time()),
			# Enhanced fields for semantic analysis
			font_analysis = if (!is.null(semantic_analysis_result)) semantic_analysis_result$font_analysis else list(),
			semantic_blocks = if (!is.null(semantic_analysis_result)) semantic_analysis_result$semantic_blocks else list(),
			processing_method = if (nrow(words_with_features) > 0 && "font_style" %in% names(words_with_features)) "enhanced_semantic" else "basic_geometry",
			# New fields for table detection and enhanced column detection
			tables = if (!is.null(table_result)) table_result$tables else list(),
			column_detection_metadata = column_detection_metadata,
			# Processing metadata as per schema
			processing_metadata = processing_metadata
		)
		geometry_filename <- sprintf("page_%04d.geometry.json", page_num)
		geometry_filepath <- file.path(dir_pages, geometry_filename)
		jsonlite::write_json(geometry_json, geometry_filepath, pretty = TRUE, auto_unbox = TRUE, null = "null")
	}

	lines_df <- lines
	num_cols <- length(sanitized_cols)
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

	# Write LLM file if enabled
	llm_filename <- NULL
	if (generate_llm_json) {
		llm_json <- list(
			page_number = res$page_number,
			columns_detected = num_cols,
			columns_text = columns_text
		)
		llm_filename <- sprintf("page_%04d.llm.json", page_num)
		llm_filepath <- file.path(dir_pages, llm_filename)
		jsonlite::write_json(llm_json, llm_filepath, pretty = TRUE, auto_unbox = TRUE, null = "null")
	}

	# Write standardized extraction result if enabled
	extraction_filename <- NULL
	if (generate_extraction_json) {
		extraction_filename <- sprintf("page_%04d.extraction.json", page_num)
		extraction_filepath <- file.path(dir_pages, extraction_filename)
		jsonlite::write_json(res, extraction_filepath, pretty = TRUE, auto_unbox = TRUE, null = "null")
	}

	# Write enhanced output if available
	enhanced_filename <- NULL
	if (!is.null(enhanced_output)) {
		enhanced_filename <- sprintf("page_%04d.enhanced.json", page_num)
		enhanced_filepath <- file.path(dir_pages, enhanced_filename)
		jsonlite::write_json(enhanced_output, enhanced_filepath, pretty = TRUE, auto_unbox = TRUE, null = "null")
	}

	# Calculate actual processing time
	page_end_time <- Sys.time()
	processing_time_secs <- as.numeric(difftime(page_end_time, page_start_time, units = "secs"))
	
	# Collect processing result for enhanced manifest
	processing_result <- list(
		page_number = page_num,
		success = TRUE,
		extraction_method = res$extraction_method,
		quality_score = if (!is.null(enhanced_output)) enhanced_output$quality_assessment$overall_quality else 0.5,
		overall_confidence = if (!is.null(enhanced_output)) enhanced_output$extraction_details$overall_confidence else list(mean = 1.0),
		language = if (!is.null(enhanced_output)) enhanced_output$extraction_details$language_detection else NULL,
		processing_time = processing_time_secs,
		start_time = page_start_time,
		end_time = page_end_time,
		errors = NULL,
		warnings = NULL
	)
	
	# Add fallback methods if available from extraction result
	if (!is.null(res$fallback_methods)) {
		processing_result$fallback_methods <- res$fallback_methods
	}
	
	# Add mixed methods detection if available from enhanced output
	if (!is.null(enhanced_output) && !is.null(enhanced_output$extraction_details$fallback_methods)) {
		processing_result$fallback_methods <- enhanced_output$extraction_details$fallback_methods
	}
	
	# Check for mixed method usage in metadata
	if (!is.null(res$processing_metadata$mixed_methods_used)) {
		processing_result$processing_metadata <- list(mixed_methods_used = res$processing_metadata$mixed_methods_used)
	}
	processing_results[[length(processing_results) + 1]] <- processing_result

	# Update page files list (only include files that were actually generated)
	generated_files <- character(0)
	if (!is.null(geometry_filename)) generated_files <- c(generated_files, geometry_filename)
	if (!is.null(llm_filename)) generated_files <- c(generated_files, llm_filename)
	if (!is.null(extraction_filename)) generated_files <- c(generated_files, extraction_filename)
	if (!is.null(enhanced_filename)) generated_files <- c(generated_files, enhanced_filename)
	
	page_files <- c(page_files, generated_files)
	pages_success <- c(pages_success, page_num)
	cat("done\n")
}

# Generate enhanced manifest
manifest_path <- file.path(dir_interim, "manifest.json")
text_method <- if (extraction_counts$digital > 0L && extraction_counts$ocr > 0L) "mixed" else if (extraction_counts$digital > 0L) "pdf_data" else "pdf_ocr_data"

# Build enhanced manifest
document_info <- list(
	catalog_year = year,
	source_file = basename(pdf_path),
	file_size = file.info(pdf_path)$size,
	document_title = NULL,  # Could be extracted from PDF metadata
	creation_date = NULL    # Could be extracted from PDF metadata
)

enhanced_manifest <- build_enhanced_manifest(
	processing_results = processing_results,
	document_info = document_info,
	processing_settings = settings,
	output_directory = dir_pages
)

# Write enhanced manifest
if (!is.null(enhanced_manifest)) {
	jsonlite::write_json(enhanced_manifest, manifest_path, pretty = TRUE, auto_unbox = TRUE, null = "null")
} else {
	# Fallback to legacy manifest if enhanced generation fails
	legacy_manifest <- list(
		catalog_year = year,
		file = normalizePath(pdf_path, winslash = "/", mustWork = FALSE),
		file_id = file_checksum(pdf_path),
		page_count = info$pages,
		pages_processed = length(unique(pages_success)),
		files_written = length(page_files),
		processing_timestamp = as.character(Sys.time()),
		extractor = list(name = "unified_preprocess"),
		pdf_type = type_detected,
		ocr_language = ocr_lang,
		ocr_performed = ocr_performed,
		text_extraction_summary = list(digital = extraction_counts$digital, ocr = extraction_counts$ocr),
		text_extraction_method = text_method
	)
	jsonlite::write_json(legacy_manifest, manifest_path, pretty = TRUE, auto_unbox = TRUE, null = "null")
}

cat(sprintf("Completed unified preprocessing. Files written: %d. Manifest: %s\n", length(page_files), manifest_path))


