#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

source(file.path("scripts", "utils_preprocess.R"))

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

# Functions for geometry-based processing (reused from columns script)
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
      current <- list(text = ch$text, x = ch$x, y = ch$y, width = ch$width, height = ch$height)
    } else {
      gap <- ch$x - (current$x + current$width)
      if (isTRUE(ch$space) || gap > max(6, 1.0 * ch$height)) {
        flush_word()
        current <- list(text = ch$text, x = ch$x, y = ch$y, width = ch$width, height = ch$height)
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

 

process_page_digital <- function(pages_data, page_num) {
	if (is.null(pages_data) || length(pages_data) < page_num) return(NULL)
	d <- pages_data[[page_num]]
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
	list(page_number = page_num, words = words, lines = lines, columns = cols, page_width = page_width, page_height = page_height,
	     method = "pdf_geometry", text_extraction_method = "pdf_data")
}

process_page_scanned <- function(pdf_path, page_num, ocr_lang) {
	ocr <- try(pdftools::pdf_ocr_data(pdf_path, pages = page_num, language = ocr_lang), silent = TRUE)
	if (inherits(ocr, "try-error") || is.null(ocr)) return(NULL)
	d <- ocr_to_char_df(ocr[[1]])
	if (nrow(d) == 0) return(NULL)
	page_width <- max(d$x + d$width, na.rm = TRUE)
	page_height <- max(d$y, na.rm = TRUE)
	# Treat OCR tokens as words directly
	words <- d[, c("text", "x", "y", "width", "height")]
	cols <- detect_columns(words)
	words <- assign_columns(words, cols)
	lines <- words_to_lines(words, page_height)
	list(page_number = page_num, words = words, lines = lines, columns = cols, page_width = page_width, page_height = page_height,
	     method = "pdf_geometry", text_extraction_method = "pdf_ocr_data")
}

info <- pdftools::pdf_info(pdf_path)
all_pages <- seq_len(info$pages)
if (!is.null(pages_arg)) {
	sel <- as.integer(strsplit(pages_arg, ",")[[1]])
	sel <- sel[sel %in% all_pages]
	if (length(sel) == 0) stop("No valid pages specified in --pages")
	all_pages <- sel
}

# Precompute pdf_data once for efficiency
pages_data <- try(pdftools::pdf_data(pdf_path), silent = TRUE)
if (inherits(pages_data, "try-error")) pages_data <- NULL

type_detected <- detect_pdf_type(pdf_path, pages_data = pages_data)
first_text <- try(pdftools::pdf_text(pdf_path)[1], silent = TRUE)
detected_lang <- map_to_tesseract_lang(if (inherits(first_text, "try-error")) "" else first_text)
ocr_lang <- ensure_ocr_language(detected_lang)
ocr_performed <- FALSE

cat(sprintf("Unified preprocessing on %d page(s). Type: %s, OCR lang: %s\n", length(all_pages), type_detected, ocr_lang))

page_files <- list()
pages_success <- integer(0)
extraction_counts <- list(digital = 0L, ocr = 0L)

for (page_num in all_pages) {
	cat(sprintf("Page %d ... ", page_num))
	flush.console()
	res <- NULL
	# Always attempt digital first, then OCR fallback
	res <- try(process_page_digital(pages_data, page_num), silent = TRUE)
	if (inherits(res, "try-error") || is.null(res)) {
		res <- try(process_page_scanned(pdf_path, page_num, ocr_lang), silent = TRUE)
		if (!(inherits(res, "try-error") || is.null(res))) {
			ocr_performed <- TRUE
			extraction_counts$ocr <- extraction_counts$ocr + 1L
		}
	}
	if (!(inherits(res, "try-error") || is.null(res)) && res$text_extraction_method == "pdf_data") {
		extraction_counts$digital <- extraction_counts$digital + 1L
	}
	if (inherits(res, "try-error") || is.null(res)) {
		cat("skip\n")
		next
	}

	geometry_json <- list(
		page_number = res$page_number,
		total_pages = info$pages,
		columns_detected = length(res$columns),
		columns = res$columns,
		blocks = res$lines,
		page_width = res$page_width,
		page_height = res$page_height,
		method = res$method,
		processing_timestamp = as.character(Sys.time())
	)
	geometry_filename <- sprintf("page_%04d.geometry.json", page_num)
	geometry_filepath <- file.path(dir_pages, geometry_filename)
	jsonlite::write_json(geometry_json, geometry_filepath, pretty = TRUE, auto_unbox = TRUE, null = "null")

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
	pages_success <- c(pages_success, page_num)
	cat("done\n")
}

# Update manifest with enhanced fields
manifest_path <- file.path(dir_interim, "manifest.json")
text_method <- if (extraction_counts$digital > 0L && extraction_counts$ocr > 0L) "mixed" else if (extraction_counts$digital > 0L) "pdf_data" else "pdf_ocr_data"
manifest <- list(
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
jsonlite::write_json(manifest, manifest_path, pretty = TRUE, auto_unbox = TRUE, null = "null")

cat(sprintf("Completed unified preprocessing. Files written: %d. Manifest: %s\n", length(page_files), manifest_path))


