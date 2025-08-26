#!/usr/bin/env Rscript

# Core extraction functions for digital and scanned PDFs

extract_digital_pdf <- function(pdf_path, page_num, language = NULL, parameters = list(), pages_data = NULL, fonts = NULL) {
  # Use precomputed pages_data if provided, otherwise compute it
  if (is.null(pages_data)) {
    pages_data <- try(pdftools::pdf_data(pdf_path, pages = page_num), silent = TRUE)
    if (inherits(pages_data, "try-error") || is.null(pages_data) || length(pages_data) == 0) {
      return(NULL)
    }
    d <- pages_data[[1]]  # Single page when using pages argument
  } else {
    if (length(pages_data) < page_num) {
      return(NULL)
    }
    d <- pages_data[[page_num]]
  }
  if (is.null(d) || nrow(d) == 0) {
    return(NULL)
  }
  # Ensure required columns exist
  for (nm in c("x","y","width","height","space","text")) if (!nm %in% names(d)) d[[nm]] <- NA
  d$space[is.na(d$space)] <- FALSE
  
  # Detect optional glyph-level font fields for inclusion
  optional_cols <- intersect(c("fontname", "font", "size"), names(d))
  all_cols <- c("text","x","y","width","height","space", optional_cols)

  # Try to get page dimensions from pdftools::pdf_pagesize for more reliability
  page_dims <- try(pdftools::pdf_pagesize(pdf_path), silent = TRUE)
  if (!inherits(page_dims, "try-error") && !is.null(page_dims) && nrow(page_dims) >= page_num) {
    page_width <- page_dims$width[page_num]
    page_height <- page_dims$height[page_num]
  } else {
    # Fallback to estimating from text box coordinates (current heuristic)
    page_width <- try(max(d$x + d$width, na.rm = TRUE), silent = TRUE)
    page_height <- try(max(d$y, na.rm = TRUE), silent = TRUE)
    if (!is.finite(page_width)) page_width <- NA_real_
    if (!is.finite(page_height)) page_height <- NA_real_
  }

  # Use precomputed fonts if provided, otherwise compute them
  font_df <- NULL
  if (is.null(fonts)) {
    fonts <- try(pdftools::pdf_fonts(pdf_path), silent = TRUE)
    if (inherits(fonts, "try-error") || is.null(fonts)) fonts <- NULL
  }
  
  if (!is.null(fonts) && nrow(fonts) > 0) {
    # Filter fonts to current page if page information is available
    if ("page" %in% names(fonts)) {
      font_df <- subset(fonts, is.na(page) | page == page_num)
    } else {
      # Attach page number to allow page-level association if not available
      fonts$page <- NA_integer_
      font_df <- fonts
    }
  }

  # Create synthetic confidence of 1.0 for digital extraction
  confidence_metrics <- list(mean_confidence = 1.0, confidence_distribution = list(min = 1.0, p25 = 1.0, median = 1.0, p75 = 1.0, max = 1.0, count = nrow(d)), low_confidence_count = 0L)

  normalize_extraction_data(
    page_number = page_num,
    extraction_method = "digital",
    text_df = d[, all_cols, drop = FALSE],
    page_width = page_width,
    page_height = page_height,
    font_df = font_df,
    language = language,
    parameters = parameters,
    confidence_metrics = confidence_metrics
  )
}

extract_scanned_pdf <- function(pdf_path, page_num, ocr_lang = "eng", parameters = list()) {
  ocr <- try(pdftools::pdf_ocr_data(pdf_path, pages = page_num, language = ocr_lang), silent = TRUE)
  if (inherits(ocr, "try-error") || is.null(ocr)) {
    return(NULL)
  }
  page_df <- ocr[[1]]
  if (is.null(page_df) || nrow(page_df) == 0) {
    return(NULL)
  }
  # Ensure expected fields
  if ("word" %in% names(page_df)) names(page_df)[names(page_df) == "word"] <- "text"
  if (!"space" %in% names(page_df)) page_df$space <- FALSE
  if (!"confidence" %in% names(page_df)) page_df$confidence <- NA_real_
  
  # Normalize confidence values to 0-1 range if they appear to be in 0-100 range
  if ("confidence" %in% names(page_df)) {
    mx <- suppressWarnings(max(page_df$confidence, na.rm = TRUE))
    if (is.finite(mx) && mx > 1) page_df$confidence <- page_df$confidence / 100
  }
  # When only bbox is present, derive x, y, width, height
  if (!all(c("x", "y", "width", "height") %in% names(page_df)) && "bbox" %in% names(page_df)) {
    # bbox format: "x1,y1,x2,y2"
    split_bbox <- do.call(rbind, strsplit(as.character(page_df$bbox), ","))
    suppressWarnings({
      x1 <- as.numeric(split_bbox[, 1])
      y1 <- as.numeric(split_bbox[, 2])
      x2 <- as.numeric(split_bbox[, 3])
      y2 <- as.numeric(split_bbox[, 4])
    })
    page_df$x <- x1
    page_df$y <- y2
    page_df$width <- pmax(0, x2 - x1)
    page_df$height <- pmax(0, y2 - y1)
  }

  # Try to get page dimensions from pdftools::pdf_pagesize for more reliability
  page_dims <- try(pdftools::pdf_pagesize(pdf_path), silent = TRUE)
  if (!inherits(page_dims, "try-error") && !is.null(page_dims) && nrow(page_dims) >= page_num) {
    page_width <- page_dims$width[page_num]
    page_height <- page_dims$height[page_num]
  } else {
    # Fallback to estimating from OCR bounding boxes (current heuristic)
    page_width <- try(max(page_df$x + page_df$width, na.rm = TRUE), silent = TRUE)
    page_height <- try(max(page_df$y, na.rm = TRUE), silent = TRUE)
    if (!is.finite(page_width)) page_width <- NA_real_
    if (!is.finite(page_height)) page_height <- NA_real_
  }

  conf <- calculate_text_confidence(page_df)
  
  # Include optional fields if available in OCR data
  optional_ocr_cols <- intersect(c("fontname", "font", "size"), names(page_df))
  all_ocr_cols <- c("text","x","y","width","height","space","confidence", optional_ocr_cols)

  normalize_extraction_data(
    page_number = page_num,
    extraction_method = "ocr",
    text_df = page_df[, all_ocr_cols, drop = FALSE],
    page_width = page_width,
    page_height = page_height,
    font_df = NULL,
    language = ocr_lang,
    parameters = parameters,
    confidence_metrics = conf
  )
}


