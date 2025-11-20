#' QA/QC for step 3 input data (waterlevel conversion)
#'
#' Ensures required columns exist, logger_type looks reasonable, timestamp is POSIXct,
#' and does some light range checks on temperature and pressure.
#'
#' @param input_data Data frame passed into convert_waterlevel_kPa_m().
#'        Must contain at least:
#'        site_station_code, timestamp, waterpress_kPa, watertemp_C, baro_data.
#' @param logger_type_expected Character pattern for allowed logger types
#'        (e.g., "u20"), matched case-insensitively in `logger_type` if present.
#' @param fun_name Name of the calling function (for nicer error messages).
#'
#' @return input_data with timestamp coerced to POSIXct if needed.


QAQC_wl_kpa_inputs <- function(input_data,
                               logger_type_expected = "u20",
                               fun_name = "convert_waterlevel_kPa_m") {
  
  ## 1) Required columns ---------------------------------------------
  required_cols <- c(
    "site_station_code",
    "timestamp",
    "waterpress_kPa",
    "watertemp_C",
    "baro_data"
  )
  
  missing_cols <- setdiff(required_cols, names(input_data))
  if (length(missing_cols) > 0) {
    stop(
      fun_name, "(): input_data is missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  ## 2) Logger type QC (if present) ---------------------------------
  if ("logger_type" %in% names(input_data)) {
    lt_raw   <- unique(input_data$logger_type)
    lt_clean <- tolower(trimws(as.character(lt_raw)))
    
    if (!any(grepl(logger_type_expected, lt_clean, fixed = TRUE))) {
      warning(
        fun_name, "(): logger type(s) in input_data (",
        paste(lt_raw, collapse = ", "),
        ") do not match expected pattern '", logger_type_expected, "'. ",
        "Proceeding anyway, but check that this really is the expected pressure logger."
      )
    }
  } else {
    warning(
      fun_name,
      "(): column 'logger_type' not found in input_data; skipping logger-type QC."
    )
  }
  
  ## 3) Timestamp QC: ensure POSIXct --------------------------------
  if (!inherits(input_data$timestamp, "POSIXct")) {
    ts_parsed <- suppressWarnings(
      lubridate::ymd_hms(input_data$timestamp, tz = "UTC")
    )
    if (any(is.na(ts_parsed))) {
      stop(
        fun_name,
        "(): failed to parse input_data$timestamp as POSIXct with ymd_hms(). ",
        "Check timestamp format."
      )
    }
    input_data$timestamp <- ts_parsed
  }
  
  ## 4) Light unit / range sanity checks -----------------------------
  
  # Water temperature: expected approx -5 to 40 °C
  if ("watertemp_C" %in% names(input_data)) {
    wt <- input_data$watertemp_C
    bad_temp <- which(!is.na(wt) & (wt < -5 | wt > 40))   # CORRECTED (& not &&)
    if (length(bad_temp)) {
      warning(
        fun_name, "(): watertemp_C has ", length(bad_temp),
        " value(s) outside [-5, 40] °C. Check units (should be °C)."
      )
    }
  }
  
  # Barometric pressure: typically 80–120 kPa
  if ("baro_data" %in% names(input_data)) {
    bp <- input_data$baro_data
    bad_baro <- which(!is.na(bp) & (bp < 80 | bp > 120))   # CORRECT
    if (length(bad_baro)) {
      warning(
        fun_name, "(): baro_data has ", length(bad_baro),
        " value(s) outside [80, 120] kPa. Check units."
      )
    }
  }
  
  # Water pressure: expected >0 and <= 600 kPa
  if ("waterpress_kPa" %in% names(input_data)) {
    wp <- input_data$waterpress_kPa
    bad_wp <- which(!is.na(wp) & (wp <= 0 | wp > 600))     # CORRECT
    if (length(bad_wp)) {
      warning(
        fun_name, "(): waterpress_kPa has ", length(bad_wp),
        " value(s) outside (0, 600] kPa. Check units."
      )
    }
  }
  
  
  input_data
}




#' QA/QC for manual stage/depth reference data
#'
#' Reads and validates a CSV of manual water-level measurements
#' (stage or depth) used as reference levels for barometric
#' compensation.
#'
#' Expected core columns (in addition to others you may have):
#'   - site_station_code
#'   - timestamp          (date-time of reference reading)
#'   - site_comments
#'   - stage_m and/or depth_m
#'
#' @param ref_path Path to the reference CSV file.
#'
#' @return A cleaned data.frame with parsed timestamp and numeric
#'         stage/depth columns.
#' @export
QAQC_reference_data <- function(ref_path) {
  tryCatch({
    ## 1) File exists? ----
    if (!file.exists(ref_path)) {
      stop("Reference file not found: ", ref_path)
    }
    
    ## 2) Read CSV ----
    ref <- read.csv(
      ref_path,
      header           = TRUE,
      stringsAsFactors = FALSE,
      check.names      = FALSE
    )
    
    ## 3) Drop completely empty rows ----
    ref <- ref[rowSums(is.na(ref) | ref == "") != ncol(ref), , drop = FALSE]
    if (!nrow(ref)) {
      stop("Reference file '", ref_path, "' has no non-empty rows after cleaning.")
    }
    
    ## 4) Required columns ----
    required_cols <- c("site_station_code", "timestamp", "site_comments")
    missing_cols  <- setdiff(required_cols, names(ref))
    if (length(missing_cols)) {
      stop(
        "Reference data is missing required column(s): ",
        paste(missing_cols, collapse = ", ")
      )
    }
    
    # Need at least stage_m or depth_m
    if (!("stage_m" %in% names(ref)) && !("depth_m" %in% names(ref))) {
      stop("Reference data must contain at least one of 'stage_m' or 'depth_m'.")
    }
    
    ## 5) Parse timestamp column ----
    parse_dt <- function(x) {
      x_chr <- as.character(x)
      
      # Try mdy_hm first (matches your sheet e.g. 7/23/2019 14:30)
      p1 <- try(lubridate::mdy_hm(x_chr, tz = "UTC"), silent = TRUE)
      if (!inherits(p1, "try-error") && !all(is.na(p1))) {
        return(p1)
      }
      
      # Fallback to more flexible parser
      p2 <- suppressWarnings(
        lubridate::parse_date_time(
          x_chr,
          orders = c("mdy HM", "mdy HMS", "mdY HM", "mdY HMS",
                     "ymd HM", "ymd HMS"),
          tz = "UTC"
        )
      )
      if (all(is.na(p2))) {
        stop(
          "Failed to parse 'timestamp'. ",
          "Expected formats like '7/23/2019 14:30' (mdy_hm)."
        )
      }
      p2
    }
    
    ref$timestamp <- parse_dt(ref$timestamp)
    
    ## 6) stage_m / depth_m numeric & light checks ----
    if ("stage_m" %in% names(ref)) {
      ref$stage_m <- suppressWarnings(as.numeric(ref$stage_m))
      if (any(ref$stage_m < 0, na.rm = TRUE)) {
        warning("Some 'stage_m' values are negative – check units and data entry.")
      }
    }
    
    if ("depth_m" %in% names(ref)) {
      ref$depth_m <- suppressWarnings(as.numeric(ref$depth_m))
      if (any(ref$depth_m < 0, na.rm = TRUE)) {
        warning("Some 'depth_m' values are negative – check units and data entry.")
      }
    }
    
    ## 7) High-level check for any usable reference values -----------
    has_stage_vals <- "stage_m" %in% names(ref) && any(!is.na(ref$stage_m))
    has_depth_vals <- "depth_m" %in% names(ref) && any(!is.na(ref$depth_m))
    
    # Optional: treat some discharge columns as acceptable "reference"
    discharge_cols <- intersect(
      names(ref),
      c("Q_cms", "Q_m3s", "discharge_cms", "discharge_m3s")
    )
    has_discharge_vals <- FALSE
    if (length(discharge_cols) > 0) {
      has_discharge_vals <- any(
        vapply(
          discharge_cols,
          function(col) any(!is.na(suppressWarnings(as.numeric(ref[[col]]))), na.rm = TRUE),
          logical(1)
        )
      )
    }
    
    if (!has_stage_vals && !has_depth_vals && !has_discharge_vals) {
      warning(
        "Reference data has no usable stage, depth, or discharge values; ",
        "no reference levels can be derived from this file."
      )
    }
    
    ## 8) Duplicate (site, timestamp) checks ----
    dup_idx <- duplicated(ref[, c("site_station_code", "timestamp")])
    if (any(dup_idx)) {
      warning(
        "Duplicate (site_station_code, timestamp) combinations detected (",
        sum(dup_idx), " duplicates). You may want to consolidate or remove them."
      )
    }
    
    ## 9) Light hygiene ----
    ref$site_station_code <- trimws(as.character(ref$site_station_code))
    ref$site_comments     <- trimws(as.character(ref$site_comments))
    
    ref
    
  }, error = function(e) {
    stop("Error reading/parsing reference data: ", e$message, call. = FALSE)
  })
}

