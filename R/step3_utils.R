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
        ") do not match expected pattern '", logger_type_expected, "'.\n",
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
  # Water temperature: we expect something like -5 to 40 °C
  if ("watertemp_C" %in% names(input_data)) {
    wt <- input_data$watertemp_C
    bad_temp <- which(!is.na(wt) & (wt < -5 | wt > 40))
    if (length(bad_temp)) {
      warning(
        fun_name, "(): watertemp_C has ", length(bad_temp),
        " value(s) outside [-5, 40] °C. Check units (should be °C)."
      )
    }
  }
  
  # Barometric pressure: typically ~80–120 kPa at Earth’s surface
  if ("baro_data" %in% names(input_data)) {
    bp <- input_data$baro_data
    bad_baro <- which(!is.na(bp) & (bp < 80 | bp > 120))
    if (length(bad_baro)) {
      warning(
        fun_name, "(): baro_data has ", length(bad_baro),
        " value(s) outside [80, 120] kPa. Check units (should be kPa)."
      )
    }
  }
  
  # Water pressure: we just enforce >0 and < 600 kPa as a sanity range
  if ("waterpress_kPa" %in% names(input_data)) {
    wp <- input_data$waterpress_kPa
    bad_wp <- which(!is.na(wp) & (wp <= 0 | wp > 600))
    if (length(bad_wp)) {
      warning(
        fun_name, "(): waterpress_kPa has ", length(bad_wp),
        " value(s) outside (0, 600] kPa. Check units (should be kPa absolute)."
      )
    }
  }
  
  return(input_data)
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
    # Now require: site_station_code, timestamp, site_comments
    required_cols <- c("site_station_code", "timestamp", "site_comments")
    missing_cols  <- setdiff(required_cols, names(ref))
    if (length(missing_cols)) {
      stop(
        "Reference data is missing required column(s): ",
        paste(missing_cols, collapse = ", ")
      )
    }
    
    # We still expect at least one of stage_m or depth_m
    if (!("stage_m" %in% names(ref)) && !("depth_m" %in% names(ref))) {
      stop(
        "Reference data must contain at least one of 'stage_m' or 'depth_m'."
      )
    }
    
    ## 5) Parse timestamp column ----
    parse_dt <- function(x) {
      x_chr <- as.character(x)
      p1 <- try(lubridate::mdy_hm(x_chr, tz = "UTC"), silent = TRUE)
      if (!inherits(p1, "try-error") && !all(is.na(p1))) {
        return(p1)
      }
      
      # fallback formats
      p2 <- suppressWarnings(
        lubridate::parse_date_time(
          x_chr,
          orders = c(
            "mdy HM", "mdy HMS", "mdY HM", "mdY HMS",
            "ymd HM", "ymd HMS"
          ),
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
    
    # Warn if any timestamps are NA after parsing
    na_ts <- which(is.na(ref$timestamp))
    if (length(na_ts)) {
      warning(
        "Missing/invalid 'timestamp' on row(s): ",
        paste(na_ts, collapse = ", ")
      )
    }
    
    ## 6) Ensure stage_m / depth_m numeric ----
    if ("stage_m" %in% names(ref)) {
      ref$stage_m <- suppressWarnings(as.numeric(ref$stage_m))
      bad_stage <- which(is.na(ref$stage_m))
      if (length(bad_stage)) {
        warning(
          "Non-numeric or missing 'stage_m' on row(s): ",
          paste(bad_stage, collapse = ", "),
          ". These will be treated as NA."
        )
      }
      if (any(ref$stage_m < 0, na.rm = TRUE)) {
        warning("Some 'stage_m' values are negative – check units and data entry.")
      }
    }
    
    if ("depth_m" %in% names(ref)) {
      ref$depth_m <- suppressWarnings(as.numeric(ref$depth_m))
      bad_depth <- which(is.na(ref$depth_m))
      if (length(bad_depth)) {
        warning(
          "Non-numeric or missing 'depth_m' on row(s): ",
          paste(bad_depth, collapse = ", "),
          ". These will be treated as NA."
        )
      }
      if (any(ref$depth_m < 0, na.rm = TRUE)) {
        warning("Some 'depth_m' values are negative – check units and data entry.")
      }
    }
    
    ## 7) Check rows with no usable reference value ----
    if ("stage_m" %in% names(ref) && "depth_m" %in% names(ref)) {
      no_level_rows <- which(is.na(ref$stage_m) & is.na(ref$depth_m))
    } else if ("stage_m" %in% names(ref)) {
      no_level_rows <- which(is.na(ref$stage_m))
    } else {
      no_level_rows <- which(is.na(ref$depth_m))
    }
    
    if (length(no_level_rows)) {
      warning(
        "Rows with no usable reference value (stage_m/depth_m) on row(s): ",
        paste(no_level_rows, collapse = ", "),
        ". These rows will still be returned but cannot be used as reference levels."
      )
    }
    
    ## 8) Duplicate (site, timestamp) checks ----
    dup_idx <- which(duplicated(ref[, c("site_station_code", "timestamp")]))
    if (length(dup_idx)) {
      warning(
        "Duplicate (site_station_code, timestamp) combinations at row(s): ",
        paste(dup_idx, collapse = ", "),
        ". You may want to consolidate or remove duplicates."
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
