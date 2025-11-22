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

QAQC_wl_kpa_inputs <- function(input_data, logger_type_expected = "u20") {
  
  ## --- Required columns etc. (your existing checks) ------------------
  required_cols <- c("site_station_code",
                     "timestamp",
                     "waterpress_kPa",
                     "watertemp_C",
                     "baro_data")
  
  missing_cols <- setdiff(required_cols, names(input_data))
  if (length(missing_cols) > 0) {
    stop(
      "convert_waterlevel_kPa_m(): input_data is missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  # Logger type QC (unchanged from before)
  if ("logger_type" %in% names(input_data)) {
    lt_raw   <- unique(input_data$logger_type)
    lt_clean <- tolower(trimws(as.character(lt_raw)))
    
    if (!any(grepl(logger_type_expected, lt_clean, fixed = TRUE))) {
      warning(
        "convert_waterlevel_kPa_m(): logger_type values (",
        paste(unique(lt_raw), collapse = ", "),
        ") do not match expected pattern '", logger_type_expected,
        "'. Proceeding anyway."
      )
    }
  } else {
    warning("convert_waterlevel_kPa_m(): column 'logger_type' not found; skipping logger-type QC.")
  }
  
  # Timestamp QC
  if (!inherits(input_data$timestamp, "POSIXct")) {
    ts_parsed <- suppressWarnings(lubridate::ymd_hms(input_data$timestamp, tz = "UTC"))
    if (any(is.na(ts_parsed))) {
      stop("convert_waterlevel_kPa_m(): failed to parse timestamp as POSIXct with ymd_hms().")
    }
    input_data$timestamp <- ts_parsed
  }
  
  ## --- Helper to warn by station + year ------------------------------
  warn_by_station_year <- function(df, value_col, lower, upper, label) {
    
    if (!value_col %in% names(df)) return(invisible())
    
    v   <- df[[value_col]]
    bad <- !is.na(v) & (v < lower | v > upper)
    
    if (!any(bad)) return(invisible())
    
    bad_df <- df[bad, c("site_station_code", "timestamp", value_col)]
    
    bad_summary <- bad_df %>%
      dplyr::mutate(year = lubridate::year(timestamp)) %>%
      dplyr::count(site_station_code, year, name = "n_bad") %>%
      dplyr::arrange(site_station_code, year)
    
    msg_lines <- paste0(
      "  - ", bad_summary$site_station_code,
      ", ", bad_summary$year, ": ",
      bad_summary$n_bad, " value(s)"
    )
    
    warning(
      label, "\n",
      paste(msg_lines, collapse = "\n")
    )
  }
  
  ## --- Range QA/QC using the helper ---------------------------------
  # Temperature: expected [-5, 40] °C
  warn_by_station_year(
    df        = input_data,
    value_col = "watertemp_C",
    lower     = -5,
    upper     = 40,
    label     = "convert_waterlevel_kPa_m(): watertemp_C outside [-5, 40] °C by station/year:"
  )
  
  # Water pressure: very rough sanity range (adjust if you like)
  warn_by_station_year(
    df        = input_data,
    value_col = "waterpress_kPa",
    lower     = 80,    # example min
    upper     = 300,   # example max
    label     = "convert_waterlevel_kPa_m(): waterpress_kPa outside [80, 300] kPa by station/year:"
  )
  
  # Baro pressure: rough sanity range
  warn_by_station_year(
    df        = input_data,
    value_col = "baro_data",
    lower     = 80,
    upper     = 120,
    label     = "convert_waterlevel_kPa_m(): baro_data outside [80, 120] kPa by station/year:"
  )
  
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


convert_waterlevel_single <- function(input_data,
                                      select_station,
                                      reference_data,
                                      ref_type_clean,    # "stage" or "depth"
                                      ref_level_col,     # "stage_m" or "depth_m"
                                      select_measurement = 1,
                                      path_to_output_folder) {
  
  ## A) Filter input_data to this station -----------------------
  output_data <- input_data[input_data$site_station_code == select_station, , drop = FALSE]
  if (!nrow(output_data)) {
    stop("No rows found in input_data for site_station_code = '", select_station, "'.")
  }
  
  ts_range <- range(output_data$timestamp, na.rm = TRUE)
  
  ## B) Extract reference rows for this station -----------------
  ref_dat <- reference_data %>%
    dplyr::filter(site_station_code == select_station) %>%
    dplyr::mutate(ref_m = .data[[ref_level_col]]) %>%
    dplyr::select(timestamp, ref_m) %>%
    dplyr::filter(!is.na(ref_m)) %>%
    dplyr::arrange(timestamp)
  
  if (!nrow(ref_dat)) {
    stop(
      "No non-NA reference measurements (", ref_level_col, ") for station ",
      select_station, "."
    )
  }
  
  # Round to nearest hour (to match logger)
  ref_dat$timestamp <- lubridate::round_date(ref_dat$timestamp, "hour")
  
  # Filter to logger time range
  ref_dat <- ref_dat %>%
    dplyr::filter(timestamp >= ts_range[1], timestamp <= ts_range[2])
  
  if (!nrow(ref_dat)) {
    stop(
      "No reference measurements for ", select_station,
      " fall within the input_data timestamp range."
    )
  }
  
  # Check select_measurement index
  if (select_measurement < 1 || select_measurement > nrow(ref_dat)) {
    stop(
      "select_measurement = ", select_measurement,
      " but there are only ", nrow(ref_dat), " reference rows in-range for station ",
      select_station, "."
    )
  }
  
  # Join reference values onto time series (for plotting later)
  output_data <- dplyr::left_join(output_data, ref_dat, by = "timestamp")
  
  ## C) Metric conversions & constants --------------------------
  FEET_TO_METERS <- 0.3048
  KPA_TO_PSI     <- 0.1450377
  PSI_TO_PSF     <- 144.0
  
  ## D) Fluid density of water ----------------------------------
  output_data <- output_data %>%
    dplyr::mutate(
      density_kgm3 =
        (999.83952 +
           16.945176 * watertemp_C -
           7.9870401e-03 * watertemp_C^2 -
           46.170461e-06 * watertemp_C^3 +
           105.56302e-09 * watertemp_C^4 -
           280.54253e-12 * watertemp_C^5) /
        (1 + 16.879850e-03 * watertemp_C),
      density_lbft3 = 0.0624279606 * density_kgm3
    )
  
  ## E) Convert pressures to depth (m) --------------------------
  output_data <- output_data %>%
    dplyr::mutate(
      sensor_depth_m      = FEET_TO_METERS * (KPA_TO_PSI * PSI_TO_PSF * waterpress_kPa) / density_lbft3,
      baro_sensor_depth_m = FEET_TO_METERS * (KPA_TO_PSI * PSI_TO_PSF * baro_data)      / density_lbft3
    )
  
  ## F) Reference point & compensation constant k ---------------
  T_meas <- ref_dat$timestamp[select_measurement]
  L_meas <- ref_dat$ref_m[select_measurement]
  
  D_ref   <- output_data$sensor_depth_m[output_data$timestamp == T_meas]
  D_baro0 <- output_data$baro_sensor_depth_m[output_data$timestamp == T_meas]
  
  if (!length(D_ref) || !length(D_baro0)) {
    stop(
      "Could not find matching timestamp in output_data for selected reference measurement #",
      select_measurement,
      " at station ", select_station,
      ". Check that reference timestamps align (after rounding) with logger timestamps."
    )
  }
  
  # If duplicates, just use the first
  D_ref   <- D_ref[1]
  D_baro0 <- D_baro0[1]
  
  k <- L_meas - (D_ref - D_baro0)
  
  ## G) Apply compensation to entire series ---------------------
  output_data <- output_data %>%
    dplyr::mutate(
      waterlevel_m         = sensor_depth_m - baro_sensor_depth_m + k,
      waterlevel_reference = ref_type_clean
    )
  
  ## H) QAQC plot (single station) ------------------------------
  p <- ggplot2::ggplot(output_data,
                       ggplot2::aes(x = timestamp, y = waterlevel_m)) +
    ggplot2::geom_line(color = "#233d4d") +
    ggplot2::geom_point(
      ggplot2::aes(y = ref_m),
      color = "#fe7f2d",
      size  = 3
    ) +
    ggplot2::labs(
      title = paste("kPa to m conversion for:", select_station),
      x     = "Timestamp",
      y     = "Water level (m)"
    ) +
    ggplot2::theme_classic()
  
  ## I) Write yearly processed files ----------------------------
  years_i <- unique(lubridate::year(output_data$timestamp))
  
  for (year_n in years_i) {
    site_year_n <- dplyr::filter(output_data, lubridate::year(timestamp) == year_n)
    if (!nrow(site_year_n)) next
    
    start_n <- gsub("\\D", "", as.character(min(lubridate::date(site_year_n$timestamp))))
    end_n   <- gsub("\\D", "", as.character(max(lubridate::date(site_year_n$timestamp))))
    
    year_dir <- file.path(path_to_output_folder, year_n)
    proc_dir <- file.path(year_dir, "processed")
    
    dir.create(year_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(proc_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Export timestamp as character to be Excel-safe
    site_year_n$timestamp <- format(site_year_n$timestamp, "%Y-%m-%d %H:%M:%S")
    
    out_file <- sprintf("%s_WL_%s_%s_v0.3.csv", select_station, start_n, end_n)
    out_path <- file.path(proc_dir, out_file)
    
    utils::write.csv(site_year_n, out_path, row.names = FALSE)
    message(sprintf("Barometric compensation (v0.3) for %s written to %s", select_station, out_path))
  }
  
  ## J) Return list ---------------------------------------------
  return(list(
    site_wl = output_data,
    ref_dat = ref_dat,
    wl_plot = plotly::ggplotly(p)
  ))
}

diagnose_reference_failure <- function(station, input_data, reference_df, reference_type) {
  
  ref_col <- if (reference_type == "depth") "depth_m" else "stage_m"
  
  logger_range <- range(input_data$timestamp[input_data$site_station_code == station], na.rm = TRUE)
  
  # 1) Any rows for this station?
  ref_rows <- reference_df[reference_df$site_station_code == station, ]
  if (nrow(ref_rows) == 0) {
    return(
      paste0(
        "Reference file contains *no rows* for station ", station, ".\n",
        "- Ensure station ID is spelled correctly\n",
        "- Confirm it exists in the reference CSV\n"
      )
    )
  }
  
  # 2) All reference values NA?
  if (all(is.na(ref_rows[[ref_col]]))) {
    return(
      paste0(
        "All reference values (", ref_col, ") for station ", station, " are NA.\n",
        "- Check field sheet for missing / invalid measurements\n",
        "- Ensure correct column chosen for reference_type=", reference_type, "\n"
      )
    )
  }
  
  # 3) After rounding timestamps – do any fall inside logger time range?
  rounded_rows <- ref_rows
  rounded_rows$timestamp <- lubridate::round_date(rounded_rows$timestamp, "hour")
  
  ref_in_range <- rounded_rows[
    rounded_rows$timestamp >= logger_range[1] &
      rounded_rows$timestamp <= logger_range[2], ]
  
  if (nrow(ref_in_range) == 0) {
    
    return(
      paste0(
        "Reference timestamps do *not* fall within logger time range.\n",
        "- Station: ", station, "\n",
        "- Logger covers: ", format(logger_range[1]), " to ", format(logger_range[2]), "\n",
        "- Earliest reference: ", format(min(rounded_rows$timestamp)), "\n",
        "- Latest reference: ", format(max(rounded_rows$timestamp)), "\n",
        "- Possibly wrong timezone or logger was started/stopped at different time.\n"
      )
    )
  }
  
  # 4) Default message when unknown error occurs
  paste0(
    "Unknown reference-data error for station ", station, ".\n",
    "Check reference formatting and timestamps."
  )
}

