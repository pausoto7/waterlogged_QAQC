# ---- Helpers ---------------------------------------------------------------

#' Normalize and validate metric parameter
#'
#' Internal helper to standardize metric names and validate input. Converts
#' various spellings and aliases to canonical metric names used throughout
#' the package.
#'
#' @param metric Character; the metric name to normalize. Can be one of:
#'   \code{"waterlevel"}, \code{"stage"}, \code{"DO"}, \code{"dissolvedoxygen"},
#'   \code{"oxygen"}, \code{"baro"}, \code{"barometric"}, \code{"airpressure"},
#'   \code{"watertemp"}, \code{"watertemperature"}, \code{"airtemp"},
#'   \code{"airtemperature"}, or \code{"all"} (if \code{allow_all = TRUE}).
#' @param allow_all Logical; if \code{TRUE}, allows \code{metric = "all"}.
#'   Default is \code{FALSE}.
#'
#' @return A character string with the normalized metric name: one of
#'   \code{"waterlevel"}, \code{"dissolvedoxygen"}, \code{"barometric"},
#'   \code{"watertemp"}, \code{"airtemp"}, or \code{"all"}.
#'
#' @keywords internal
#' @noRd
normalise_metric <- function(metric, allow_all = FALSE) {
  if (is.null(metric) || length(metric) != 1L) {
    stop("`metric` must be a single, non-NULL value.", call. = FALSE)
  }
  
  m <- tolower(trimws(metric))
  m <- gsub("\\s+", "", m)
  
  if (m == "all") {
    if (allow_all) return("all")
    stop("`metric = 'all'` is only allowed when explicitly permitted.", call. = FALSE)
  }
  
  if (m %in% c("waterlevel", "stage"))               return("waterlevel")
  if (m %in% c("do", "dissolvedoxygen", "oxygen"))   return("dissolvedoxygen")
  if (m %in% c("baro", "barometric", "airpressure")) return("barometric")
  if (m %in% c("watertemp", "watertemperature"))     return("watertemp")
  if (m %in% c("airtemp", "airtemperature"))         return("airtemp")
  
  stop(
    "Unknown `metric`. Expected one of: ",
    "'waterlevel', 'dissolvedoxygen', 'barometric', 'watertemp', 'airtemp'",
    " (case and spaces ignored). You gave: '", metric, "'.",
    call. = FALSE
  )
}


#' Resolve data processing version to file pattern
#'
#' Internal helper to convert user-supplied data processing version strings
#' into standardized versions and corresponding file name patterns.
#'
#' @param data_processing Character; the data processing version. Accepts
#'   various formats including: \code{"raw"}, \code{"v0.1"}, \code{"v0.2"},
#'   \code{"v0.3"}, \code{"v1.0"}, \code{"clean"}, \code{"final"}, etc.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{data_processing}{Normalized version string (e.g., "v0.1", "v1.0")}
#'     \item{pattern}{Regex pattern to match files of that version (e.g., "v0.1\\.csv$")}
#'   }
#'
#' @keywords internal
#' @noRd
resolve_data_processing_pattern <- function(data_processing) {
  
  if (is.null(data_processing) || length(data_processing) != 1L) {
    stop("`data_processing` must be a single value.", call. = FALSE)
  }
  
  dp <- tolower(trimws(data_processing))
  dp <- gsub("\\s+", "", dp)
  
  # explicit mappings
  if (dp %in% c("raw")) {
    dp <- "raw"
  } else if (dp %in% c("v0.1", "v01", "0.1")) {
    dp <- "v0.1"
  } else if (dp %in% c("v0.2", "v02", "0.2")) {
    dp <- "v0.2"
  } else if (dp %in% c("v0.3", "v03", "0.3")) {
    dp <- "v0.3"
  } else if (dp %in% c("v1.0", "v10", "1.0", "1", "final", "processed", "clean")) {
    dp <- "v1.0"
  } else {
    # partial / fuzzy mappings as backup
    if (grepl("^raw", dp))           dp <- "raw"
    else if (grepl("0\\.1$", dp))    dp <- "v0.1"
    else if (grepl("0\\.2$", dp))    dp <- "v0.2"
    else if (grepl("0\\.3$", dp))    dp <- "v0.3"
    else if (grepl("^v1", dp))       dp <- "v1.0"
    else {
      stop(
        "`data_processing` must be one of:\n",
        "  - 'raw'\n",
        "  - 'v0.1'\n",
        "  - 'v0.2'\n",
        "  - 'v0.3'\n",
        "  - 'v1.0' / 'clean' (final version)\n\n",
        "You gave: '", data_processing, "'.",
        call. = FALSE
      )
    }
  }
  
  pattern <- switch(
    dp,
    "raw"  = "\\.csv$",      # any csv
    "v0.1" = "v0.1\\.csv$",
    "v0.2" = "v0.2\\.csv$",
    "v0.3" = "v0.3\\.csv$",
    "v1.0" = "v1.0\\.csv$",
    stop("Unexpected `data_processing` value after QC: ", dp, call. = FALSE)
  )
  
  list(data_processing = dp, pattern = pattern)
}



#' Summarize logger data by temporal scale
#'
#' Internal helper to aggregate time series data by hour, day, week, month,
#' or year. Produces summary statistics (mean, min, max) for metric-specific
#' value columns.
#'
#' @param dat Data frame containing cleaned logger data with a \code{timestamp}
#'   column, \code{site_station_code}, and metric-specific value columns.
#' @param metric_norm Character; normalized metric name (one of
#'   \code{"dissolvedoxygen"}, \code{"waterlevel"}, \code{"barometric"},
#'   \code{"airtemp"}, \code{"watertemp"}).
#' @param temporal_scale Character; aggregation interval. One of
#'   \code{"hourly"}, \code{"daily"}, \code{"weekly"}, \code{"monthly"},
#'   or \code{"yearly"}.
#'
#' @return A data frame with one row per station per time group, containing
#'   mean, min, and max values for the relevant metric columns.
#'
#' @keywords internal
#' @noRd
summarise_clean_data <- function(dat, metric_norm, temporal_scale) {
  # temporal_scale already validated upstream
  
  # --- create time_group ---------------------------------------------
  dat <- dat %>%
    dplyr::mutate(
      time_group = dplyr::case_when(
        temporal_scale == "hourly"  ~ lubridate::floor_date(timestamp, "hour"),
        temporal_scale == "daily"   ~ as.Date(timestamp),
        temporal_scale == "weekly"  ~ lubridate::floor_date(timestamp, "week", week_start = 1),
        temporal_scale == "monthly" ~ lubridate::floor_date(timestamp, "month"),
        temporal_scale == "yearly"  ~ lubridate::floor_date(timestamp, "year"),
        TRUE ~ NA
      )
    )
  
  if (all(is.na(dat$time_group))) {
    stop("Failed to create `time_group` for temporal_scale = ", temporal_scale, call. = FALSE)
  }
  
  group_vars <- c("site_station_code", "time_group")
  
  # --- metric-specific summaries -------------------------------------
  if (metric_norm == "dissolvedoxygen") {
    
    required <- c("do_mgl_adj", "do_percsat_adj", "watertemp_C_adj")
    missing  <- setdiff(required, names(dat))
    if (length(missing) > 0L) {
      stop(
        "For metric 'dissolvedoxygen', expected columns: ",
        paste(required, collapse = ", "),
        ". Missing: ", paste(missing, collapse = ", "),
        ". Did you select the correct folder (clean v1.0 DO data)?",
        call. = FALSE
      )
    }
    
    out <- dat %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
      dplyr::summarise(
        mean_do_mgl      = mean(do_mgl_adj,      na.rm = TRUE),
        mean_do_percsat  = mean(do_percsat_adj,  na.rm = TRUE),
        mean_watertemp_C = mean(watertemp_C_adj, na.rm = TRUE),
        min_do_mgl       = min(do_mgl_adj,       na.rm = TRUE),
        min_do_percsat   = min(do_percsat_adj,   na.rm = TRUE),
        min_watertemp_C  = min(watertemp_C_adj,  na.rm = TRUE),
        max_do_mgl       = max(do_mgl_adj,       na.rm = TRUE),
        max_do_percsat   = max(do_percsat_adj,   na.rm = TRUE),
        max_watertemp_C  = max(watertemp_C_adj,  na.rm = TRUE),
        .groups          = "drop"
      )
    
  } else if (metric_norm == "waterlevel") {
    
    required <- c("waterlevel_m_adj", "watertemp_C_adj")
    missing  <- setdiff(required, names(dat))
    if (length(missing) > 0L) {
      stop(
        "For metric 'waterlevel', expected columns: ",
        paste(required, collapse = ", "),
        ". Missing: ", paste(missing, collapse = ", "),
        ". Did you select the correct folder (clean v1.0 WL data)?",
        call. = FALSE
      )
    }
    
    out <- dat %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
      dplyr::summarise(
        mean_waterlevel_m = mean(waterlevel_m_adj, na.rm = TRUE),
        mean_watertemp_C  = mean(watertemp_C_adj,  na.rm = TRUE),
        min_waterlevel_m  = min(waterlevel_m_adj,  na.rm = TRUE),
        min_watertemp_C   = min(watertemp_C_adj,   na.rm = TRUE),
        max_waterlevel_m  = max(waterlevel_m_adj,  na.rm = TRUE),
        max_watertemp_C   = max(watertemp_C_adj,   na.rm = TRUE),
        .groups           = "drop"
      )
    
  } else if (metric_norm == "barometric") {
    
    required <- c("airpress_kPa_adj", "airtemp_C_adj")
    missing  <- setdiff(required, names(dat))
    if (length(missing) > 0L) {
      stop(
        "For metric 'barometric', expected columns: ",
        paste(required, collapse = ", "),
        ". Missing: ", paste(missing, collapse = ", "),
        ". Did you select the correct folder (clean v1.0 BARO data)?",
        call. = FALSE
      )
    }
    
    out <- dat %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
      dplyr::summarise(
        mean_airpress_kPa = mean(airpress_kPa_adj, na.rm = TRUE),
        mean_airtemp_C    = mean(airtemp_C_adj,    na.rm = TRUE),
        min_airpress_kPa  = min(airpress_kPa_adj,  na.rm = TRUE),
        min_airtemp_C     = min(airtemp_C_adj,     na.rm = TRUE),
        max_airpress_kPa  = max(airpress_kPa_adj,  na.rm = TRUE),
        max_airtemp_C     = max(airtemp_C_adj,     na.rm = TRUE),
        .groups           = "drop"
      )
    
  } else if (metric_norm == "airtemp") {
    
    required <- c("airtemp_C_adj")
    missing  <- setdiff(required, names(dat))
    if (length(missing) > 0L) {
      stop(
        "For metric 'airtemp', expected column: airtemp_C_adj. Missing: ",
        paste(missing, collapse = ", "),
        ". Did you select the correct folder (clean v1.0 AT data)?",
        call. = FALSE
      )
    }
    
    out <- dat %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
      dplyr::summarise(
        mean_airtemp_C = mean(airtemp_C_adj, na.rm = TRUE),
        min_airtemp_C  = min(airtemp_C_adj,  na.rm = TRUE),
        max_airtemp_C  = max(airtemp_C_adj,  na.rm = TRUE),
        .groups        = "drop"
      )
    
  } else if (metric_norm == "watertemp") {
    
    required <- c("watertemp_C_adj")
    missing  <- setdiff(required, names(dat))
    if (length(missing) > 0L) {
      stop(
        "For metric 'watertemp', expected column: watertemp_C_adj. Missing: ",
        paste(missing, collapse = ", "),
        ". Did you select the correct folder (clean v1.0 WT data)?",
        call. = FALSE
      )
    }
    
    out <- dat %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
      dplyr::summarise(
        mean_watertemp_C = mean(watertemp_C_adj, na.rm = TRUE),
        min_watertemp_C  = min(watertemp_C_adj,  na.rm = TRUE),
        max_watertemp_C  = max(watertemp_C_adj,  na.rm = TRUE),
        .groups          = "drop"
      )
    
  } else {
    stop("Metric '", metric_norm, "' not implemented in summarise_clean_data().", call. = FALSE)
  }
  
  # round numeric, clean Inf/NaN ------------------------------------------------
  out <- out %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2)))
  
  out[] <- lapply(out, function(x) {
    if (is.numeric(x)) {
      x[is.infinite(x) | is.nan(x)] <- NA_real_
    }
    x
  })
  
  out
}


#' Normalize and validate temporal_scale parameter
#'
#' Internal helper to standardize temporal aggregation scale strings and
#' validate input.
#'
#' @param temporal_scale Character; the temporal scale to normalize. Can be
#'   one of: \code{"none"}, \code{"hourly"}, \code{"daily"}, \code{"weekly"},
#'   \code{"monthly"}, or \code{"yearly"} (case and spaces ignored).
#'
#' @return A character string with the normalized temporal scale: one of
#'   \code{"none"}, \code{"hourly"}, \code{"daily"}, \code{"weekly"},
#'   \code{"monthly"}, or \code{"yearly"}.
#'
#' @keywords internal
#' @noRd
normalise_temporal_scale <- function(temporal_scale) {
  if (is.null(temporal_scale) || length(temporal_scale) != 1L) {
    stop("`temporal_scale` must be a single, non-NULL value.", call. = FALSE)
  }

  ts <- tolower(trimws(temporal_scale))
  ts <- gsub("\\s+", "", ts)

  # Map various spellings to canonical forms
  if (ts %in% c("none", "raw", "original")) return("none")
  if (ts %in% c("hour", "hourly", "hr"))    return("hourly")
  if (ts %in% c("day", "daily"))            return("daily")
  if (ts %in% c("week", "weekly"))          return("weekly")
  if (ts %in% c("month", "monthly"))        return("monthly")
  if (ts %in% c("year", "yearly", "annual")) return("yearly")

  stop(
    "Unknown `temporal_scale`. Expected one of: ",
    "'none', 'hourly', 'daily', 'weekly', 'monthly', 'yearly' ",
    "(case and spaces ignored). You gave: '", temporal_scale, "'.",
    call. = FALSE
  )
}


#' Column names to convert to factors
#'
#' Internal character vector listing column names that should be converted
#' to factors when reading logger data. Used by \code{get_logger_data()}.
#'
#' @keywords internal
#' @noRd
factor_cols <- c(
  "sn", "baro_code", "qaqc_code", "wl_qaqc_code",
  "do_qaqc_code", "wt_qaqc_code", "at_qaqc_code", "baro_qaqc_code"
)


#' Read a single logger CSV file
#'
#' Internal helper to read logger CSV files and convert specific columns
#' to factors.
#'
#' @param file_path Character; path to a single CSV file.
#'
#' @return A data frame with the file contents, with specific columns
#'   converted to factors.
#'
#' @keywords internal
#' @noRd
read_logger_file <- function(file_path) {
  dat <- utils::read.csv(file_path, stringsAsFactors = FALSE)

  # Convert timestamp to POSIXct if it exists
  if ("timestamp" %in% names(dat)) {
    if (!inherits(dat$timestamp, "POSIXct")) {
      dat$timestamp <- as.POSIXct(dat$timestamp, tz = "UTC")
    }
  }

  # Convert specific columns to factors
  for (col in factor_cols) {
    if (col %in% names(dat)) {
      dat[[col]] <- as.factor(dat[[col]])
    }
  }

  dat
}

