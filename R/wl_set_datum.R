#' Set an arbitrary datum for water-level time series using survey level information.
#'
#' Creates a consistent arbitrary elevation system for a station by anchoring the
#' first valid levelling visit's primary benchmark (PBM) at 100.0 m and translating
#' all other benchmarks using within-visit survey differences.
#'
#' If later visits use a different PBM, its arbitrary elevation is inferred from the
#' existing benchmark network (so it is NOT forced to 100.0 m).
#'
#' The function then computes a single constant datum offset from the first valid
#' visit's water-surface elevation and applies it to the station's logger stage series,
#' creating `wl_elev_m`.
#'
#' Grade handling:
#' - Grade is expected in the level-runs CSV per visit.
#' - No filtering by grade is performed.
#' - If the first valid visit is graded "Poor" a warning is issued.
#'
#' Expected logger columns (in `input_data`):
#' - `site_station_code`
#' - `timestamp` (POSIXct or parseable)
#' - `waterlevel_m_adj` (preferred) OR `waterlevel_m`
#'
#' Expected level-runs CSV columns (minimum):
#' - `site_station_code`
#' - `visit_date`  (e.g. "2025-04-19" or Excel-like "4/19/2025")
#' - `visit_time`  (e.g. "16:26" or "16:26:00")
#' - `primary_bm`  (e.g. "BM1", "BM2", "TBM1" – must match a BM*/TBM* column name)
#' - `grade`       ("Excellent", "Good", "OK", "Poor")
#' - One or more benchmark columns named `BM*` or `TBM*`
#'   (e.g. `BM1`, `BM2`, `TBM1`, `TBM_A`).
#' - One or more water-surface columns named `water_elev_<n>` (e.g. water_elev_1, water_elev_2)
#'
#' Survey values in benchmark columns and `water_elev_*` are assumed to be in the same
#' per-visit survey coordinate system (e.g., "0.12", "0.19", "1.652", etc.).
#' The absolute arbitrary elevation is constructed as:
#'   abs_elev = pbm_abs + (value_raw - pbm_raw)
#'
#' QA/QC bookkeeping:
#' - Adds/updates `wl_elev_m` for the station
#' - Adds/updates `flag_wl_datum_applied` (TRUE for station rows with wl_elev_m)
#' - Appends a QA/QC log entry with code `WL_DATUM_APPLIED`
#'
#' @param input_data Logger data frame (one or more stations).
#' @param level_runs_path Path to the levelling runs CSV.
#' @param select_station Station code to process.
#' @param log_root Root folder where QA/QC logs are stored.
#' @param user Analyst name for the QA/QC log.
#'
#' @return `input_data` with `wl_elev_m` and `flag_wl_datum_applied` added/updated
#'   for `select_station`.
#'
#' @export
#' @import dplyr
#' @importFrom lubridate parse_date_time ymd_hms
#' @importFrom utils read.csv
wl_set_datum <- function(input_data,
                         level_runs_path,
                         select_station,
                         log_root,
                         user = Sys.info()[["user"]]) {
  
  # ---- 0. Basic checks -------------------------------------------------------
  req_cols <- c("site_station_code", "timestamp")
  missing_logger <- setdiff(req_cols, names(input_data))
  if (length(missing_logger) > 0) {
    stop(
      "wl_set_datum(): input_data is missing required column(s): ",
      paste(missing_logger, collapse = ", "),
      call. = FALSE
    )
  }
  
  # pick stage column (no col-name params)
  stage_col <- NULL
  if ("waterlevel_m_adj" %in% names(input_data)) {
    stage_col <- "waterlevel_m_adj"
  } else if ("waterlevel_m" %in% names(input_data)) {
    stage_col <- "waterlevel_m"
  } else {
    stop(
      "wl_set_datum(): input_data must contain `waterlevel_m_adj` or `waterlevel_m`.",
      call. = FALSE
    )
  }
  
  # subset station
  logger_data <- input_data %>%
    dplyr::filter(site_station_code == select_station) %>%
    dplyr::arrange(timestamp)
  
  if (nrow(logger_data) == 0) {
    stop(
      "wl_set_datum(): no rows found for station '", select_station, "'.",
      call. = FALSE
    )
  }
  
  # timestamp parse if needed
  if (!inherits(logger_data$timestamp, "POSIXct")) {
    logger_data$timestamp <- lubridate::ymd_hms(logger_data$timestamp, tz = "UTC")
    if (any(is.na(logger_data$timestamp))) {
      stop("wl_set_datum(): could not parse logger timestamps to POSIXct.", call. = FALSE)
    }
  }
  
  # ---- 1. Read level runs ----------------------------------------------------
  if (!is.character(level_runs_path) || length(level_runs_path) != 1L) {
    stop("wl_set_datum(): `level_runs_path` must be a single file path.", call. = FALSE)
  }
  if (!file.exists(level_runs_path)) {
    stop("wl_set_datum(): level runs file not found: ", level_runs_path, call. = FALSE)
  }
  
  lv <- utils::read.csv(level_runs_path, stringsAsFactors = FALSE, check.names = FALSE)
  
  req_lv <- c("site_station_code", "visit_date", "visit_time", "primary_bm", "grade")
  missing_lv <- setdiff(req_lv, names(lv))
  if (length(missing_lv) > 0) {
    stop(
      "wl_set_datum(): level runs file is missing required column(s): ",
      paste(missing_lv, collapse = ", "),
      call. = FALSE
    )
  }
  
  # restrict to station
  lv <- lv %>%
    dplyr::filter(site_station_code == select_station)
  
  if (nrow(lv) == 0) {
    stop("wl_set_datum(): no level-run rows for station '", select_station, "'.", call. = FALSE)
  }
  
  # benchmark + water elevation columns
  bm_cols <- grep("^(BM|TBM)", names(lv), value = TRUE)
  we_cols <- grep("^water_elev_", names(lv), value = TRUE)
  
  if (length(bm_cols) == 0) {
    stop(
      "wl_set_datum(): no benchmark columns found. ",
      "Expected columns starting with 'BM' or 'TBM' (e.g. BM1, TBM1).",
      call. = FALSE
    )
  }
  if (length(we_cols) == 0) {
    stop(
      "wl_set_datum(): no water elevation columns found (expected `water_elev_<n>`).",
      call. = FALSE
    )
  }
  
  # build visit datetime from separate date + time (Excel-friendly)
  lv$visit_dt <- lubridate::parse_date_time(
    paste(lv$visit_date, lv$visit_time),
    orders = c(
      "ymd HMS", "ymd HM",
      "mdy HMS", "mdy HM",
      "dmy HMS", "dmy HM"
    ),
    tz = "UTC"
  )
  
  if (any(is.na(lv$visit_dt))) {
    bad <- which(is.na(lv$visit_dt))
    stop(
      "wl_set_datum(): could not parse visit_date/visit_time on row(s): ",
      paste(bad, collapse = ", "),
      call. = FALSE
    )
  }
  
  # normalize grade text
  lv$grade <- trimws(lv$grade)
  grade_ok <- lv$grade %in% c("Excellent", "Good", "OK", "Poor")
  if (!all(grade_ok)) {
    bad <- which(!grade_ok)
    stop(
      "wl_set_datum(): invalid grade on row(s): ",
      paste(bad, collapse = ", "),
      ". Allowed: Excellent, Good, OK, Poor.",
      call. = FALSE
    )
  }
  
  # primary_bm must match an existing BM/TBM column exactly
  lv$primary_bm <- trimws(lv$primary_bm)
  missing_pbm <- which(!lv$primary_bm %in% bm_cols)
  if (length(missing_pbm) > 0) {
    stop(
      "wl_set_datum(): `primary_bm` does not match any BM/TBM column on row(s): ",
      paste(missing_pbm, collapse = ", "),
      ". `primary_bm` must exactly match one of: ",
      paste(bm_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  # coerce BM/TBM and water_elev columns to numeric
  for (nm in c(bm_cols, we_cols)) {
    lv[[nm]] <- suppressWarnings(as.numeric(lv[[nm]]))
  }
  
  lv <- lv %>% dplyr::arrange(visit_dt)
  
  # ---- 2. Choose first valid visit (anchor) ---------------------------------
  # valid = has primary BM value AND at least one water_elev value
  is_valid <- rep(TRUE, nrow(lv))
  for (i in seq_len(nrow(lv))) {
    pbm_col_i <- lv$primary_bm[i]
    pbm_raw_i <- lv[[pbm_col_i]][i]
    
    we_vals <- unlist(lv[i, we_cols, drop = FALSE])
    we_mean <- mean(we_vals, na.rm = TRUE)
    
    if (is.na(pbm_raw_i) || is.na(we_mean)) {
      is_valid[i] <- FALSE
    }
  }
  
  if (!any(is_valid)) {
    stop(
      "wl_set_datum(): no valid visits. Need primary_bm value and at least one non-NA water_elev_*.",
      call. = FALSE
    )
  }
  
  first_idx <- which(is_valid)[1]
  
  if (lv$grade[first_idx] == "Poor") {
    warning(
      "wl_set_datum(): First visit used to define datum is graded 'Poor'. ",
      "All subsequent elevations will be relative to this anchor.",
      call. = FALSE
    )
  }
  
  # ---- 3. Build benchmark network across visits ------------------------------
  # bm_abs_map: named numeric vector stored as list for easy lookup
  bm_abs_map <- list()
  
  # anchor: primary BM at 100.0
  anchor_pbm_id  <- lv$primary_bm[first_idx]   # e.g., "BM1"
  anchor_pbm_raw <- lv[[anchor_pbm_id]][first_idx]
  
  bm_abs_map[[anchor_pbm_id]] <- 100.0
  
  infer_pbm_abs <- function(pbm_id, pbm_raw, visit_row) {
    # If PBM abs isn't known, infer it from any BM/TBM in this visit that IS known.
    for (bm_id in bm_cols) {
      known_abs <- bm_abs_map[[bm_id]]
      bm_raw    <- visit_row[[bm_id]]
      if (!is.null(known_abs) && !is.na(bm_raw)) {
        # bm_abs = pbm_abs + (bm_raw - pbm_raw)  => pbm_abs = bm_abs - (bm_raw - pbm_raw)
        return(known_abs - (bm_raw - pbm_raw))
      }
    }
    NA_real_
  }
  
  # iterate visits in time order and populate bm_abs_map
  for (i in seq_len(nrow(lv))) {
    visit_row <- lv[i, , drop = FALSE]
    
    pbm_id_i  <- visit_row$primary_bm
    pbm_raw_i <- visit_row[[pbm_id_i]]
    
    if (is.na(pbm_raw_i)) next
    
    pbm_abs_i <- bm_abs_map[[pbm_id_i]]
    if (is.null(pbm_abs_i)) {
      pbm_abs_i <- infer_pbm_abs(pbm_id_i, pbm_raw_i, visit_row)
      if (is.na(pbm_abs_i)) {
        stop(
          "wl_set_datum(): cannot connect visit at ", as.character(visit_row$visit_dt),
          " to existing benchmark network (primary_bm '", pbm_id_i,
          "' not known and no other known BM/TBM present).",
          call. = FALSE
        )
      }
      bm_abs_map[[pbm_id_i]] <- pbm_abs_i
    }
    
    # update all BM/TBM abs values seen on this visit
    for (bm_id in bm_cols) {
      bm_raw <- visit_row[[bm_id]]
      if (is.na(bm_raw)) next
      
      bm_abs_new <- pbm_abs_i + (bm_raw - pbm_raw_i)
      
      if (!is.null(bm_abs_map[[bm_id]])) {
        if (abs(bm_abs_map[[bm_id]] - bm_abs_new) > 0.02) {
          warning(
            "wl_set_datum(): benchmark '", bm_id,
            "' has inconsistent inferred elevation across visits (>0.02 m). ",
            "Keeping first value. Visit time: ", as.character(visit_row$visit_dt),
            call. = FALSE
          )
        }
      } else {
        bm_abs_map[[bm_id]] <- bm_abs_new
      }
    }
  }
  
  # ---- 4. Compute anchor visit water elevation (absolute) --------------------
  anchor_row    <- lv[first_idx, , drop = FALSE]
  anchor_we_raw <- mean(unlist(anchor_row[we_cols]), na.rm = TRUE)
  
  anchor_pbm_abs <- bm_abs_map[[anchor_pbm_id]]
  
  # absolute arbitrary water elevation at anchor visit
  anchor_water_abs <- anchor_pbm_abs + (anchor_we_raw - anchor_pbm_raw)
  
  # ---- 5. Compute constant datum offset using nearest logger sample ----------
  anchor_dt <- anchor_row$visit_dt
  
  dt_sec <- abs(as.numeric(logger_data$timestamp - anchor_dt, units = "secs"))
  j <- which.min(dt_sec)
  
  # require within 1 hour (hard requirement for datum)
  if (is.na(dt_sec[j]) || dt_sec[j] > 3600) {
    stop(
      "wl_set_datum(): anchor visit is more than 1 hour from nearest logger sample. ",
      "Anchor visit: ", as.character(anchor_dt),
      call. = FALSE
    )
  }
  
  stage_at_anchor <- logger_data[[stage_col]][j]
  if (is.na(stage_at_anchor)) {
    stop("wl_set_datum(): nearest logger stage is NA at anchor visit; cannot set datum.", call. = FALSE)
  }
  
  datum_offset_m <- anchor_water_abs - stage_at_anchor
  
  # ---- 6. Apply datum to station time series --------------------------------
  logger_data$wl_elev_m <- logger_data[[stage_col]] + datum_offset_m
  
  if (!"flag_wl_datum_applied" %in% names(logger_data)) {
    logger_data$flag_wl_datum_applied <- TRUE
  } else {
    logger_data$flag_wl_datum_applied <- TRUE
  }
  
  # ---- ensure columns exist in full input_data with correct types ------------
  # wl_elev_m must be numeric
  if (!"wl_elev_m" %in% names(input_data) || !is.numeric(input_data$wl_elev_m)) {
    input_data$wl_elev_m <- NA_real_
  }
  
  # flag must be logical
  if (!"flag_wl_datum_applied" %in% names(input_data) || !is.logical(input_data$flag_wl_datum_applied)) {
    input_data$flag_wl_datum_applied <- FALSE
  }
  
  # any other new columns (future-proofing)
  new_cols <- setdiff(names(logger_data), names(input_data))
  if (length(new_cols) > 0) {
    for (nm in new_cols) {
      if (nm == "wl_elev_m") {
        input_data[[nm]] <- NA_real_
      } else if (startsWith(nm, "flag_") || startsWith(nm, "edit_")) {
        input_data[[nm]] <- NA  # logical NA
      } else {
        input_data[[nm]] <- NA
      }
    }
  }
  
  # bring station rows back into the full dataset
  input_data <- dplyr::rows_update(
    input_data,
    logger_data,
    by = c("site_station_code", "timestamp")
  )
  
  # ---- 7. QA/QC log ----------------------------------------------------------
  ts_for_log <- c(min(logger_data$timestamp, na.rm = TRUE),
                  max(logger_data$timestamp, na.rm = TRUE))
  
  log_row <- make_qaqc_log_row(
    timestamps  = ts_for_log,
    station     = select_station,
    metric      = "WL",
    field       = "wl_elev_m",
    action      = "DATUM_SET",
    code        = "WL_DATUM_APPLIED",
    action_note = paste0(
      "Applied arbitrary datum using first valid levelling visit (",
      as.character(anchor_dt), "). Primary BM '", anchor_pbm_id,
      "' anchored at 100.0 m. Datum offset = ", round(datum_offset_m, 3),
      " m (wl_elev_m = ", stage_col, " + offset)."
    ),
    manual_note = "AUTOMATIC DATUM SET",
    fun_name    = "wl_set_datum",
    user        = user
  )
  
  log_path <- qaqc_log_path(log_root, select_station, "WL")
  qaqc_log_append(
    log_path = log_path,
    station  = select_station,
    metric   = "WL",
    log_rows = log_row
  )
  
  message("WL datum QA/QC log updated: ", log_path)
  
  input_data
}
