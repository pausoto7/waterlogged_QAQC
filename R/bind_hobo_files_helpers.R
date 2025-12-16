#Bind_hobo_utils

# READ METADATA FILE, CONVERT datetime to correct format, check that removal is after deployment
QAQC_metadata <- function(metadata_path) {
  tryCatch({
    # 1) File check
    if (!file.exists(metadata_path)) {
      stop("File not found: ", metadata_path)
    }
    
    # 2) Read CSV (quietly, no factors)
    metadata <- read.csv(metadata_path, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
    
    # Remove completely empty rows (all NA or all blank strings)
    metadata <- metadata[rowSums(is.na(metadata) | metadata == "") != ncol(metadata), ]
    
    # 3) Required columns present?
    required_cols <- c("site_station_code", "timestamp_deploy", "timestamp_remove", 
                       "model", "sn", "metric", "status", "latitude", "longitude")
    
    missing_cols  <- setdiff(required_cols, names(metadata))
    if (length(missing_cols)) {
      stop("Missing required column(s): ", paste(missing_cols, collapse = ", "))
    }
    
    # 4) Parse timestamps (expect mdy_hm; auto-UTC)
    parse_dt <- function(x) {
      x_chr <- as.character(x)
      p1 <- try(lubridate::mdy_hm(x_chr, tz = "UTC"), silent = TRUE)
      if (!inherits(p1, "try-error") && !all(is.na(p1))) return(p1)
      p2 <- suppressWarnings(
        lubridate::parse_date_time(
          x_chr,
          orders = c("mdy HM", "mdy HMS", "mdY HM", "mdY HMS", "ymd HM", "ymd HMS"),
          tz = "UTC"
        )
      )
      if (all(is.na(p2))) {
        stop("Failed to parse timestamp column. Expected mdy_hm like '4/30/2025 13:45' in UTC.")
      }
      p2
    }
    metadata$timestamp_deploy <- parse_dt(metadata$timestamp_deploy)
    metadata$timestamp_remove <- parse_dt(metadata$timestamp_remove)
    
    # 4) Warn if any timestamps are NA after parsing 
    
    ####
    # NA values in timestamp_remove are expected if the logger is still logging
    # update to produce an error if the is a value and the timestamp does not parse but ignore if the row was read in as "NA"
    ####
    
    na_deploy <- which(is.na(metadata$timestamp_deploy))
    na_remove <- which(is.na(metadata$timestamp_remove))
    if (length(na_deploy)) warning("Missing/invalid 'timestamp_deploy' on row(s): ", paste(na_deploy, collapse = ", "))
    if (length(na_remove)) warning("Missing/invalid 'timestamp_remove' on row(s): ", paste(na_remove, collapse = ", "))
    
    # 5) check deploy > remove
    if (any(metadata$timestamp_deploy > metadata$timestamp_remove, na.rm = TRUE)) {
      bad_rows <- which(metadata$timestamp_deploy > metadata$timestamp_remove)
      stop("Deploy time is after remove time on row(s): ", paste(bad_rows, collapse = ", "))
    }
    
    # 6) station number as character
    if ("sn" %in% names(metadata)) {
      metadata$sn <- as.character(metadata$sn)  # preserve leading zeros if any
    }
    
    # 7) Latitude / Longitude QAQC (decimal degrees expected)
    has_lat <- "latitude"  %in% names(metadata)
    has_lon <- "longitude" %in% names(metadata)
    if (!has_lat || !has_lon) {
      warning("Latitude/longitude column(s) missing in metadata (expected 'latitude' and 'longitude').")
    } else {
      lat_chr <- trimws(as.character(metadata$latitude))
      lon_chr <- trimws(as.character(metadata$longitude))
      
      # Looks like DMS or contains N/S/E/W or degree symbols?
      looks_non_decimal <- function(x) grepl("[NSEW°'\"\u00B0]", x, ignore.case = TRUE)
      bad_fmt_rows <- which((!is.na(lat_chr) & looks_non_decimal(lat_chr)) |
                              (!is.na(lon_chr) & looks_non_decimal(lon_chr)))
      if (length(bad_fmt_rows)) {
        warning("Lat/long values appear non-decimal (DMS or N/S/E/W) on row(s): ",
                paste(bad_fmt_rows, collapse = ", "),
                ". Expected decimal degrees (e.g., 49.1234, -123.4567).")
      }
      
      # Coerce to numeric (non-numeric -> NA)
      metadata$latitude  <- suppressWarnings(as.numeric(lat_chr))
      metadata$longitude <- suppressWarnings(as.numeric(lon_chr))
      
      # Missingness warnings
      miss_lat <- which(is.na(metadata$latitude))
      miss_lon <- which(is.na(metadata$longitude))
      if (length(miss_lat)) warning("Missing latitude (decimal degrees) on row(s): ", paste(miss_lat, collapse = ", "))
      if (length(miss_lon)) warning("Missing longitude (decimal degrees) on row(s): ", paste(miss_lon, collapse = ", "))
      
      # Only-one-missing per row
      one_missing <- which(xor(is.na(metadata$latitude), is.na(metadata$longitude)))
      if (length(one_missing)) {
        warning("Only one of latitude/longitude supplied on row(s): ", paste(one_missing, collapse = ", "))
      }
      
      # Range checks
      bad_lat <- which(!is.na(metadata$latitude)  & (metadata$latitude  < -90  | metadata$latitude  > 90))
      bad_lon <- which(!is.na(metadata$longitude) & (metadata$longitude < -180 | metadata$longitude > 180))
      if (length(bad_lat)) warning("Latitude out of range [-90, 90] on row(s): ", paste(bad_lat, collapse = ", "))
      if (length(bad_lon)) warning("Longitude out of range [-180, 180] on row(s): ", paste(bad_lon, collapse = ", "))
    }
    
    # 8) Metric / measurement_type QAQC (folded-in qc_measurement_type)
    valid_metrics = c("waterlevel", "barometric", "conductivity", "dissolvedoxygen", "airtemp", "watertemp", "light")
    
    metric_raw <- metadata$metric
    metric_std <- gsub(" ", "", tolower(as.character(metric_raw)))
    
    bad_metric_rows <- which(is.na(metric_std) | !metric_std %in% valid_metrics)
    
    
    if (length(bad_metric_rows) > 0) {
      stop(
        "QC check failed: invalid 'metric' value(s) on row(s): ",
        paste(bad_metric_rows, collapse = ", "),
        "\nValid options are: ",
        paste(valid_metrics, collapse = ", ")
      )
    }
    
    # Overwrite with standardized metric names (lowercase, no spaces)
    metadata$metric <- metric_std
    
    metadata

  }, error = function(e) {
    stop("Error reading/parsing metadata: ", e$message)
  })
}


extract_alldata_from_file <- function(file) {
  # Read raw HOBO file as-is (no name mangling)
  data <- utils::read.csv(
    file,
    header           = FALSE,
    sep              = ",",
    dec              = ".",
    stringsAsFactors = FALSE,
    check.names      = FALSE
  )
  
  file_name <- sub(".*/", "", file, perl = TRUE)
  
  # ---- Parse timezone + logger info from row 2 -------------------------------
  tz_raw <- as.character(data[2, 2])
  tz     <- sub(".*(Date Time.*), ", "", tz_raw)
  
  logger_info <- as.character(data[2, 3])
  logger_sn   <- regmatches(logger_info, regexpr("\\d{8}", logger_info))
  
  # helper: parse "<Type>, <unit> (LGR ...)" -> type + unit
  parse_type_unit <- function(info) {
    info <- as.character(info)
    if (is.na(info) || info == "") {
      return(list(type = NA_character_, unit = NA_character_))
    }
    type <- sub("\\,.*", "", info)              # before first comma
    unit <- sub("\\s*\\(.*", "", info)          # before "("
    unit <- sub(".*, ", "", unit)               # after last ", "
    list(type = trimws(type), unit = trimws(unit))
  }
  
  header_row <- as.character(data[2, ])
  n_cols     <- ncol(data)
  
  # ---- Build metadata for all data columns (>=3 and non-empty) ---------------
  meta_tbl <- tibble::tibble(
    col_index  = seq_len(n_cols),
    raw_header = header_row
  ) %>%
    dplyr::filter(
      col_index >= 3,
      !is.na(raw_header),
      raw_header != ""
    ) %>%
    dplyr::mutate(
      parsed = purrr::map(raw_header, parse_type_unit),
      type   = purrr::map_chr(parsed, "type"),
      unit   = purrr::map_chr(parsed, "unit")
    )
  
  # First two channels kept for backward compatibility
  if (nrow(meta_tbl) >= 1) {
    dat1_type <- meta_tbl$type[1]
    dat1_unit <- meta_tbl$unit[1]
  } else {
    dat1_type <- dat1_unit <- NA_character_
  }
  if (nrow(meta_tbl) >= 2) {
    dat2_type <- meta_tbl$type[2]
    dat2_unit <- meta_tbl$unit[2]
  } else {
    dat2_type <- dat2_unit <- NA_character_
  }
  
  # ---- Trim to data rows + relevant columns ----------------------------------
  # Use serial number as first column
  data$V1 <- logger_sn
  
  # Remove the two header rows, keep SN + timestamp + all data cols we identified
  data_clean <- data[-c(1, 2), c(1, 2, meta_tbl$col_index), drop = FALSE]
  
  # Build measurement column names
  meas_names <- purrr::map2_chr(
    meta_tbl$type,
    meta_tbl$unit,
    function(type, unit) {
      if (is.na(type) && is.na(unit)) {
        NA_character_
      } else {
        paste(type, unit)
      }
    }
  )
  
  # Fallback to the raw header text when parse failed
  meas_names <- ifelse(
    is.na(meas_names) | meas_names == "",
    meta_tbl$raw_header,
    meas_names
  )
  
  new_names <- c("sn", "timestamp", meas_names)
  colnames(data_clean) <- new_names
  
  data_clean <- tibble::as_tibble(data_clean)
  
  # ---- Coerce measurement cols to numeric and drop all-NA rows ---------------
  meas_cols <- setdiff(names(data_clean), c("sn", "timestamp"))
  
  if (length(meas_cols) > 0) {
    # numeric coercion (suppress warnings from non-numeric junk)
    data_clean <- data_clean %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(meas_cols),
          ~ suppressWarnings(as.numeric(.x))
        )
      )
    
    # drop rows where ALL measurement columns are NA
    keep_row <- apply(
      dplyr::select(data_clean, dplyr::all_of(meas_cols)),
      1,
      function(z) any(!is.na(z))
    )
    data_clean <- data_clean[keep_row, , drop = FALSE]
  }
  
  # ---- Attach metadata columns -----------------------------------------------
  data_clean <- data_clean %>%
    dplyr::mutate(
      file_name  = file_name,
      timezone   = tz,
      data1_type = dat1_type,
      data1_unit = dat1_unit,
      data2_type = dat2_type,
      dat2_unit  = dat2_unit
    )
  
  return(data_clean)
}
