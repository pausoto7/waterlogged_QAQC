write_clean_data <- function(
    input_data,
    path_to_output_folder,
    metric
) {
  # ---- config by metric ----
  metric_config <- list(
    dissolvedoxygen = list(
      header   = "DO",
      required = c(
        "site_station_code",
        "timestamp",
        "do_mgl",
        "do_mgl_adj",
        "do_percsat",
        "do_percsat_adj"
      ),
      optional = c(
        "watertemp_C",
        "watertemp_C_adj"
      )
    ),
    barometric = list(
      header   = "BARO",
      required = c(
        "site_station_code",
        "timestamp",
        "airpress_kPa",
        "airpress_kPa_adj"
      ),
      optional = c(
        "airtemp_C",
        "airtemp_C_adj"
      )
    ),
    waterlevel = list(
      header   = "WL",
      required = c(
        "site_station_code",
        "timestamp",
        "waterlevel_m",
        "waterlevel_m_adj"
      ),
      optional = c(
        "watertemp_C",
        "watertemp_C_adj",
        "waterlevel_reference"
      )
    ),
    watertemp = list(
      header   = "WT",
      required = c(
        "site_station_code",
        "timestamp",
        "watertemp_C",
        "watertemp_C_adj"
      ),
      optional = character(0)
    ),
    airtemp = list(
      header   = "AT",
      required = c(
        "site_station_code",
        "timestamp",
        "airtemp_C"
      ),
      optional = c("airtemp_C_adj")
    )
  )
  
  cfg <- metric_config[[metric]]
  if (is.null(cfg)) {
    stop("write_clean_data(): Unsupported metric: ", metric)
  }
  
  # optional safety: if there's a metric column, make sure it matches
  if ("metric" %in% names(input_data)) {
    metric_in_data <- unique(input_data$metric)
    if (length(metric_in_data) > 1L) {
      stop("write_clean_data(): input_data contains multiple metrics: ",
           paste(metric_in_data, collapse = ", "))
    }
    if (!identical(metric_in_data, metric)) {
      warning("write_clean_data(): metric argument ('", metric,
              "') does not match metric column in data ('",
              metric_in_data, "'). Using argument.")
    }
  }
  
  # ---- required columns must exist ------------------------------------------
  missing_required <- setdiff(cfg$required, names(input_data))
  if (length(missing_required) > 0) {
    stop(
      "write_clean_data(): Missing required column(s) for metric '",
      metric, "': ",
      paste(missing_required, collapse = ", ")
    )
  }
  
  # ensure base output folder exists
  if (!dir.exists(path_to_output_folder)) {
    dir.create(path_to_output_folder, recursive = TRUE)
  }
  
  # columns to keep: required + optional (if present) + any flag_/edit_ cols
  base_cols <- c(
    cfg$required,
    intersect(cfg$optional, names(input_data))
  )
  
  flag_cols <- grep("^flag_", names(input_data), value = TRUE)
  edit_cols <- grep("^edit_", names(input_data), value = TRUE)
  
  cols_to_keep <- unique(c(base_cols, flag_cols, edit_cols))
  
  # ---- subset and add year ---------------------------------------------------
  output_data <- input_data[, cols_to_keep, drop = FALSE]
  
  if (!("timestamp" %in% names(output_data))) {
    stop("write_clean_data(): 'timestamp' column missing after subsetting.")
  }
  if (!("site_station_code" %in% names(output_data))) {
    stop("write_clean_data(): 'site_station_code' column missing after subsetting.")
  }
  
  output_data$year <- lubridate::year(output_data$timestamp)
  
  # ---- split by station + year using base R ---------------------------------
  split_list <- split(
    output_data,
    list(
      output_data$site_station_code,
      output_data$year
    ),
    drop = TRUE
  )
  
  # helper: ensure year/clean folders exist and return clean path
  ensure_clean_folder <- function(base_path, year) {
    year_chr  <- as.character(year)
    year_path <- file.path(base_path, year_chr)
    clean_path <- file.path(year_path, "clean")
    if (!dir.exists(year_path))  dir.create(year_path)
    if (!dir.exists(clean_path)) dir.create(clean_path)
    clean_path
  }
  
  # ---- iterate over groups and write files -----------------------------------
  for (i in seq_along(split_list)) {
    df_i <- split_list[[i]]
    if (nrow(df_i) == 0) next
    
    site_i <- unique(df_i$site_station_code)
    year_i <- unique(df_i$year)
    
    if (length(site_i) != 1L || length(year_i) != 1L) {
      stop("write_clean_data(): problem with split grouping (site/year not unique).")
    }
    
    # date range for filename
    start_date <- min(lubridate::as_date(df_i$timestamp))
    end_date   <- max(lubridate::as_date(df_i$timestamp))
    
    start_j <- gsub("\\D", "", as.character(start_date))
    end_j   <- gsub("\\D", "", as.character(end_date))
    
    # drop helper year, format timestamp
    df_write <- df_i
    df_write$year <- NULL
    df_write$timestamp <- format(df_write$timestamp)
    
    clean_path <- ensure_clean_folder(path_to_output_folder, year_i)
    
    file_out <- file.path(
      clean_path,
      paste0(
        site_i, "_", cfg$header, "_",
        start_j, "_", end_j, "_v1.0.csv"
      )
    )
    
    utils::write.csv(df_write, file_out, row.names = FALSE)
    
    message("Clean data (v1.0) from ", site_i,
            " written to ", year_i,
            " (", basename(file_out), ")")
  }
  
  invisible(input_data)
}
