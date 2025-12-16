add_nearest_baro <- function(input_data,
                             path_to_output_folder,
                             baro_data_path,
                             metric = c("water level", "DO", "conductivity", "both", "all"),
                             max_km = 30,
                             baro_site_selection = "auto",
                             metadata_path) {
  
  metadata <- QAQC_metadata(metadata_path)
  
  # allow NULL or numeric for max_km ------------------------------------------
  if (!is.null(max_km) && !is.numeric(max_km)) {
    stop("add_nearest_baro(): `max_km` must be numeric or NULL.", call. = FALSE)
  }
  
  # only convert to POSIXct format if it's not already in the format.
  if (!inherits(input_data$timestamp, "POSIXct")) {
    input_data$timestamp <- lubridate::ymd_hms(input_data$timestamp, tz = "UTC")
  }
  
  # QC + normalization for metrics param ----
  metrics_info      <- resolve_metrics_param(metric[1])
  metrics_need_baro <- metrics_info$metrics_need_baro
  
  # normalize metric names in input_data and metadata ----
  input_data <- input_data %>%
    dplyr::mutate(
      metric_norm = dplyr::case_when(
        grepl("cond", tolower(metric)) ~ normalize_string("conductivity"),
        TRUE                          ~ normalize_string(metric)
      )
    )
  
  metadata <- metadata %>%
    dplyr::mutate(metric_norm = normalize_string(metric))
  
  # keep only rows that need baro ----
  input_data <- input_data %>%
    dplyr::filter(metric_norm %in% metrics_need_baro)
  
  if (!nrow(input_data)) {
    stop(
      "add_nearest_baro(): No rows found requiring baro for metrics: ",
      paste(metrics_need_baro, collapse = ", "),
      call. = FALSE
    )
  }
  
  # make sure output path has trailing slash ----
  if (!endsWith(path_to_output_folder, "/")) {
    path_to_output_folder <- paste0(path_to_output_folder, "/")
  }
  
  # function for creating safe regex patterns
  escape_regex <- function(x) gsub("([][{}()+*^$|\\.?\\\\-])", "\\\\\\1", x)
  
  # metric -> file code mapping (bind_hobo_files naming convention)
  metric_code_map <- list(
    "waterlevel"      = "WL",
    "wl"              = "WL",
    "do"              = "DO",
    "dissolvedoxygen" = "DO",
    "conductivity"    = "COND",
    "cond"            = "COND"
  )
  
  metric_to_code <- function(metric_norm_value) {
    if (metric_norm_value %in% names(metric_code_map)) {
      metric_code_map[[metric_norm_value]]
    } else {
      toupper(metric_norm_value)
    }
  }
  
  pct_missing <- function(x) {
    if (is.null(x)) return(NA_real_)
    n <- length(x)
    if (n == 0) return(NA_real_)
    round(mean(is.na(x)) * 100, 1)
  }
  
  df_with_baro <- NULL
  
  ## ---------------------------------------------------------------------------
  ## MANUAL BARO SELECTION
  ## ---------------------------------------------------------------------------
  if (baro_site_selection != "auto") {
    
    baro_meta <- metadata %>%
      dplyr::filter(metric == "barometric",
                    site_station_code == baro_site_selection)
    
    if (nrow(baro_meta) == 0) {
      stop("Selected barometric site '", baro_site_selection,
           "' not found in metadata with metric == 'barometric'.", call. = FALSE)
    }
    
    baro_stn <- baro_site_selection
    
    # Find all BARO files for that station
    baro_pattern <- paste0("^", escape_regex(baro_stn), "_BARO_.*\\.csv$")
    
    file_paths <- list.files(
      baro_data_path,
      pattern    = baro_pattern,
      full.names = TRUE,
      recursive  = TRUE
    )
    
    if (!length(file_paths)) {
      stop("No BARO files found for selected site: ", baro_stn,
           " under ", baro_data_path, call. = FALSE)
    }
    
    file <- dplyr::bind_rows(
      lapply(file_paths, read.csv, stringsAsFactors = FALSE, check.names = FALSE)
    )
    
    # Ensure timestamp exists and convert to POSIXct
    if (!"timestamp" %in% names(file)) {
      ts_cand <- intersect(c("timestamp","Timestamp","Date Time","datetime","date_time"),
                           names(file))
      if (length(ts_cand) == 0) {
        stop("No timestamp column found in BARO files for ", baro_stn, call. = FALSE)
      }
      file <- dplyr::rename(file, timestamp = !!rlang::sym(ts_cand[1]))
    }
    
    file$timestamp <- lubridate::ymd_hms(file$timestamp, tz = "UTC")
    
    # Prefer adjusted columns if present
    press_col <- if ("airpress_kPa_adj" %in% names(file)) "airpress_kPa_adj" else "airpress_kPa"
    temp_col  <- if ("airtemp_C_adj"   %in% names(file)) "airtemp_C_adj"   else "airtemp_C"
    
    if (!press_col %in% names(file)) {
      stop("BARO file(s) for ", baro_stn,
           " are missing airpress_kPa (or airpress_kPa_adj).", call. = FALSE)
    }
    
    press_clean <- file %>%
      dplyr::select(timestamp, !!rlang::sym(press_col)) %>%
      dplyr::rename(airpress_kPa = !!rlang::sym(press_col))
    
    if (!is.null(temp_col) && temp_col %in% names(file)) {
      temp_clean <- file %>%
        dplyr::select(timestamp, !!rlang::sym(temp_col)) %>%
        dplyr::rename(airtemp_C = !!rlang::sym(temp_col))
    } else {
      temp_clean <- dplyr::tibble(timestamp = press_clean$timestamp, airtemp_C = NA_real_)
    }
    
    wide_baro_df <- press_clean %>%
      dplyr::arrange(timestamp) %>%
      dplyr::mutate(
        baro_site_stn_code = baro_stn,
        baro_qaqc_adj      = NA_character_,
        baro_qaqc_note     = "manual barometric selection",
        baro_qaqc_code     = "MANUAL_BARO"
      ) %>%
      dplyr::left_join(temp_clean %>% dplyr::arrange(timestamp), by = "timestamp")
    
    df_with_baro <- input_data %>%
      dplyr::left_join(
        wide_baro_df %>%
          dplyr::select(
            timestamp, airpress_kPa, airtemp_C,
            baro_site_stn_code, baro_qaqc_adj, baro_qaqc_note, baro_qaqc_code
          ),
        by = "timestamp"
      ) %>%
      dplyr::arrange(site_station_code, timestamp)
    
  } else {
    
    ## -------------------------------------------------------------------------
    ## AUTO BARO SELECTION (nearest neighbours, calibration)
    ## -------------------------------------------------------------------------
    
    site_coords <- metadata %>%
      dplyr::filter(metric_norm %in% metrics_need_baro) %>%
      dplyr::select(site_code = site_station_code, latitude, longitude) %>%
      dplyr::distinct()
    
    baro_coords <- metadata %>%
      dplyr::filter(metric == "barometric") %>%
      dplyr::select(baro_code = site_station_code, baro_lat = latitude, baro_lon = longitude) %>%
      dplyr::distinct()
    
    if (!nrow(site_coords)) {
      stop("add_nearest_baro(): No site coordinates found for requested metrics in metadata.", call. = FALSE)
    }
    if (!nrow(baro_coords)) {
      stop("add_nearest_baro(): No barometric stations found in metadata.", call. = FALSE)
    }
    
    # All site × baro pairs, then compute distance & rank ----------------------
    site_baro_rank_raw <- tidyr::crossing(site_coords, baro_coords) %>%
      dplyr::mutate(
        dist_m = geosphere::distHaversine(
          cbind(longitude, latitude),
          cbind(baro_lon,  baro_lat)
        )
      ) %>%
      dplyr::group_by(site_code) %>%
      dplyr::arrange(dist_m, .by_group = TRUE) %>%
      dplyr::mutate(rank = dplyr::row_number()) %>%
      dplyr::ungroup()
    
    # Apply distance filter only if max_km is not NULL -------------------------
    site_baro_rank <- if (is.null(max_km)) {
      site_baro_rank_raw %>%
        dplyr::filter(rank <= 3)
    } else {
      site_baro_rank_raw %>%
        dplyr::filter(dist_m <= max_km * 1000, rank <= 3)
    }
    
    # All baro stations we might ever need (for any site)
    baro_site_station_codes <- unique(site_baro_rank$baro_code)
    
    baro_press_list <- list()
    baro_temp_list  <- list()
    
    for (baro_stn in baro_site_station_codes) {
      
      baro_pattern <- paste0("^", escape_regex(baro_stn), "_BARO_.*\\.csv$")
      
      file_paths <- list.files(
        baro_data_path,
        pattern    = baro_pattern,
        full.names = TRUE,
        recursive  = TRUE
      )
      
      if (!length(file_paths)) {
        warning("add_nearest_baro(): No baro files found for ", baro_stn)
        next
      }
      
      file <- dplyr::bind_rows(
        lapply(file_paths, read.csv, stringsAsFactors = FALSE, check.names = FALSE)
      )
      
      if (!"timestamp" %in% names(file)) {
        ts_cand <- intersect(c("timestamp","Timestamp", "Date Time", "datetime", "date_time"),
                             names(file))
        if (length(ts_cand) == 0) {
          stop("No timestamp column in file(s) for ", baro_stn, call. = FALSE)
        }
        file <- dplyr::rename(file, timestamp = !!rlang::sym(ts_cand[1]))
      }
      
      file$timestamp <- lubridate::ymd_hms(file$timestamp, tz = "UTC")
      
      press_col <- if ("airpress_kPa_adj" %in% names(file)) "airpress_kPa_adj" else "airpress_kPa"
      temp_col  <- if ("airtemp_C_adj"   %in% names(file)) "airtemp_C_adj"   else "airtemp_C"
      
      if (!press_col %in% names(file)) {
        warning("add_nearest_baro(): ", baro_stn, " missing airpress_kPa (or _adj). Skipping.")
        next
      }
      
      press_clean <- file %>%
        dplyr::select(timestamp, !!rlang::sym(press_col)) %>%
        dplyr::rename(!!baro_stn := !!rlang::sym(press_col))
      
      if (!is.null(temp_col) && temp_col %in% names(file)) {
        temp_clean <- file %>%
          dplyr::select(timestamp, !!rlang::sym(temp_col)) %>%
          dplyr::rename(!!baro_stn := !!rlang::sym(temp_col))
      } else {
        temp_clean <- dplyr::tibble(timestamp = press_clean$timestamp, !!baro_stn := NA_real_)
      }
      
      baro_press_list[[baro_stn]] <- press_clean
      baro_temp_list[[baro_stn]]  <- temp_clean
    }
    
    if (!length(baro_press_list)) {
      stop("No usable barometric files found under: ", baro_data_path, call. = FALSE)
    }
    
    sites <- unique(input_data$site_station_code)
    df_with_baro_list <- list()
    
    for (site_i in sites) {
      
      site_data <- input_data %>%
        dplyr::filter(site_station_code == site_i)
      
      if (!nrow(site_data)) next
      
      baros_for_site <- site_baro_rank %>%
        dplyr::filter(site_code == site_i) %>%
        dplyr::pull(baro_code) %>%
        unique()
      
      if (!length(baros_for_site)) {
        if (is.null(max_km)) {
          warning("add_nearest_baro(): No barometric stations found in metadata for site ", site_i, ". Skipping.")
        } else {
          warning("add_nearest_baro(): No barometric stations within ", max_km,
                  " km for site ", site_i, ". Skipping.")
        }
        next
      }
      
      # only keep baros we successfully loaded
      baros_for_site <- baros_for_site[baros_for_site %in% names(baro_press_list)]
      if (!length(baros_for_site)) {
        warning("add_nearest_baro(): No usable baro files available for site ", site_i, ". Skipping.")
        next
      }
      
      wide_press_site <- baros_for_site %>%
        purrr::map(~ baro_press_list[[.x]]) %>%
        purrr::reduce(dplyr::full_join, by = "timestamp") %>%
        dplyr::arrange(timestamp)
      
      wide_temp_site <- baros_for_site %>%
        purrr::map(~ baro_temp_list[[.x]]) %>%
        purrr::reduce(dplyr::full_join, by = "timestamp") %>%
        dplyr::arrange(timestamp)
      
      baro_cols <- setdiff(names(wide_press_site), "timestamp")
      if (length(baro_cols) == 0) {
        warning("add_nearest_baro(): No baro columns for site ", site_i, " after merge. Skipping.")
        next
      }
      
      ref_col <- baro_cols[1]
      
      calib <- calibrate_baro_series(
        wide_press  = wide_press_site,
        baro_cols   = baro_cols,
        ref_col     = ref_col,
        min_overlap = 10L
      )
      
      wide_press_site <- calib$wide_press
      use_adj <- paste0(baro_cols, "_adj")
      
      wide_baro_df_site <- wide_press_site %>%
        dplyr::mutate(
          airpress_kPa = dplyr::coalesce(!!!rlang::syms(use_adj))
        )
      
      baro_site_stn_code <- apply(
        wide_baro_df_site[use_adj],
        1,
        function(row) {
          idx <- which(!is.na(row))[1]
          if (length(idx) == 0 || is.na(idx)) NA_character_ else baro_cols[idx]
        }
      )
      wide_baro_df_site$baro_site_stn_code <- baro_site_stn_code
      
      # temperature: coalesce the raw temp series
      temp_syms <- rlang::syms(baro_cols)
      baro_temp_df_site <- wide_temp_site %>%
        dplyr::transmute(
          timestamp,
          airtemp_C = dplyr::coalesce(!!!temp_syms)
        )
      
      wide_baro_df_site <- wide_baro_df_site %>%
        dplyr::mutate(
          baro_qaqc_adj  = NA_character_,
          baro_qaqc_note = NA_character_,
          baro_qaqc_code = NA_character_
        ) %>%
        dplyr::left_join(baro_temp_df_site, by = "timestamp")
      
      pct <- pct_missing(wide_baro_df_site$airpress_kPa)
      if (!is.na(pct) && pct > 0) {
        warning(sprintf(
          "add_nearest_baro(): Baro data missing for %.1f%% of timestamps for site %s.",
          pct, site_i
        ))
      }
      
      baro_usage_summary <- wide_baro_df_site %>%
        dplyr::count(baro_site_stn_code, name = "count") %>%
        dplyr::mutate(percent = round(100 * count / sum(count), 1)) %>%
        dplyr::arrange(dplyr::desc(percent))
      
      cat("\nBaro usage summary (% of timestamps) for site ", site_i, ":\n", sep = "")
      print(baro_usage_summary)
      
      na_gaps <- find_na_runs(wide_baro_df_site, value_col = "airpress_kPa")
      if (nrow(na_gaps) > 0) {
        cat("Gaps located in baro data for site ", site_i, ":\n", sep = "")
        print(na_gaps)
      }
      
      df_site_with_baro <- site_data %>%
        dplyr::left_join(
          wide_baro_df_site %>%
            dplyr::select(
              timestamp, airpress_kPa, baro_site_stn_code, airtemp_C,
              baro_qaqc_adj, baro_qaqc_note, baro_qaqc_code
            ),
          by = "timestamp"
        ) %>%
        dplyr::arrange(site_station_code, timestamp)
      
      df_with_baro_list[[site_i]] <- df_site_with_baro
    }
    
    df_with_baro <- dplyr::bind_rows(df_with_baro_list)
  }
  
  # ---- WRITE OUTPUTS (same behavior as before, but code-based naming) ----
  sites_out <- unique(df_with_baro$site_station_code)
  
  for (site_i in sites_out) {
    site_dat <- df_with_baro[df_with_baro$site_station_code == site_i, , drop = FALSE]
    if (!nrow(site_dat)) next
    
    metrics_site <- unique(site_dat$metric_norm)
    
    for (metric_key in metrics_site) {
      metric_rows <- dplyr::filter(site_dat, metric_norm == metric_key)
      if (!nrow(metric_rows)) next
      
      metric_header <- metric_to_code(metric_key)
      
      years_i <- unique(lubridate::year(metric_rows$timestamp))
      
      for (yy in years_i) {
        site_year <- dplyr::filter(metric_rows, lubridate::year(timestamp) == yy)
        if (!nrow(site_year)) next
        
        start_str <- gsub("\\D", "", as.character(min(lubridate::date(site_year$timestamp))))
        end_str   <- gsub("\\D", "", as.character(max(lubridate::date(site_year$timestamp))))
        
        year_dir <- file.path(path_to_output_folder, yy)
        proc_dir <- file.path(year_dir, "processed")
        dir.create(proc_dir, recursive = TRUE, showWarnings = FALSE)
        
        # timestamp export as character
        site_year$timestamp <- as.character(site_year$timestamp)
        
        out_name <- sprintf("%s_%s_%s_%s_v0.2.csv", site_i, metric_header, start_str, end_str)
        out_path <- file.path(proc_dir, out_name)
        
        utils::write.csv(site_year, out_path, row.names = FALSE)
        message(sprintf("Data (v0.2) from %s (%s) written to %s", site_i, metric_header, out_path))
      }
    }
  }
  
  # ---- QC plot + return ----
  qc_plot <- df_with_baro %>%
    dplyr::filter(!is.na(site_station_code)) %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x     = timestamp,
        y     = airpress_kPa,
        color = baro_site_stn_code
      )
    ) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::geom_point(size = 0.7, alpha = 0.7) +
    ggplot2::facet_wrap(~ site_station_code, scales = "free_x") +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
    ggplot2::labs(
      title  = "Baro stitch QC by site",
      x      = "Time",
      y      = "Air pressure (kPa)",
      color  = "Baro code"
    )
  
  return(list(df_with_baro, qc_plot))
}
