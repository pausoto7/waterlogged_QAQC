
 source("R/add_nearest_baro_helpers.R")


add_nearest_baro <- function(input_data, path_to_output_folder,
                             baro_data_path,
                             metric = c("water level", "DO", "both"), 
                             max_km = 30, 
                             baro_site_selection = "auto",
                             metadata_path){
  
  metadata <- QAQC_metadata(metadata_path)
  
  # only convert to posixct format if it's not already in the format. If you convert twice, lubridate has an error with midnight that causes it to go NA
  if (!is.POSIXct(input_data$timestamp)){
    input_data$timestamp <- lubridate::ymd_hms(input_data$timestamp)
  }
  
  
  # QC + normalization for metrics param ----
  metrics_info <- resolve_metrics_param(metric)
  metrics_raw       <- metrics_info$metrics_raw
  metrics_need_baro <- metrics_info$metrics_need_baro
  
  # normalize metric names in input_data and metadata ----
  input_data <- input_data %>%
    dplyr::mutate(metric_norm = normalize_string(metric))
  
  metadata <- metadata %>%
    dplyr::mutate(metric_norm = normalize_string(metric))
  
  # keep only rows that need baro ----
  input_data <- input_data %>%
    dplyr::filter(metric_norm %in% metrics_need_baro)
  
  if (!nrow(input_data)) {
    stop(
      "No rows found requiring baro for metrics: ",
      paste(metrics_need_baro, collapse = ", "),
      call. = FALSE
    )
  }
  
  

  
  #make sure all paths have trailing slash
  if (!endsWith(path_to_output_folder, "/")) {
    path_to_output_folder <- paste0(path_to_output_folder, "/")
  }

  # function for creating csv file name
  escape_regex <- function(x) gsub("([][{}()+*^$|\\.?\\\\-])", "\\\\\\1", x)
  
  
  # manually choose baro site to attach logger to ---------------------------------
  if (baro_site_selection != "auto"){
    
    
    # 1) Validate that the requested baro site exists in metadata
    baro_meta <- metadata %>%
      dplyr::filter(metric == "barometric",
                    site_station_code == baro_site_selection)
    
    if (nrow(baro_meta) == 0) {
      stop("Selected barometric site '", baro_site_selection,
           "' not found in metadata with metric == 'barometric'.")
    }
    
    baro_stn <- baro_site_selection
    
    # 2) Find all BARO files for that station under baro_data_path (all years, recursive)
    baro_pattern <- paste0("^", escape_regex(baro_stn), "_BARO_.*\\.csv$")
    
    file_paths <- list.files(
      baro_data_path,
      pattern   = baro_pattern,
      full.names = TRUE,
      recursive  = TRUE
    )
    
    print(list.files(baro_data_path, pattern = "_BARO_", recursive = TRUE))
    
    if (!length(file_paths)) {
      stop("No BARO files found for selected site: ", baro_stn,
           " under ", baro_data_path)
    }
    
    # 3) Read + bind those CSVs
    file <- dplyr::bind_rows(
      lapply(file_paths, read.csv, stringsAsFactors = FALSE, check.names = FALSE)
    )
    
    # 4) Ensure timestamp column exists and convert to POSIXct
    if (!"timestamp" %in% names(file)) {
      ts_cand <- intersect(c("Timestamp", "Date Time", "datetime", "date_time"),
                           names(file))
      if (length(ts_cand) == 0)
        stop("No timestamp column found in BARO files for ", baro_stn)
      
      file <- dplyr::rename(file, timestamp = !!rlang::sym(ts_cand[1]))
    }
    
    file$timestamp <- lubridate::ymd_hms(file$timestamp, tz = "UTC")
    
    # 5) Build wide_press (just one baro column) and wide_temp
    press_clean <- file %>%
      dplyr::select(timestamp, airpress_kPa) %>%
      dplyr::rename(!!baro_stn := airpress_kPa)
    
    if ("airtemp_C" %in% names(file)) {
      temp_clean <- file %>%
        dplyr::select(timestamp, airtemp_C) %>%
        dplyr::rename(!!baro_stn := airtemp_C)
    } else {
      temp_clean <- dplyr::tibble(
        timestamp = press_clean$timestamp,
        !!baro_stn := NA_real_
      )
    }
    
    wide_press <- press_clean %>% dplyr::arrange(timestamp)
    wide_temp  <- temp_clean  %>% dplyr::arrange(timestamp)
    
    baro_col <- baro_stn  # single baro column name
    
    # 6) Build wide_baro_df for manual mode (no substitutes, no regression)
    wide_baro_df <- wide_press %>%
      dplyr::mutate(
        airpress_kPa       = .data[[baro_col]],  # just this station
        baro_site_stn_code = baro_col,
        baro_qaqc_adj      = NA_character_,
        baro_qaqc_note     = "manual barometric selection",
        baro_qaqc_code     = "MANUAL_BARO"
      ) %>%
      dplyr::left_join(
        wide_temp %>%
          dplyr::transmute(
            timestamp,
            airtemp_C = .data[[baro_col]]
          ),
        by = "timestamp"
      )
    
    df_with_baro <- input_data %>%
      dplyr::left_join(
        wide_baro_df %>%
          dplyr::select(
            timestamp, airpress_kPa, baro_site_stn_code, airtemp_C,
            baro_qaqc_adj, baro_qaqc_note, baro_qaqc_code
          ),
        by = "timestamp"
      ) %>%
      dplyr::arrange(site_station_code, timestamp)
    
  } else {

    # Make column names unambiguous, then do a cartesian pairing
    site_coords <- metadata %>% 
      dplyr::filter(metric_norm %in% metrics_need_baro) %>%   
      dplyr::select(site_code = site_station_code, latitude, longitude) %>%
      dplyr::distinct()
    
    baro_coords <- metadata %>%
      dplyr::filter(metric == "barometric") %>%
      dplyr::select(baro_code = site_station_code, baro_lat = latitude, baro_lon = longitude) %>%
      dplyr::distinct()
    
    # All site × baro pairs, then compute distance & keep up to 3 within max_km
    site_baro_rank <- tidyr::crossing(site_coords, baro_coords) %>%
      dplyr::mutate(
        dist_m = geosphere::distHaversine(
          cbind(longitude, latitude),
          cbind(baro_lon,  baro_lat)
        )
      ) %>%
      dplyr::group_by(site_code) %>%
      dplyr::arrange(dist_m, .by_group = TRUE) %>%
      dplyr::mutate(rank = dplyr::row_number()) %>%
      dplyr::filter(dist_m <= max_km * 1000, rank <= 3) %>%
      dplyr::ungroup()
    
    # All baro stations we might ever need (for any site)
    baro_site_station_codes <- unique(site_baro_rank$baro_code)
    
    # Load all baro series once
    baro_press_list <- list()
    baro_temp_list  <- list()
    
    for (baro_stn in baro_site_station_codes) {
      baro_pattern <- paste0("^", escape_regex(baro_stn), "_BARO_.*\\.csv$")
      file_path <- list.files(
        baro_data_path,
        pattern    = baro_pattern,
        full.names = TRUE,
        recursive  = TRUE
      )
      
      if (!length(file_path)) {
        warning("No baro files found for ", baro_stn)
        next
      }
      
      file <- dplyr::bind_rows(
        lapply(file_path, read.csv, stringsAsFactors = FALSE, check.names = FALSE)
      )
      
      # ensure timestamp exists and is POSIXct
      if (!"timestamp" %in% names(file)) {
        ts_cand <- intersect(
          c("Timestamp", "Date Time", "datetime", "date_time"),
          names(file)
        )
        if (length(ts_cand) == 0)
          stop("No timestamp column in file(s) for ", baro_stn)
        file <- dplyr::rename(file, timestamp = !!rlang::sym(ts_cand[1]))
      }
      
      file$timestamp <- lubridate::ymd_hms(file$timestamp, tz = "UTC")
      
      # station-named pressure & temp columns
      press_clean <- file %>%
        dplyr::select(timestamp, airpress_kPa) %>%
        dplyr::rename(!!baro_stn := airpress_kPa)
      
      if ("airtemp_C" %in% names(file)) {
        temp_clean <- file %>%
          dplyr::select(timestamp, airtemp_C) %>%
          dplyr::rename(!!baro_stn := airtemp_C)
      } else {
        temp_clean <- dplyr::tibble(
          timestamp = press_clean$timestamp,
          !!baro_stn := NA_real_
        )
      }
      
      baro_press_list[[baro_stn]] <- press_clean
      baro_temp_list[[baro_stn]]  <- temp_clean
    }
    
    if (!length(baro_press_list))
      stop("No usable barometric files found under: ", baro_data_path)
    if (!length(baro_temp_list))
      stop("No usable barometric temperature files found under: ", baro_data_path)
    
    # Map metric names to official naming codes (once)
    metric_code_map <- list(
      "waterlevel"      = "WL",
      "wl"              = "WL",
      "do"              = "DO",
      "dissolvedoxygen" = "DO"
    )
    
    
    # Helper for % missing
    pct_missing <- function(x) {
      if (is.null(x)) return(NA_real_)
      n <- length(x)
      if (n == 0) return(NA_real_)
      round(mean(is.na(x)) * 100, 1)
    }
    
    # Process one station at a time
    sites <- unique(input_data$site_station_code)
    df_with_baro_list <- list()
    
    for (site_i in sites) {
      
      site_data <- input_data %>%
        dplyr::filter(site_station_code == site_i)
      
      if (!nrow(site_data)) next
      
      # nearest baros for THIS site
      baros_for_site <- site_baro_rank %>%
        dplyr::filter(site_code == site_i) %>%
        dplyr::pull(baro_code) %>%
        unique()
      
      if (!length(baros_for_site)) {
        warning("No barometric stations within ", max_km,
                " km for site ", site_i, ". Skipping.")
        next
      }
      
      # build wide_press / wide_temp only for this site's baros
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
        warning("No baro columns for site ", site_i, " after merge.")
        next
      }
      
      ref_col  <- baro_cols[1]
      calib <- calibrate_baro_series(
        wide_press  = wide_press_site,
        baro_cols   = baro_cols,
        ref_col     = ref_col,
        min_overlap = 10L
      )
      
      wide_press_site <- calib$wide_press
      use_adj <- paste0(baro_cols, "_adj")
      
      # Build baro_data + site-specific baro_site_stn_code
      wide_baro_df_site <- wide_press_site %>%
        dplyr::mutate(
          airpress_kPa = dplyr::coalesce(!!!rlang::syms(use_adj))
        )
      
      baro_site_stn_code <- apply(
        wide_baro_df_site[use_adj],
        1,
        function(row) {
          idx <- which(!is.na(row))[1]
          if (length(idx) == 0 || is.na(idx)) {
            NA_character_
          } else {
            baro_cols[idx]
          }
        }
      )
      wide_baro_df_site$baro_site_stn_code <- baro_site_stn_code
      
      # temp coalesce
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
      
      # Missing pressure warning for this site
      pct <- if ("airpress_kPa" %in% names(wide_baro_df_site)) {
        pct_missing(wide_baro_df_site$airpress_kPa)
      } else {
        NA_real_
      }
      if (!is.na(pct) && pct > 0) {
        warning(sprintf(
          "Baro data missing for %.1f%% of timestamps for site %s.",
          pct, site_i
        ))
      }
      
      # Usage summary for THIS site
      baro_usage_summary <- wide_baro_df_site %>%
        dplyr::count(baro_site_stn_code, name = "count") %>%
        dplyr::mutate(percent = round(100 * count / sum(count), 1)) %>%
        dplyr::arrange(dplyr::desc(percent))
      
      cat("\nBaro usage summary (% of timestamps) for site", site_i, ":\n")
      print(baro_usage_summary)
      
      # find gaps for THIS site's baro_data
      na_gaps <- find_na_runs(
        wide_baro_df_site,
        value_col = "airpress_kPa"
      )
      if (nrow(na_gaps) > 0) {
        cat("Gaps located in baro data for site", site_i, ":\n")
        print(na_gaps)
      }
      
      # Join site_data with its baro data
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
      
      # Write yearly processed files for this site
      metrics_site <- unique(df_site_with_baro$metric_norm)
      
      for (metric_key in metrics_site) {
        metric_rows <- df_site_with_baro %>%
          dplyr::filter(metric_norm == metric_key)
        if (!nrow(metric_rows)) next
        
        # pick header based on metric_key
        metric_header <- if (metric_key %in% names(metric_code_map)) {
          metric_code_map[[metric_key]]
        } else {
          toupper(metric_key)
        }
        
        years_i <- unique(lubridate::year(metric_rows$timestamp))
        
        for (yy in years_i) {
          site_year <- metric_rows %>%
            dplyr::filter(lubridate::year(timestamp) == yy)
          if (!nrow(site_year)) next
          
          start_str <- gsub("\\D", "", as.character(min(lubridate::date(site_year$timestamp))))
          end_str   <- gsub("\\D", "", as.character(max(lubridate::date(site_year$timestamp))))
          
          year_dir <- file.path(path_to_output_folder, yy)
          proc_dir <- file.path(year_dir, "processed")
          if (!dir.exists(year_dir)) dir.create(year_dir, recursive = TRUE)
          if (!dir.exists(proc_dir)) dir.create(proc_dir, recursive = TRUE)
          
          site_year$timestamp <- as.character(site_year$timestamp)
          
          out_name <- sprintf(
            "%s_%s_%s_%s_v0.2.csv",
            site_i, metric_header, start_str, end_str
          )
          out_path <- file.path(proc_dir, out_name)
          
          utils::write.csv(site_year, out_path, row.names = FALSE)
          message(sprintf(
            "Data (v0.2) from %s (%s) added to %s/processed",
            site_i, metric_header, yy
          ))
        }
      }
      df_with_baro_list[[site_i]] <- df_site_with_baro
      
    }
    
    df_with_baro <- dplyr::bind_rows(df_with_baro_list)
    
  }
  # ---- QC plot + return (unchanged) ----
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
      title  = "Baro stitch QC by water-level site",
      x      = "Time",
      y      = "Air pressure (kPa)",
      color  = "Baro code"
    )
  
  return(list(df_with_baro, qc_plot))
}


