#step 2

baro_bound <- bind_hobo_files(path_to_raw_folder = "data/testing/raw/baro", 
                              path_to_output_folder = "data/testing/processed", 
                              metadata_path = "data/testing/raw/testing_metadata.csv")

level_bound <- bind_hobo_files(path_to_raw_folder = "data/testing/raw/level", 
                              path_to_output_folder = "data/testing/processed", 
                              metadata_path = "data/testing/raw/testing_metadata.csv")


input_data = level_bound[[1]]
path_to_output_folder = "data/testing/processed"

max_km <- 30

baro_data_path <- "data/testing/processed"

source("R/step2_utils.R")


add_nearest_baro <- function(input_data, path_to_output_folder, baro_data_path,
                             baro_site_selection = "auto",
                             metadata_path){

  # only convert to posixct format if it's not already in the format. If you convert twice, lubridate has an error with midnight that causes it to go NA
  if (!is.POSIXct(input_data$timestamp)){
    input_data$timestamp <- lubridate::ymd_hms(input_data$timestamp)
    #baro_data$timestamp <- lubridate::ymd_hms(baro_data$timestamp)
  }

  metadata <- QAQC_metadata(metadata_path)
  
  
  #make sure all paths have trailing slash
  if (!endsWith(path_to_output_folder, "/")) {
    path_to_output_folder <- paste0(path_to_output_folder, "/")
  }

  # function for creating csv file name
  escape_regex <- function(x) gsub("([][{}()+*^$|\\.?\\\\-])", "\\\\\\1", x)
  
  
  # manually choose baro site to attach logger to
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
        baro_data          = .data[[baro_col]],        # just this station
        baro_site_stn_code = baro_col,
        baro_qaqc_adj      = NA_character_,
        baro_qaqc_note     = "manual barometric selection",
        baro_qaqc_code     = "MANUAL_BARO"
      ) %>%
      dplyr::left_join(
        wide_temp %>%
          dplyr::transmute(
            timestamp,
            baro_temp = .data[[baro_col]]
          ),
        by = "timestamp"
      )
    
    
    
  }else{
  
    
    # Make column names unambiguous, then do a cartesian pairing
    site_coords <- metadata %>% 
      filter(metric %in% unique(input_data$metric)) %>%   
      select(site_code = site_station_code, latitude, longitude) %>%
      distinct()
    
    baro_coords <- metadata %>%
      dplyr::filter(metric == "barometric") %>%
      dplyr::select(baro_code = site_station_code, baro_lat = latitude, baro_lon = longitude) %>%
      dplyr::distinct()
    
    # All site × baro pairs, then compute distance
    site_baro_rank <- tidyr::crossing(site_coords, baro_coords) %>%
      mutate(dist_m = geosphere::distHaversine(
        cbind(longitude, latitude),
        cbind(baro_lon,  baro_lat))) %>%
      group_by(site_code) %>%
      arrange(dist_m, .by_group = TRUE) %>%
      mutate(rank = row_number()) %>%
      filter(dist_m <= max_km*1000, rank <= 3) %>%
      ungroup()

    # for use in for loop
    baro_site_station_codes <- unique(site_baro_rank$baro_code)

    
    # goes through each of the closest baro stations, pulls from files, puts into a list of dataframes which will be used to sub later on
    baro_press_list <- list()
    baro_temp_list  <- list()
    
    for (baro_stn in baro_site_station_codes){
      baro_pattern <- paste0("^", escape_regex(baro_stn), "_BARO_.*\\.csv$")
      file_path <- list.files(baro_data_path, pattern = baro_pattern, full.names = TRUE, recursive = TRUE)
      
      if (!length(file_path)) {
        warning("No baro files found for ", baro_stn)
        next
      }
      # bind and then read any csv's that are relevant 
      file <- dplyr::bind_rows(
        lapply(file_path, read.csv, stringsAsFactors = FALSE, check.names = FALSE)
      )      
      
      # ensure timestamp exists and is POSIXct
      # has multiple file names in case in the future with other files or formats datetime col is different
      if (!"timestamp" %in% names(file)) {
        ts_cand <- intersect(c("Timestamp","Date Time","datetime","date_time"), names(file))
        
        if (length(ts_cand) == 0) stop("No timestamp column in file(s) for ", baro_stn)
        file <- dplyr::rename(file, timestamp = !!rlang::sym(ts_cand[1]))
      }
      
      # put into posicxt 
      file$timestamp <- lubridate::ymd_hms(file$timestamp, tz = "UTC")
      
      # build station-named pressure & temp columns
      press_clean <- dplyr::select(file, timestamp, airpress_kPa) %>%
        dplyr::rename(!!baro_stn := airpress_kPa)
      
      # temp may be missing in some exports; guard it ################################ REVIEW ############################
      if ("airtemp_C" %in% names(file)) {
        temp_clean <- dplyr::select(file, timestamp, airtemp_C) %>%
          dplyr::rename(!!baro_stn := airtemp_C)
      } else {
        temp_clean <- dplyr::tibble(timestamp = press_clean$timestamp) %>%
          dplyr::mutate(!!baro_stn := NA_real_)
      }
      
      baro_press_list[[baro_stn]] <- press_clean
      baro_temp_list[[baro_stn]]  <- temp_clean
    }
    
    if (!length(baro_press_list)) stop("No usable barometric files found under: ", baro_data_path)
    if (!length(baro_temp_list))  stop("No usable barometric temperature files found under: ", baro_data_path)
    

    wide_press <- purrr::reduce(baro_press_list, full_join, by = "timestamp") %>% 
      arrange(timestamp)
    wide_temp  <- purrr::reduce(baro_temp_list,  full_join, by = "timestamp") %>% 
      arrange(timestamp)
    

    # The baro columns should be the 2nd–4th columns of wide_baro_df (after timestamp) in order of relevance 
    baro_cols <- setdiff(names(wide_press), "timestamp") # finds col names other than "timestamp"
    if (length(baro_cols) == 0) stop("No baro columns in wide_press")
    
    
    ref_col   <- baro_cols[1]
    alt_cols  <- baro_cols[-1]
    

    calib <- calibrate_baro_series(
      wide_press  = wide_press,
      baro_cols   = baro_cols,
      ref_col     = ref_col,
      min_overlap = 10L
    )
    
    
    
    wide_press   <- calib$wide_press
    reg_summary  <- calib$reg_summary
    
    # #####
    ### This section needs to be qaqc'd more comprehensively with real data
    ### 

    use_adj <- paste0(baro_cols, "_adj")

    
    # After calibrate_baro_series() and after you define baro_cols/use_adj:
    
    wide_baro_df <- wide_press %>%
      dplyr::mutate(
        baro_data = dplyr::coalesce(!!!rlang::syms(use_adj))
      )
    
    baro_site_stn_code <- apply(wide_baro_df[use_adj], 1, function(row) {
      idx <- which(!is.na(row))[1]
      if (length(idx) == 0 || is.na(idx)) {
        NA_character_
      } else {
        baro_cols[idx]
      }
    })
    
    wide_baro_df$baro_site_stn_code <- baro_site_stn_code
    
    wide_baro_df <- wide_baro_df %>%
      dplyr::mutate(
        baro_qaqc_adj  = NA_character_,
        baro_qaqc_note = NA_character_,
        baro_qaqc_code = NA_character_
      )
    
    
    temp_syms <- rlang::syms(baro_cols)
    
    baro_temp_df <- wide_temp %>%
      dplyr::transmute(
        timestamp,
        baro_temp = dplyr::coalesce(!!!temp_syms)
      )
    
    
    wide_baro_df <- wide_baro_df  %>%
      dplyr::left_join(baro_temp_df, by = "timestamp")
    
    # warn if any pressure still missing

    pct_missing <- function(x) {
      if (is.null(x)) return(NA_real_)
      n <- length(x)
      if (n == 0) return(NA_real_)
      round(mean(is.na(x)) * 100, 1)
    }
    
    pct <- if ("baro_data" %in% names(wide_baro_df)) pct_missing(wide_baro_df$baro_data) else NA_real_
    
    if (!is.na(pct) && pct > 0) {
      warning(sprintf("Baro data missing for %.1f%% of timestamps.", pct))
    }
    
    
    # usage summary
    baro_usage_summary <- wide_baro_df %>%
      dplyr::count(baro_site_stn_code, name = "count") %>%
      dplyr::mutate(percent = round(100 * count / sum(count), 1)) %>%
      dplyr::arrange(dplyr::desc(percent))
    print("Baro usage summary (% of timestamps):")
    print(baro_usage_summary)
    
    ##  Gap finder + write joined output -----------------------------------
    
 
    # find any gaps within the data set
    na_gaps <- find_na_runs(wide_baro_df, value_col = "baro_data") # currently missing "gaps" at front and end of data set
    if (nrow(na_gaps) > 0){
      print("Gaps located in baro data")
      print(na_gaps)
    }
    
    logger_header <- unique(input_data$metric)
    

    # If you only have wide_baro_df (timestamp + baro_data + baro_site_stn_code),
    # join it into input_data so we have site_station_code alongside baro_data:
    df_with_baro <- input_data %>%
      dplyr::left_join(
        wide_baro_df %>% dplyr::select(timestamp, baro_data, baro_site_stn_code, baro_temp,
                                       baro_qaqc_adj, baro_qaqc_note, baro_qaqc_code),
        by = "timestamp"
      ) %>%
      dplyr::arrange(site_station_code, timestamp)
    
    sites <- unique(df_with_baro$site_station_code)
    
    for (site_i in sites) {
      site_dat <- df_with_baro[df_with_baro$site_station_code == site_i, , drop = FALSE]
      if (!nrow(site_dat)) next
      
      years_i <- unique(lubridate::year(site_dat$timestamp))
      for (yy in years_i) {
        site_year <- dplyr::filter(site_dat, lubridate::year(timestamp) == yy)
        if (!nrow(site_year)) next
        
        start_str <- gsub("\\D", "", as.character(min(lubridate::date(site_year$timestamp))))
        end_str   <- gsub("\\D", "", as.character(max(lubridate::date(site_year$timestamp))))
        
        # create year/processed directories if missing
        year_dir <- file.path(path_to_output_folder, yy)
        proc_dir <- file.path(year_dir, "processed")
        if (!dir.exists(year_dir)) dir.create(year_dir, recursive = TRUE)
        if (!dir.exists(proc_dir)) dir.create(proc_dir, recursive = TRUE)
        
        # fix midnight format quirks (export as character to keep exact text form if desired)
        site_year$timestamp <- as.character(site_year$timestamp)
        
        out_name <- sprintf("%s_%s_%s_%s_v0.2.csv", site_i, logger_header, start_str, end_str)
        out_path <- file.path(proc_dir, out_name)
        
        utils::write.csv(site_year, out_path, row.names = FALSE)
        message(sprintf("Data (v0.2) from %s added to %s/processed", site_i, yy))
      }
    }
    
  }
  
  
  qc_plot <- df_with_baro %>%
    dplyr::filter(!is.na(site_station_code)) %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x     = timestamp,
        y     = baro_data,
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
} # end of function


# nearest_baro <- add_nearest_baro(input_data, path_to_output_folder, baro_data_path,
#                              baro_site_selection = "auto",
#                              metadata_path)
# 


