#step 2

baro_bound <- bind_hobo_files(path_to_raw_folder = "data/deadwood/raw/baro", 
                              path_to_output_folder = "data/deadwood/processed", 
                              logger_type = "U20", 
                              metadata_path = "data/deadwood/raw/deadwood_metadata.csv")

level_bound <- bind_hobo_files(path_to_raw_folder = "data/deadwood/raw/level", 
                              path_to_output_folder = "data/deadwood/processed", 
                              logger_type = "U20", 
                              metadata_path = "data/deadwood/raw/deadwood_metadata.csv")


input_data = level_bound[[1]]
input_logger_type = "u20"
path_to_output_folder = "data/deadwood/processed"
baro_data = baro_bound[[1]]



test <- read.csv("data/deadwood/processed/2025/processed/DWD_WL_20250128_20250703_v0.1.csv")

metadata <- read.csv("data/deadwood/raw/deadwood_metadata.csv")

max_km <- 30

baro_data_path <- "data/deadwood/processed/2025/processed"

source("R/step2_utils.R")




add_nearest_baro <- function(input_data, input_logger_type, path_to_output_folder, baro_data_path,
                             baro_site_selection = "auto",
                             #var_airpress_kPa = "airpress_kPa_U20", 
                             #var_airtemp_C = "airtemp_C_U20",
                             metadata){
  
  # we can't assume the user is inputting the df straight from the previous frame, so change timestamp to datetime format
  input_data$timestamp <- lubridate::ymd_hms(input_data$timestamp)
  baro_data$timestamp <- lubridate::ymd_hms(baro_data$timestamp)
  
  
  #make sure all paths have trailing slash
  if (!endsWith(path_to_output_folder, "/")) {
    path_to_output_folder <- paste0(path_to_output_folder, "/")
  }

  
  if (baro_site_selection != "auto"){
    
    baro_site = metadata %>%
      filter(site_station_code == baro_site_selection)
      
    
    # 
    # 
    # # Validate against metadata (optional but nice)
    # known_baro <- metadata %>%
    #   dplyr::filter(metric == "barometric") %>%
    #   dplyr::pull(site_station_code) %>%
    #   unique()
    # missing_codes <- setdiff(manual_codes, known_baro)
    # if (length(missing_codes)) {
    #   warning("These manual baro codes are not in metadata (continuing anyway): ",
    #           paste(missing_codes, collapse = ", "))
    # }
    # 
    # # Build lists for pressure & temp (same structure as auto)
    # baro_press_list <- list()
    # baro_temp_list  <- list()
    # 
    # escape_regex <- function(x) gsub("([][{}()+*^$|\\.?\\\\-])", "\\\\\\1", x)
    # 
    # for (baro_stn in manual_codes) {
    #   baro_pattern <- paste0("^", escape_regex(baro_stn), "_BARO_.*\\.csv$")
    #   file_paths <- list.files(baro_data_path, pattern = baro_pattern,
    #                            full.names = TRUE, recursive = TRUE)
    #   if (!length(file_paths)) {
    #     warning("No BARO files found for manual code: ", baro_stn)
    #     next
    #   }
    #   
    #   df <- dplyr::bind_rows(lapply(file_paths, read.csv, stringsAsFactors = FALSE, check.names = FALSE))
    #   
    #   # ensure timestamp
    #   if (!"timestamp" %in% names(df)) {
    #     ts_cand <- intersect(c("Timestamp","Date Time","datetime","date_time"), names(df))
    #     if (!length(ts_cand)) stop("No timestamp column found for ", baro_stn)
    #     df <- dplyr::rename(df, timestamp = !!rlang::sym(ts_cand[1]))
    #   }
    #   df$timestamp <- lubridate::ymd_hms(df$timestamp, tz = "UTC")
    #   
    #   # pressure
    #   if (!"airpress_kPa" %in% names(df)) stop("Missing 'airpress_kPa' in files for ", baro_stn)
    #   press_clean <- df %>% dplyr::select(timestamp, airpress_kPa) %>%
    #     dplyr::rename(!!baro_stn := airpress_kPa)
    #   
    #   # temperature (optional)
    #   if ("airtemp_C" %in% names(df)) {
    #     temp_clean <- df %>% dplyr::select(timestamp, airtemp_C) %>%
    #       dplyr::rename(!!baro_stn := airtemp_C)
    #   } else {
    #     temp_clean <- dplyr::tibble(timestamp = press_clean$timestamp, !!baro_stn := NA_real_)
    #   }
    #   
    #   baro_press_list[[baro_stn]] <- press_clean
    #   baro_temp_list[[baro_stn]]  <- temp_clean
    # }
    # 
    # # Wide matrices
    # if (!length(baro_press_list)) stop("No usable BARO data found for the manual selection.")
    # wide_press <- purrr::reduce(baro_press_list, dplyr::full_join, by = "timestamp") %>% dplyr::arrange(timestamp)
    # wide_temp  <- purrr::reduce(baro_temp_list,  dplyr::full_join, by = "timestamp") %>% dplyr::arrange(timestamp)
    # 
    # # From here down, REUSE your existing pressure offset + coalesce logic,
    # # just replace 'wide_baro_df' with 'wide_press' during computation,
    # # and coalesce temperature using the same baro column order.
    # 
    # baro_cols <- setdiff(names(wide_press), "timestamp")
    # if (!length(baro_cols)) stop("No baro columns in manual wide_press.")
    # ref_col  <- baro_cols[1]
    # alt_cols <- baro_cols[-1]
    # 
    # offset_map <- numeric(0)
    # for (alt in alt_cols) {
    #   overlap_idx <- which(!is.na(wide_press[[ref_col]]) & !is.na(wide_press[[alt]]))
    #   if (length(overlap_idx)) {
    #     i <- overlap_idx[1]
    #     press_diff <- wide_press[[ref_col]][i] - wide_press[[alt]][i]
    #     wide_press[[paste0(alt, "_adj")]] <- wide_press[[alt]] + press_diff
    #     offset_map[alt] <- press_diff
    #   } else {
    #     wide_press[[paste0(alt, "_adj")]] <- wide_press[[alt]]
    #     offset_map[alt] <- NA_real_
    #   }
    # }
    # wide_press[[paste0(ref_col, "_adj")]] <- wide_press[[ref_col]]
    # 
    # use_cols <- c(ref_col, paste0(alt_cols, "_adj"))
    # use_syms <- rlang::syms(intersect(use_cols, names(wide_press)))
    # 
    # wide_press <- wide_press %>%
    #   dplyr::mutate(
    #     baro_data = dplyr::coalesce(!!!use_syms),
    #     baro_site_stn_code = dplyr::case_when(
    #       !!!purrr::imap(set_names(use_cols), function(col, i) {
    #         rlang::expr(!is.na(.data[[!!col]]) ~ !!sub("_adj$", "", col))
    #       }),
    #       TRUE ~ NA_character_
    #     ),
    #     offset_used   = unname(offset_map[baro_site_stn_code]),
    #     baro_qaqc_adj = dplyr::if_else(!is.na(offset_used), paste("added", round(offset_used, 3)), NA_character_),
    #     baro_qaqc_note= dplyr::if_else(!is.na(offset_used), "added difference in pressure prior to change", NA_character_),
    #     baro_qaqc_code= dplyr::if_else(!is.na(offset_used), "ALT_BARO_CORR", NA_character_)
    #   ) %>%
    #   dplyr::select(-offset_used)
    # 
    # # temperature coalesce in same order
    # temp_syms <- rlang::syms(baro_cols)
    # baro_temp_df <- wide_temp %>%
    #   dplyr::transmute(timestamp, baro_temp = dplyr::coalesce(!!!temp_syms))
    # 
    # # Final object same shape as auto mode
    # wide_baro_df <- wide_press %>%
    #   dplyr::left_join(baro_temp_df, by = "timestamp")
    # 
    # # warning + usage summary (same as your auto path)
    # missing_pct <- round(mean(is.na(wide_baro_df$baro_data)) * 100, 1)
    # if (missing_pct > 0) warning(sprintf("Baro data missing for %.1f%% of timestamps.", missing_pct))
    # 
    # baro_usage_summary <- wide_baro_df %>%
    #   dplyr::count(baro_site_stn_code, name = "count") %>%
    #   dplyr::mutate(percent = round(100 * count / sum(count), 1)) %>%
    #   dplyr::arrange(dplyr::desc(percent))
    # print("Baro usage summary (% of timestamps):")
    # print(baro_usage_summary)
    # 
    
    
  }else{
  
    
    # Make column names unambiguous, then do a Cartesian pairing
    site_coords <- metadata %>% 
      filter(metric %in% unique(input_data$metric)) %>%   
      select(site_code = site_station_code, latitude, longitude) %>%
      distinct()
    
    baro_coords <- metadata %>%
      filter(metric == "barometric") %>%
      select(baro_code = site_station_code, baro_lat = latitude, baro_lon = longitude) %>%
      distinct()
    
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
    escape_regex <- function(x) gsub("([][{}()+*^$|\\.?\\\\-])", "\\\\\\1", x)
    
    
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
      if (!"timestamp" %in% names(file)) {
        ts_cand <- intersect(c("Timestamp","Date Time","datetime","date_time"), names(file))
        if (length(ts_cand) == 0) stop("No timestamp column in file(s) for ", baro_stn)
        file <- dplyr::rename(file, timestamp = !!rlang::sym(ts_cand[1]))
      }
      file$timestamp <- lubridate::ymd_hms(file$timestamp, tz = "UTC")
      
      # build station-named pressure & temp columns
      press_clean <- dplyr::select(file, timestamp, airpress_kPa) %>%
        dplyr::rename(!!baro_stn := airpress_kPa)
      
      # temp may be missing in some exports; guard it
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
    

    wide_press <- purrr::reduce(baro_press_list, full_join, by = "timestamp") %>% arrange(timestamp)
    wide_temp  <- purrr::reduce(baro_temp_list,  full_join, by = "timestamp") %>% arrange(timestamp)
    

    # The baro columns should be the 2nd–4th columns of wide_baro_df (after timestamp) in order of relevance 
    baro_cols <- setdiff(names(wide_press), "timestamp")
    if (length(baro_cols) == 0) stop("No baro columns in wide_press")
    
    
    ref_col   <- baro_cols[1]
    alt_cols  <- baro_cols[-1]
    
    offset_map <- numeric(0)
    for (alt in alt_cols) {
      overlap_idx <- which(!is.na(wide_press[[ref_col]]) & !is.na(wide_press[[alt]]))
      if (length(overlap_idx)) {
        i <- overlap_idx[1]
        press_diff <- wide_press[[ref_col]][i] - wide_press[[alt]][i]
        wide_press[[paste0(alt, "_adj")]] <- wide_press[[alt]] + press_diff
        offset_map[alt] <- press_diff
      } else {
        wide_press[[paste0(alt, "_adj")]] <- wide_press[[alt]]
        offset_map[alt] <- NA_real_
      }
    }
    wide_press[[paste0(ref_col, "_adj")]] <- wide_press[[ref_col]]
    
    
    
    # #####
    ### This section needs to be qaqc'd more comprehensively with real data
    ### 
    
    
    # ref_col: single string; alt_cols: character vector or NULL
    # wide_press: your data.frame
    
    # 1) Normalize alt_cols safely
    alt_cols_chr <- if (is.null(alt_cols)) character(0) else as.character(alt_cols)
    alt_cols_chr <- trimws(alt_cols_chr)
    
    # Drop NA/empty after trim
    alt_cols_norm <- alt_cols_chr[ nzchar(alt_cols_chr) & !is.na(alt_cols_chr) ]
    alt_cols_norm <- unique(alt_cols_norm)
    
    # 2) Allow inputs already ending in "_adj"; unify to base names
    base_alts <- ifelse(grepl("_adj$", alt_cols_norm), sub("_adj$", "", alt_cols_norm), alt_cols_norm)
    
    # 3) Build requested adjusted columns and keep only those present
    requested_alt_adj <- paste0(base_alts, "_adj")
    alt_adj_cols_present <- intersect(requested_alt_adj, names(wide_press))
    
    # 4) Final coalescing order: ref first, then existing alts
    use_cols <- unique(c(ref_col, alt_adj_cols_present))
    use_syms <- rlang::syms(use_cols)
    
    # 5) Optional, targeted warnings
    missing_alt_adj <- setdiff(requested_alt_adj, names(wide_press))
    if (length(base_alts) > 0 && length(missing_alt_adj) > 0) {
      warning("Missing alternate columns in wide_press: ", paste(missing_alt_adj, collapse = ", "))
    }
    
    # # (Optional) sanity check for ref_col too
    # if (!ref_col %in% names(wide_press)) {
    #   warning("ref_col not found in wide_press: ", ref_col)
    # }
    # 
    wide_baro_df <- wide_press %>%
      dplyr::mutate(
        baro_data = dplyr::coalesce(!!!use_syms),
        baro_site_stn_code = dplyr::case_when(
          !!!purrr::imap(set_names(use_cols), function(col, i) {
            rlang::expr(!is.na(.data[[!!col]]) ~ !!sub("_adj$", "", col))
          }),
          TRUE ~ NA_character_
        ),
        offset_used = unname(offset_map[baro_site_stn_code]),
        baro_qaqc_adj  = dplyr::if_else(!is.na(offset_used), paste("added", round(offset_used, 3)), NA_character_),
        baro_qaqc_note = dplyr::if_else(!is.na(offset_used), "added difference in pressure prior to change", NA_character_),
        baro_qaqc_code = dplyr::if_else(!is.na(offset_used), "ALT_BARO_CORR", NA_character_)
      ) %>%
      dplyr::select(-offset_used)
    
    
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
    
    # (Optional) alias for compatibility with old script naming
    # wide_baro_df <- wide_baro_df %>%
    #   dplyr::mutate(airpress_kPa_adj = baro_data)
    # 
    # 

    
    
    ###
    # ADD LATER CODE TO INFILL HOLES?
    ###
    
    
    na_gaps <- find_na_runs(wide_baro_df, value_col = "baro_data")
    print("Gaps located in baro data")
    print(na_gaps)
    
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
  
  
  qc_plot <- ggplot2::ggplot(wide_baro_df, ggplot2::aes(x = timestamp, y = baro_data)) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_point(ggplot2::aes(color = baro_site_stn_code), size = 0.9) +
    ggplot2::theme_classic() +
    ggplot2::labs(title = "Baro stitch QC", x = "Time", y = "Air pressure (kPa)", color = "Baro code")
  print(qc_plot)
  
  return(list(df_with_baro, qc_plot))
} # end of function

