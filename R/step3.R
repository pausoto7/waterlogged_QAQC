#' Waterlevel conversion (kPa -> m) using barometric compensation
#'
#' @param input_data Dataframe containing timeseries of raw water level and baro:
#'   required cols: site_station_code, timestamp, waterpress_kPa,
#'                  watertemp_C, baro_data
#' @param select_station Character: either a single site_station_code,
#'   or "all"/NA/NULL to process all stations in input_data.
#' @param reference_data_path Character: path to CSV with manual measurements,
#'   expected (at minimum):
#'   - site_station_code
#'   - timestamp
#'   - stage_m  (if reference_type = "stage")
#'   - depth_m  (if reference_type = "depth")
#'   plus any extra columns (e.g., site_comments, timestamp_comments)
#' @param reference_type Either "stage" or "depth" (default = "stage").
#'   Controls which reference column is used.
#' @param select_measurement Integer; which reference row to use (default = 1).
#'   Applied per station (for ALL mode it is the nth row *for each station*).
#' @param logger_type_expected Pattern to check in input_data$logger_type
#'   for QA (default "u20").
#' @param path_to_output_folder Folder where yearly /processed CSVs will be written.
#'
#' @return list(
#'   site_wl   = dataframe of converted waterlevel (single or all stations),
#'   ref_dat   = dataframe of reference measurements used,
#'   wl_plot   = QAQC plot (plotly), single or facetted by station
#' )
#' @export


source("R/step3_utils.R")

convert_waterlevel_kPa_m <- function(input_data,
                                     select_station,
                                     reference_data,      # path or data.frame
                                     reference_type = "stage",
                                     select_measurement = 1,
                                     logger_type_expected = "u20",
                                     path_to_output_folder) {
  ## 1) Read & QAQC reference data -------------------------------
  reference_data <- QAQC_reference_data(reference_data)
  
  ## 2) QAQC input_data -----------------------------------------
  input_data <- QAQC_wl_kpa_inputs(
    input_data,
    logger_type_expected = logger_type_expected
  )
  
  ## 3) Clean reference_type ------------------------------------
  ref_type_clean <- tolower(trimws(reference_type))
  if (!ref_type_clean %in% c("stage", "depth")) {
    stop("reference_type must be either 'stage' or 'depth'.")
  }
  ref_level_col <- if (ref_type_clean == "stage") "stage_m" else "depth_m"
  
  ## 4) Ensure reference_data has needed cols -------------------
  needed_ref_cols <- c("site_station_code", "timestamp", ref_level_col)
  missing_ref_cols <- setdiff(needed_ref_cols, names(reference_data))
  if (length(missing_ref_cols) > 0) {
    stop(
      "Reference data is missing required column(s) for reference_type = '", ref_type_clean, "': ",
      paste(missing_ref_cols, collapse = ", "),
      "\nExpected (for '", ref_type_clean, "') columns: ",
      paste(needed_ref_cols, collapse = ", ")
    )
  }
  
  ## 5) Handle select_station = "all" / NA / missing ------------
  if (missing(select_station) ||
      is.null(select_station) ||
      (length(select_station) == 1 &&
       (is.na(select_station) || tolower(select_station) == "all"))) {
    
    stations <- sort(unique(input_data$site_station_code))
    if (!length(stations)) {
      stop("No site_station_code values found in input_data.")
    }
    
    cat(
      "Running waterlevel conversion for ALL stations:",
      paste(stations, collapse = ", "),
      "\n"
    )
    
    site_list <- list()
    ref_list  <- list()
    
    for (stn in stations) {
      cat("Processing station:", stn, "\n")
      
      res <- tryCatch(
        convert_waterlevel_single(
          input_data            = input_data,
          select_station        = stn,
          reference_data        = reference_data,
          ref_type_clean        = ref_type_clean,
          ref_level_col         = ref_level_col,
          select_measurement    = select_measurement,
          path_to_output_folder = path_to_output_folder
        ),
        error = function(e) {
          msg <- conditionMessage(e)
          warning(
            sprintf("Station %s failed with error:\n  %s", stn, msg),
            call. = FALSE
          )
          return(NULL)
        }
      )
      
      if (!is.null(res)) {
        site_list[[stn]] <- res$site_wl
        ref_list[[stn]]  <- res$ref_dat
      }
    }
    
    if (!length(site_list)) {
      stop("No stations could be successfully processed.")
    }
    
    site_wl_all <- dplyr::bind_rows(site_list)
    ref_all     <- dplyr::bind_rows(ref_list)
    
    # Facetted QAQC plot across all stations
    p_all <- ggplot2::ggplot(site_wl_all,
                             ggplot2::aes(x = timestamp, y = waterlevel_m)) +
      ggplot2::geom_line(color = "#233d4d") +
      ggplot2::geom_point(
        ggplot2::aes(y = ref_m),
        color = "#fe7f2d",
        size  = 2
      ) +
      ggplot2::facet_wrap(~ site_station_code, scales = "free_y") +
      ggplot2::labs(
        title = "kPa to m conversion (all stations)",
        x     = "Timestamp",
        y     = "Water level (m)"
      ) +
      ggplot2::theme_classic()
    
    message("Results returned as list: [[1]] all waterlevel data, [[2]] all reference data, [[3]] facetted QAQC plot")
    
    return(list(
      site_wl = site_wl_all,
      ref_dat = ref_all,
      wl_plot = plotly::ggplotly(p_all)
    ))
  }
  
  ## 6) Single-station mode -------------------------------------
  
  res_single <- convert_waterlevel_single(
    input_data            = input_data,
    select_station        = select_station,
    reference_data        = reference_data,
    ref_type_clean        = ref_type_clean,
    ref_level_col         = ref_level_col,
    select_measurement    = select_measurement,
    path_to_output_folder = path_to_output_folder
  )
  
  message("Results returned as list: [[1]] waterlevel data, [[2]] reference data, [[3]] QAQC plot")
  
  return(res_single)
}
