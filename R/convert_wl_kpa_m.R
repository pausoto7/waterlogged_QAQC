#' Convert water level pressure (kPa) to depth (meters)
#'
#' Converts absolute pressure readings from water level loggers (in kPa) to
#' water depth or stage (in meters) using manual reference measurements.
#' The function performs calibration by matching logger pressure readings to
#' known water levels at specific times, then applies the conversion to the
#' entire time series.
#'
#' @param input_data Data frame containing water level logger data with at
#'   least `site_station_code`, `timestamp`, `waterpress_kPa`, and
#'   `airpress_kPa` columns (typically from [add_nearest_baro()]).
#' @param select_station Character; station code to process. Use `"all"` or
#'   `NA` to process all stations in `input_data`.
#' @param reference_data Either a file path to a CSV containing manual reference
#'   measurements or a data frame with columns `site_station_code`, `timestamp`,
#'   and either `stage_m` or `depth_m`.
#' @param reference_type Character; either `"stage"` or `"depth"` to indicate
#'   which column in `reference_data` contains the reference water level.
#'   Defaults to `"stage"`.
#' @param select_measurement Integer; which reference measurement to use when
#'   multiple measurements exist for a station (defaults to 1, the first/closest).
#' @param logger_type_expected Character; expected logger type for validation
#'   (defaults to `"u20"`). Used to check that pressure data are appropriate.
#' @param path_to_output_folder Directory where yearly v0.3 converted files will
#'   be written (e.g., `"data/processed"`).
#'
#' @details
#' The function converts pressure to water level using the formula:
#' \deqn{waterlevel\_m = (waterpress\_kPa - airpress\_kPa) / 9.81}
#'
#' Calibration adjusts this value to match reference measurements by calculating
#' an offset. When `select_station = "all"`, the function processes all stations
#' and returns combined results with a faceted QAQC plot.
#'
#' Yearly v0.3 CSV files are written to
#' `<path_to_output_folder>/<year>/processed/` with filenames of the form
#' `<station>_WL_<startdate>_<enddate>_v0.3.csv`.
#'
#' @return A list with three elements:
#'   \describe{
#'     \item{[[1]] (site_wl)}{Data frame with converted water level in meters
#'       (`waterlevel_m` column) plus all original columns.}
#'     \item{[[2]] (ref_dat)}{Data frame of reference measurements used for
#'       calibration.}
#'     \item{[[3]] (wl_plot)}{Interactive plotly plot showing converted water
#'       level time series with reference measurements overlaid.}
#'   }
#'
#' @seealso [add_nearest_baro()], [QAQC_reference_data()], [convert_waterlevel_single()]
#'
#' @importFrom dplyr bind_rows
#' @importFrom ggplot2 ggplot aes geom_line geom_point facet_wrap labs theme_classic
#' @importFrom plotly ggplotly
#' @export
convert_waterlevel_kPa_m <- function(input_data,
                                     select_station,
                                     reference_data,
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
