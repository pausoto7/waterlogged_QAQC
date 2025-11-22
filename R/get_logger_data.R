

#' Retrieve and summarise logger data
#'
#' High-level helper to load logger data from disk, optionally filter by station,
#' optionally summarise to a temporal scale, and optionally return plots.
#'
#' This function works with the original wide-format processed files
#' (e.g. columns like \code{waterlevel_m_U20_adj}, \code{do_mgl_U26_adj}).
#' Summarisation is only performed for clean data (v1.0), and only for the
#' measurement variables corresponding to the chosen \code{metric}.
#'
#' @param path_to_data Path to the folder containing the CSV files. The function
#'   will search this folder and its subfolders recursively.
#' @param data_processing One of \code{"raw"}, \code{"v0.1"}, \code{"v0.2"},
#'   \code{"v0.3"}, \code{"v1.0"}, \code{"clean"}. The value is used to select
#'   files by version suffix. \code{"clean"} is treated as \code{"v1.0"}.
#'   Summarisation (i.e., \code{temporal_scale != "none"}) is only allowed for
#'   v1.0/clean data.
#' @param metric A character string indicating which metric group to summarise.
#'   Used only when \code{temporal_scale != "none"}. Accepted values (case and
#'   spaces ignored) are mapped to:
#'   \itemize{
#'     \item \code{"waterlevel"}: water level + water temperature
#'     \item \code{"dissolvedoxygen"}: DO (mg/L, %sat) + water temperature
#'     \item \code{"barometric"}: barometric pressure + air temperature
#'     \item \code{"airtemp"}: air temperature (e.g., TidbiT)
#'     \item \code{"watertemp"}: water temperature (e.g., TidbiT)
#'   }
#' @param select_station Either \code{"all"} (default) or one or more
#'   \code{site_station_code} values to filter on.
#' @param temporal_scale Temporal aggregation. Case and spaces ignored and
#'   normalised to one of: \code{"none"}, \code{"hourly"}, \code{"daily"},
#'   \code{"weekly"}, \code{"monthly"}, \code{"yearly"}. Use \code{"none"} to
#'   return raw timestamps with no summarisation. "Hourly" aggregates to
#'   average-by-hour (using \code{floor_date}).
#' @param return_plots Logical; if \code{TRUE} and \code{temporal_scale != "none"},
#'   returns a list with data and ggplot objects. If \code{FALSE}, returns just
#%'   the data frame.
#'
#' @return
#'   \itemize{
#'     \item If \code{temporal_scale == "none"}: a data frame of combined logger data.
#'     \item Otherwise, if \code{return_plots = FALSE}: a summarised data frame.
#'     \item Otherwise, a list with elements:
#'       \itemize{
#'         \item \code{data}: summarised data frame
#'         \item \code{plot1}, \code{plot2}, \code{plot3}: ggplot objects
#'           (some may be \code{NULL} depending on \code{metric}).
#'       }
#'   }
#'
#' @export
#' @import dplyr
#' @import ggplot2
#' @import lubridate
#' @import purrr
#' 

source("R/get_logger_data_utils.R")

get_logger_data <- function(path_to_data,
                            data_processing = c("v1.0", "raw", "v0.1", "v0.2", "v0.3"),
                            metric          = NULL,
                            select_station  = "all",
                            temporal_scale  = "none",
                            return_plots    = TRUE) {
  
  temporal_scale <- normalise_temporal_scale(temporal_scale)
  dp_info        <- resolve_data_processing_pattern(data_processing)
  data_processing <- dp_info$data_processing
  pattern         <- dp_info$pattern
  
  # ---- Handle metric for temporal_scale = "none" -------------------------------
  if (temporal_scale == "none") {
    if (!missing(metric) && !is.null(metric)) {
      message(
        "Note: `metric` was provided (\"", metric, "\"), ",
        "but is ignored when `temporal_scale = 'none'`."
      )
    }
  }
  
  # find files recursively
  file_paths <- list.files(
    path       = path_to_data,
    pattern    = pattern,
    full.names = TRUE,
    recursive  = TRUE
  )
  
  if (length(file_paths) == 0L) {
    stop(
      "No files found in `path_to_data` (recursively) matching pattern '", pattern, "'. ",
      "Check the folder path, data_processing choice, and file naming.",
      call. = FALSE
    )
  }
  
  # read and combine
  dat <- purrr::map_df(file_paths, read_logger_file)
  
  # optional station filter
  if (!identical(select_station, "all")) {
    dat <- dat %>% dplyr::filter(site_station_code %in% select_station)
  }
  
  # if no summarisation requested, return combined wide data
  if (temporal_scale == "none") {
    return(dat)
  }
  
  # summarisation: only allowed for v1.0/clean
  if (!identical(data_processing, "v1.0")) {
    stop(
      "Summarisation (hourly/daily/weekly/monthly/yearly) is only allowed for ",
      "clean data (v1.0). Use `data_processing = \"clean\"` or set ",
      "`temporal_scale = \"none\"` for raw/intermediate data.",
      call. = FALSE
    )
  }
  
  metric_norm <- normalise_metric(metric)
  
  dat_sum <- summarise_clean_data(dat, metric_norm, temporal_scale)
  
  if (!isTRUE(return_plots)) {
    return(dat_sum)
  }
  
  plots <- build_plots(dat_sum, metric_norm, temporal_scale)
  
  message("Returning list: $data, $plot1, $plot2, $plot3 (some plots may be NULL).")
  
  list(
    data  = dat_sum,
    plot1 = plots$plot1,
    plot2 = plots$plot2,
    plot3 = plots$plot3
  )
}
