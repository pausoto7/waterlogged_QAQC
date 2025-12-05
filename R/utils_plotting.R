#' Colour palette for QA/QC time series plots
#'
#' Centralised colour definitions used by \code{plot_qaqc_timeseries()}.
#'
#' @return A named list of hex colour codes.
#' @keywords internal
#' @noRd
qaqc_plot_cols <- function() {
  list(
    # DO
    do_raw       = "#264653",
    do_adj       = "#2a9d8f",
    do_temp_raw  = "#619b8a",
    do_temp_adj  = "#619b8a",
    do_air_temp  = "#fe7f2d",
    
    # Conductivity
    cond_raw     = "#00008B",
    cond_adj     = "#00008B",
    cond_temp    = "#619b8a",
    cond_temp_adj= "#619b8a",
    cond_air_temp= "#fe7f2d",
    
    # Water level
    wl_raw       = "#233d4d",
    wl_adj       = "#233d4d",
    wl_temp      = "#619b8a",
    wl_temp_adj  = "#619b8a",
    wl_air_temp  = "#fe7f2d",
    
    # Barometric
    baro_air_raw = "orange",
    baro_air_adj = "gold",
    baro_p_raw   = "black",
    baro_p_adj   = "darkgreen"
  )
}

#' Default shapes/colours for QA/QC flags
#'
#' Given the set of flag levels, returns consistent shapes and colours.
#'
#' @param levels_vec Character vector of flag names (factor levels).
#'
#' @return A list with elements \code{shape} and \code{colour},
#'   each a named vector keyed by \code{levels_vec}.
#' @keywords internal
#' @noRd
qaqc_flag_style <- function(levels_vec) {
  base_shapes <- c(16, 17, 15, 3, 4, 1, 2)
  base_cols   <- c("navy", "black", "burlywood", "darkslategrey",
                   "firebrick", "dodgerblue4", "darkgreen")
  
  n <- length(levels_vec)
  shape_map <- base_shapes[seq_len(n)]
  names(shape_map) <- levels_vec
  
  colour_map <- base_cols[seq_len(n)]
  names(colour_map) <- levels_vec
  
  list(shape = shape_map, colour = colour_map)
}
