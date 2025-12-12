#' Colour palette for QA/QC time series plots
#'
#' Centralised colour definitions used by \code{plot_qaqc_timeseries()}.
#' Uses the Okabe–Ito colour-blind–friendly palette.
#'
#' @return A named list of hex colour codes.
#' @keywords internal
#' @noRd
qaqc_plot_cols <- function() {
  # Okabe–Ito palette (colour-blind friendly)
  col_black    <- "#000000"
  col_orange   <- "#E69F00"
  col_skyblue  <- "#56B4E9"
  col_green    <- "#009E73"
  col_yellow   <- "#F0E442"
  col_blue     <- "#0072B2"
  col_vermil   <- "#D55E00"
  col_purple   <- "#CC79A7"
  
  list(
    # DO (raw vs adj differ mainly by alpha in geoms)
    do_raw       = col_blue,
    do_adj       = col_skyblue,
    do_temp_raw  = col_green,
    do_temp_adj  = col_green,
    do_air_temp  = col_orange,
    
    # Conductivity
    cond_raw      = col_purple,
    cond_adj      = col_purple,
    cond_temp     = col_green,
    cond_temp_adj = col_green,
    cond_air_temp = col_orange,
    
    # Water level
    wl_raw       = col_black,
    wl_adj       = col_blue,
    wl_temp      = col_green,
    wl_temp_adj  = col_green,
    wl_air_temp  = col_orange,
    
    # Barometric
    baro_air_raw = col_orange,
    baro_air_adj = col_yellow,
    baro_p_raw   = col_black,
    baro_p_adj   = col_green
  )
}



#' Shapes & colours for QA/QC flags (distinct colour-blind friendly palette)
#'
#' Uses Paul Tol's "vibrant" palette
#'
#' @param levels_vec Character vector of flag names.
#'
#' @return A list with named vectors: $shape and $colour
#' @keywords internal
#' @noRd
qaqc_flag_style <- function(levels_vec) {
  
  # Paul Tol Vibrant colours (also CB-friendly)
  base_cols <- c(
    "#EE7733", # orange
    "#0077BB", # blue
    "#009988", # teal
    "#CC3311", # red
    "#EE3377", # magenta,
    "#33BBEE", # cyan
    "#BBBBBB"  # grey
  )
  
  # Strongly distinct shapes
  base_shapes <- c(
    16, # filled circle
    17, # filled triangle
    15, # filled square
    3,  # plus
    4,  # ×
    7,  # diamond
    8   # star
  )
  
  n <- length(levels_vec)
  cols   <- rep(base_cols,   length.out = n)
  shapes <- rep(base_shapes, length.out = n)
  
  names(cols)   <- levels_vec
  names(shapes) <- levels_vec
  
  list(shape = shapes, colour = cols)
}






















#' 
#' 
#' 
#' #' Colour palette for QA/QC time series plots
#' #'
#' #' Centralised colour definitions used by \code{plot_qaqc_timeseries()}.
#' #'
#' #' @return A named list of hex colour codes.
#' #' @keywords internal
#' #' @noRd
#' qaqc_plot_cols <- function() {
#'   list(
#'     # DO
#'     do_raw       = "#264653",
#'     do_adj       = "#2a9d8f",
#'     do_temp_raw  = "#619b8a",
#'     do_temp_adj  = "#619b8a",
#'     do_air_temp  = "#fe7f2d",
#'     
#'     # Conductivity
#'     cond_raw     = "#00008B",
#'     cond_adj     = "#00008B",
#'     cond_temp    = "#619b8a",
#'     cond_temp_adj= "#619b8a",
#'     cond_air_temp= "#fe7f2d",
#'     
#'     # Water level
#'     wl_raw       = "#233d4d",
#'     wl_adj       = "#233d4d",
#'     wl_temp      = "#619b8a",
#'     wl_temp_adj  = "#619b8a",
#'     wl_air_temp  = "#fe7f2d",
#'     
#'     # Barometric
#'     baro_air_raw = "orange",
#'     baro_air_adj = "gold",
#'     baro_p_raw   = "black",
#'     baro_p_adj   = "darkgreen"
#'   )
#' }
#' 
#' #' Default shapes/colours for QA/QC flags
#' #'
#' #' Given the set of flag levels, returns consistent shapes and colours.
#' #'
#' #' @param levels_vec Character vector of flag names (factor levels).
#' #'
#' #' @return A list with elements \code{shape} and \code{colour},
#' #'   each a named vector keyed by \code{levels_vec}.
#' #' @keywords internal
#' #' @noRd
#' qaqc_flag_style <- function(levels_vec) {
#'   base_shapes <- c(16, 17, 15, 3, 4, 1, 2)
#'   base_cols   <- c("navy", "black", "burlywood", "darkslategrey",
#'                    "firebrick", "dodgerblue4", "darkgreen")
#'   
#'   n <- length(levels_vec)
#'   shape_map <- base_shapes[seq_len(n)]
#'   names(shape_map) <- levels_vec
#'   
#'   colour_map <- base_cols[seq_len(n)]
#'   names(colour_map) <- levels_vec
#'   
#'   list(shape = shape_map, colour = colour_map)
#' }
