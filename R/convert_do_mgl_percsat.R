#' Dissolved oxygen conversion
#'
#' Convert dissolved oxygen from mg/L to percent air saturation.
#'
#' Assumes input has already been QA/QC'd and standardized to metric-based
#' column names:
#'   - do_mgl        (dissolved oxygen, mg/L)
#'   - watertemp_C   (water temperature, °C)
#'   - airpress_kPa  (barometric pressure, kPa)
#'
#' @param do_data Data frame with DO, temperature, pressure, timestamp,
#'   and site_station_code.
#' @param output_dir Optional directory where per-site, per-year CSVs
#'   should be written. If NULL (default), no files are written.
#' @param version_label Character tag for output files (e.g., "v0.3").
#'
#' @return A data frame with a new column `do_percsat` (percent air saturation).
#' @export
#' @import dplyr
#' @importFrom lubridate year date
convert_do_mgl_percsat <- function(do_data,
                                   output_dir = NULL,
                                   version_label = "v0.3") {
  
  # safety check
  req <- c("do_mgl", "watertemp_C", "airpress_kPa")
  missing <- setdiff(req, names(do_data))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse=", "))
  }
  
  # create new column directly in do_data
  do_data$do_percsat <- NA_real_
  
  # valid rows
  idx <- !is.na(do_data$do_mgl) &
    do_data$do_mgl != -888.88 &
    !is.na(do_data$watertemp_C) &
    !is.na(do_data$airpress_kPa)
  
  if (any(idx)) {
    temp_K    <- 273.15 + do_data$watertemp_C
    press_atm <- do_data$airpress_kPa / 101.325
    
    do_1atm <- exp(
      -139.34411 +
        (1.575701e5  / temp_K) -
        (6.642308e7  / temp_K^2) +
        (1.243800e10 / temp_K^3) -
        (8.621949e11 / temp_K^4)
    )
    
    u_atm <- exp(11.8571 - (3840.70 / temp_K) - (216961 / temp_K^2))
    
    theta_o <- 0.000975 -
      1.426e-5 * do_data$watertemp_C +
      6.436e-8 * do_data$watertemp_C^2
    
    F_p <- ((press_atm - u_atm) * (1 - theta_o * press_atm)) /
      ((1 - u_atm) * (1 - theta_o))
    
    do_sat_real <- do_1atm * F_p
    
    raw_perc <- 100 * (do_data$do_mgl / do_sat_real)
    
    do_data$do_percsat[idx] <- signif(raw_perc[idx], 4)
  }
  
  # writing logic stays the same
  if (!is.null(output_dir)) {
    # ... (unchanged)
  }
  
  return(do_data)
}
