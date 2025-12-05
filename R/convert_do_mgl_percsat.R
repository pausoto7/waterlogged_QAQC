#' Convert dissolved oxygen (mg/L) to percent air saturation
#'
#' Computes percent air saturation from dissolved oxygen concentrations
#' (\code{do_mgl}), water temperature (\code{watertemp_C}), and barometric
#' pressure (\code{airpress_kPa}), following standard Weiss (1970/1974)
#' oxygen solubility equations.
#'
#' The function also writes per-site, per-year CSV files using the naming:
#'
#' \preformatted{
#'   <site>_DO_<startdate>_<enddate>_<version>.csv
#' }
#'
#' and stores them in:
#'
#' \preformatted{
#'   <output_dir>/<year>/processed/
#' }
#'
#' Input data must already be QA/QC'd and standardized to metric-based
#' column names:
#' \itemize{
#'   \item \code{timestamp}        (POSIXct, UTC)
#'   \item \code{site_station_code}
#'   \item \code{do_mgl}           dissolved oxygen (mg/L)
#'   \item \code{watertemp_C}      water temperature (°C)
#'   \item \code{airpress_kPa}     barometric pressure (kPa)
#' }
#'
#' Values of \code{-888.88} in \code{do_mgl} are treated as missing.
#'
#' @param do_data A data frame containing DO, temperature, barometric
#'   pressure, timestamp, and site identifiers. Must contain the columns:
#'   \code{timestamp}, \code{site_station_code}, \code{do_mgl},
#'   \code{watertemp_C}, and \code{airpress_kPa}.
#' @param output_dir Character scalar. Root folder where per-year,
#'   per-site output files will be written. Must already exist.
#' @param version_label Character version string appended to output filenames
#'   (e.g., \code{"v0.3"}).
#'
#' @return A data frame identical to \code{do_data} but with an additional
#'   numeric column \code{do_percsat} (percent air saturation).
#'   Files are written to disk as side effects.
#'
#' @details
#' The calculation uses Weiss (1970, 1974) oxygen solubility equations.
#' Barometric pressure is internally converted from kPa to atmospheres.
#'
#' @references
#' Weiss, R. F. (1970). *The solubility of nitrogen, oxygen and argon in water
#'   and seawater.* Deep-Sea Research.
#'
#' Weiss, R. F. (1974). *Oxygen solubility in water and seawater based on
#'   the virial equation.* Deep-Sea Research.
#'
#' @export
#'
#' @import dplyr
#' @importFrom lubridate year date ymd_hms


convert_do_mgl_percsat <- function(do_data,
                                   output_dir,
                                   version_label = "v0.3") {
  
  # ---- output_dir required + validation ----
  if (missing(output_dir) || is.null(output_dir)) {
    stop("`output_dir` must be supplied — writing CSVs is mandatory.")
  }
  
  if (!is.character(output_dir) || length(output_dir) != 1L) {
    stop("`output_dir` must be a single character string path.")
  }
  
  if (!dir.exists(output_dir)) {
    stop("`output_dir` does not exist: ", output_dir)
  }
  
  # ---- required columns ------------------------------------------------------
  req <- c("timestamp", "site_station_code", "do_mgl", "watertemp_C", "airpress_kPa")
  missing <- setdiff(req, names(do_data))
  
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }
  
  # Ensure timestamp is POSIXct (UTC)
  if (!inherits(do_data$timestamp, "POSIXct")) {
    do_data$timestamp <- lubridate::ymd_hms(do_data$timestamp, tz = "UTC")
  }
  
  # ---- main calculation ------------------------------------------------------
  do_data_reviewed <- do_data %>%
    dplyr::mutate(
      # valid rows
      valid = !is.na(do_mgl) &
        do_mgl != -888.88 &
        !is.na(watertemp_C) &
        !is.na(airpress_kPa),
      
      temp_K    = 273.15 + watertemp_C,
      press_atm = airpress_kPa / 101.325,
      
      do_1atm = exp(
        -139.34411 +
          (1.575701e5  / temp_K) -
          (6.642308e7  / temp_K^2) +
          (1.243800e10 / temp_K^3) -
          (8.621949e11 / temp_K^4)
      ),
      
      u_atm = exp(11.8571 - (3840.70 / temp_K) - (216961 / temp_K^2)),
      
      theta_o = 0.000975 -
        1.426e-5 * watertemp_C +
        6.436e-8 * watertemp_C^2,
      
      F_p = ((press_atm - u_atm) * (1 - theta_o * press_atm)) /
        ((1 - u_atm) * (1 - theta_o)),
      
      do_sat_real = do_1atm * F_p,
      
      do_percsat = dplyr::case_when(
        valid & !is.na(do_sat_real) & do_sat_real > 0 ~
          signif(100 * (do_mgl / do_sat_real), 4),
        TRUE ~ NA_real_
      )
    )
  
  # strip intermediate columns for return
  do_data_reviewed <- do_data_reviewed %>%
    dplyr::select(
      -valid, -temp_K, -press_atm, -do_1atm, -u_atm,
      -theta_o, -F_p, -do_sat_real
    )
  
  # ---- write outputs: per year × site ---------------------------------------
  df_write <- do_data_reviewed %>%
    dplyr::arrange(site_station_code, timestamp) %>%
    dplyr::mutate(
      year = lubridate::year(timestamp),
      date = lubridate::date(timestamp)
    )
  
  # unique year-site combinations
  key_df <- df_write %>%
    dplyr::distinct(year, site_station_code) %>%
    dplyr::filter(!is.na(year), !is.na(site_station_code))
  
  if (nrow(key_df) == 0) {
    warning("No valid year/site_station_code combinations found — no files written.")
    return(do_data_reviewed)
  }
  
  for (i in seq_len(nrow(key_df))) {
    year_j <- key_df$year[i]
    site_k <- key_df$site_station_code[i]
    
    dat <- df_write %>%
      dplyr::filter(year == year_j, site_station_code == site_k)
    
    if (!nrow(dat)) next
    
    start_k <- gsub("\\D", "", as.character(min(dat$date)))
    end_k   <- gsub("\\D", "", as.character(max(dat$date)))
    
    year_dir <- file.path(output_dir, year_j)
    proc_dir <- file.path(year_dir, "processed")
    
    if (!dir.exists(year_dir)) dir.create(year_dir, recursive = TRUE)
    if (!dir.exists(proc_dir)) dir.create(proc_dir, recursive = TRUE)
    
    dat_to_write <- dat %>%
      dplyr::select(-year, -date) %>%
      dplyr::mutate(timestamp = as.character(timestamp))
    
    out_name <- sprintf(
      "%s_DO_%s_%s_%s.csv",
      site_k, start_k, end_k, version_label
    )
    out_path <- file.path(proc_dir, out_name)
    
    utils::write.csv(dat_to_write, out_path, row.names = FALSE)
    
    message(sprintf(
      "DO conversion (%s) written for %s -> %s/processed",
      version_label, site_k, year_j
    ))
  }
  
  do_data_reviewed
}
