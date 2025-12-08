


#' Apply temperature compensation to conductivity measurements
#'
#' Adjusts conductivity values to a standard reference temperature (typically 25\u00B0C)
#' using temperature compensation coefficients based on ISO 7888. This corrects
#' for the effect of temperature on conductivity measurements.
#'
#' @param input_data Data frame containing conductivity data with columns
#'   `conduct_uScm_adj` (adjusted conductivity in µS/cm) and `watertemp_C_adj`
#'   (adjusted water temperature in \u00B0C).
#'
#' @return The input data frame with an additional column `spc_uScm_adj`
#'   containing specific conductance (temperature-compensated conductivity
#'   values) normalized to 25\u00B0C.
#'
#' @details
#' The function uses a 5th-order polynomial fitted to ISO 7888 temperature
#' compensation coefficients. The compensation is applied row-wise based on
#' the water temperature at each measurement.
#'
#' Temperature compensation formula:
#' \deqn{C_{25} = C_T / f(T)}
#'
#' where \eqn{C_T} is conductivity at temperature T, \eqn{C_{25}} is conductivity
#' at 25\u00B0C, and \eqn{f(T)} is the temperature compensation coefficient.
#'
#' @seealso [conductivity_qaqc()], [conductivity_qaqc_all()]
#'
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr starts_with mutate select arrange
#' @export
conductivity_temp_compensation <- function(input_data) {
  
  # ---- Required columns ------------------------------------------------------
  req_cols <- c("conduct_uScm_adj", "watertemp_C_adj")
  missing_cols <- setdiff(req_cols, names(input_data))
  
  if (length(missing_cols) > 0) {
    stop(
      "conduct_temp_comp(): Input missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  # ---- Temperature compensation coefficients (ISO 7888 nonlinear) -----------
  path_tcomp_tbl <- system.file("extdata", "tempcomp27888.csv", package = "waterloggerQAQC")

  if (path_tcomp_tbl == "" || !file.exists(path_tcomp_tbl)) {
    stop("Temperature compensation table 'tempcomp27888.csv' not found in package extdata. ",
         "This file is required for conductivity temperature compensation.")
  }

  tcomp <- read.csv(path_tcomp_tbl)
  
  # tidy table: expand temp range offset columns X0..X0.9
  tlong <- tidyr::pivot_longer(
    tcomp,
    cols = dplyr::starts_with("X"),
    names_to = "Tsub",
    values_to = "coef"
  )
  
  tlong$Tsub <- as.numeric(gsub("X", "", tlong$Tsub))
  tlong <- tlong |>
    dplyr::mutate(temp = Temp_C + Tsub) |>
    dplyr::select(temp, coef) |>
    dplyr::arrange(temp)
  
  # fit 5th-order polynomial (same as original)
  model5 <- lm(coef ~ poly(temp, 5, raw = TRUE), data = tlong)
  coefs <- coef(model5)
  
  tempcomp <- function(T){
    coefs[1] +
      coefs[2] * T +
      coefs[3] * T^2 +
      coefs[4] * T^3 +
      coefs[5] * T^4 +
      coefs[6] * T^5
  }
  
  # ---- Compute SPC -----------------------------------------------------------
  out <- input_data
  
  out$spc_uScm_adj <- with(out, {
    ifelse(
      is.na(watertemp_C_adj) | is.na(conduct_uScm_adj),
      NA_real_,
      tempcomp(watertemp_C_adj) * conduct_uScm_adj
    )
  })
  
  # Warn if many temps are missing
  pct_missing_temp <- mean(is.na(out$watertemp_C_adj)) * 100
  if (pct_missing_temp > 5) {
    warning(
      sprintf(
        "conduct_temp_comp(): %.1f%% of watertemp_C_adj values are NA; ",
        pct_missing_temp
      ),
      "specific conductance could not be computed for those timestamps."
    )
  }
  
  return(out)
}
