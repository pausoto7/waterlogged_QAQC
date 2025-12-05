


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
  path_tcomp_tbl <- system.file("extdata", "tempcomp27888.csv", package = "waterlogged")
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
