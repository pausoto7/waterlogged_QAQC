
#' Resolve metric argument for barometric assignment
#'
#' Normalizes and validates the \code{metric} argument used in
#' \code{add_nearest_baro()}, ensuring it is one of \code{"DO"},
#' \code{"water level"}, or \code{"both"}. Returns a list describing the
#' normalized input and the set of metric keys that require barometric data.
#'
#' @param metrics A single character string specifying which metrics require
#'   barometric data (e.g., \code{"DO"}, \code{"water level"}, or
#'   \code{"both"}).
#'
#' @return A list with components:
#'   \itemize{
#'     \item \code{metrics_raw}: normalized version of \code{metrics}
#'       (lowercase, no spaces).
#'     \item \code{metrics_need_baro}: character vector of metric keys that
#'       should receive barometric data.
#'   }
#'
#' @keywords internal
resolve_metrics_param <- function(metrics) {
  # length check
  if (length(metrics) != 1L) {
    stop("`metrics` must be a single value ('DO', 'water level', 'conductivity', 'both', or 'all'), ",
         "not a vector of length ", length(metrics), ".",
         call. = FALSE)
  }
  
  metrics_raw <- normalize_string(metrics)
  
  valid_inputs <- c("do", "waterlevel", "conductivity", "both", "all")
  if (!metrics_raw %in% valid_inputs) {
    stop("`metrics` must be one of: 'DO', 'water level', 'conductivity', 'both', or 'all'. You supplied: ",
         metrics, call. = FALSE)
  }
  
  wl_keys   <- c("waterlevel")
  do_keys   <- c("do", "dissolvedoxygen")
  cond_keys <- c("conductivity", "cond")
  
  metrics_need_baro <- switch(
    metrics_raw,
    "waterlevel"    = wl_keys,
    "do"            = do_keys,
    "conductivity"  = cond_keys,
    "both"          = c(wl_keys, do_keys),
    "all"           = c(wl_keys, do_keys, cond_keys)
  )
  
  list(
    metrics_raw       = metrics_raw,
    metrics_need_baro = metrics_need_baro
  )
}



#' Calibrate and stitch multiple barometric series
#'
#' Adjusts one or more barometric pressure time series to the scale of a
#' reference series using simple linear regression, and returns adjusted
#' columns along with regression diagnostics. Typically used internally by
#' \code{add_nearest_baro()} to place multiple barometric stations onto a
#' common reference scale.
#'
#' @param wide_press A wide data frame containing a \code{timestamp} column
#'   and one column per barometric station specified in \code{baro_cols}.
#' @param baro_cols Character vector of column names in \code{wide_press}
#'   representing barometric series.
#' @param ref_col Name of the reference barometric column in \code{wide_press}
#'   to which all other series will be aligned.
#' @param min_overlap Integer; minimum number of overlapping points required
#'   between the reference and an alternate series before regression-based
#'   adjustment is considered reliable. A warning is issued if overlap is
#'   fewer than this.
#'
#' @return A list with components:
#'   \itemize{
#'     \item \code{wide_press}: the input data frame with additional
#'       \code{"<baro_col>_adj"} columns containing adjusted series, including
#'       an adjusted copy of the reference column.
#'     \item \code{reg_summary}: a data frame summarizing the regression used
#'       for each alternate series (columns include \code{alt_col},
#'       \code{ref_col}, \code{n_overlap}, \code{intercept}, \code{slope},
#'       and \code{r2}).
#'   }
#'
#' @keywords internal
calibrate_baro_series <- function(wide_press,
                                  baro_cols = NULL,
                                  ref_col   = NULL,
                                  min_overlap = 20L) {
  
  
  alt_cols <- setdiff(baro_cols, ref_col)
  
  # Container for regression diagnostics
  reg_summary <- vector("list", length(alt_cols))
  names(reg_summary) <- alt_cols
  
  for (alt in alt_cols) {
    # rows where BOTH ref and alt have data
    overlap <- !is.na(wide_press[[ref_col]]) & !is.na(wide_press[[alt]])
    numb_overlap  <- sum(overlap)
    
    if (numb_overlap == 0L) {
      warning("No overlap between ", ref_col, " and ", alt,
              "; ", alt, "_adj is unadjusted copy of ", alt, ".")
      
      wide_press[[paste0(alt, "_adj")]] <- wide_press[[alt]]
      
      reg_summary[[alt]] <- list(
        alt_col   = alt,
        ref_col   = ref_col,
        n_overlap = 0L,
        intercept = NA_real_,
        slope     = NA_real_,
        r2        = NA_real_
      )
      next
    }
    
    if (numb_overlap < min_overlap) {
      warning("Only ", numb_overlap, " overlapping points between ",
              ref_col, " and ", alt,
              "; regression-based adjustment may be unstable.")
    }
    
    df_ovlp <- data.frame(
      ref = wide_press[[ref_col]][overlap],
      alt = wide_press[[alt]][overlap]
    )
    
    fit <- lm(ref ~ alt, data = df_ovlp)
    coefs <- coef(fit)
    intercept <- unname(coefs[1])
    slope     <- unname(coefs[2])
    r2        <- summary(fit)$r.squared
    
    # Adjust alt onto ref scale
    adj_col <- paste0(alt, "_adj")
    wide_press[[adj_col]] <- intercept + slope * wide_press[[alt]]
    
    reg_summary[[alt]] <- list(
      alt_col   = alt,
      ref_col   = ref_col,
      n_overlap = numb_overlap,
      intercept = intercept,
      slope     = slope,
      r2        = r2
    )
  }
  
  # Reference gets its own _adj copy (no change)
  wide_press[[paste0(ref_col, "_adj")]] <- wide_press[[ref_col]]
  
  # Turn reg_summary into a tibble/data.frame
  reg_summary_df <- purrr::map_dfr(reg_summary, ~as.data.frame(.x))
  
  list(
    wide_press  = wide_press,
    reg_summary = reg_summary_df
  )
}
