find_na_runs <- function(df, value_col = "baro_data", add_step = TRUE) {
  stopifnot("timestamp" %in% names(df))
  vcol <- rlang::sym(value_col)
  
  # ensure ordered
  df <- df %>% arrange(timestamp)
  
  # estimate native step (for regular series)
  dt <- suppressWarnings(median(diff(df$timestamp), na.rm = TRUE))
  add_dt <- if (add_step && is.finite(as.numeric(dt))) dt else as.difftime(0, units = "secs")
  
  # logical NA vector
  is_na <- is.na(df[[value_col]])
  
  if (!any(is_na)) {
    return(tibble(
      gap_id = integer(), gap_start = as.POSIXct(character()), gap_end = as.POSIXct(character()),
      n_missing = integer(), duration = difftime(character(), character())
    ))
  }
  
  # start = NA now & (previous non-NA or row 1)
  starts <- which(is_na & !dplyr::lag(is_na, default = FALSE))
  # end   = NA now & (next non-NA or last row)
  ends   <- which(is_na & !dplyr::lead(is_na, default = FALSE))
  
  tibble(
    gap_id    = seq_along(starts),
    gap_start = df$timestamp[starts],
    gap_end   = df$timestamp[ends],
    n_missing = ends - starts + 1
  ) %>%
    mutate(duration = (gap_end - gap_start) + add_dt)
}


calibrate_baro_series <- function(wide_press,
                                  baro_cols = NULL,
                                  ref_col   = NULL,
                                  min_overlap = 20L) {
  
  
  #alt_cols <- setdiff(baro_cols, ref_col)
  
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