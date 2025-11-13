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
