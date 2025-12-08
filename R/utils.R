
#' Normalize a character vector for matching
#'
#' Converts strings to lower case, trims leading/trailing whitespace, and
#' removes internal spaces. Useful for normalizing user inputs or metadata
#' fields before comparison.
#'
#' @param x A character vector.
#'
#' @return A character vector of the same length as \code{x}, normalized for
#'   case and spacing.
#'
#' @keywords internal
normalize_string <- function(x) {
  x <- tolower(x)
  x <- trimws(x)
  x <- gsub("\\s+", "", x)   # remove internal spaces
  x
}


#' Convert Fahrenheit to Celsius
#'
#' Converts a numeric vector of temperatures from degrees Fahrenheit (\u00B0F)
#' to degrees Celsius (\u00B0C) using the standard linear transformation.
#'
#' This is an internal helper used when parsing HOBO logger files that
#' report temperature in \u00B0F.
#'
#' @param x Numeric vector of temperatures in degrees Fahrenheit.
#'
#' @return A numeric vector of temperatures converted to degrees Celsius.
#'
#' @examples
#' convert_F_to_C(32)   # 0
#' convert_F_to_C(212)  # 100
#'
#' @keywords internal
convert_F_to_C <- function(x) (x - 32) * (5/9)


#' Escape regex special characters
#'
#' @param x A character string to escape.
#'
#' @return A character string with regex special characters escaped.
#'
#' @keywords internal
escape_regex <- function(x) {
  gsub("([][{}()+*^$|\\.?\\\\-])", "\\\\\\1", x)
}


#' Find contiguous runs of missing values in a time series
#'
#' Identifies contiguous runs of \code{NA} values in a time series column and
#' returns their start/end timestamps, length (in points), and duration.
#'
#' The input data frame must contain a \code{timestamp} column that can be
#' ordered, and a numeric column specified by \code{value_col}.
#'
#' @param df A data frame containing at least a \code{timestamp} column and the
#'   value column to be checked for gaps.
#' @param value_col Name of the column (as a single character string) in
#'   \code{df} to inspect for \code{NA} runs. Defaults to
#'   \code{"airpress_kPa"}.
#' @param add_step Logical; if \code{TRUE}, the estimated native time step
#'   (median difference of \code{timestamp}) is added to the gap duration so
#'   that the reported duration corresponds to the full span covered by the
#'   missing values.
#'
#' @return A tibble with one row per gap, containing:
#'   \itemize{
#'     \item \code{gap_id}: integer identifier for the gap.
#'     \item \code{gap_start}: timestamp of the first missing value.
#'     \item \code{gap_end}: timestamp of the last missing value.
#'     \item \code{n_missing}: number of consecutive missing values.
#'     \item \code{duration}: time difference between \code{gap_start} and
#'       \code{gap_end}, optionally plus one time step.
#'   }
#'   If no missing values are found, an empty tibble with the same columns is
#'   returned.
#'
#' @keywords internal
find_na_runs <- function(df, value_col = "airpress_kPa", add_step = TRUE) {
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
