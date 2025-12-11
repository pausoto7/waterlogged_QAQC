
guess_role_from_cols <- function(df) {
  if (is.null(df)) return("none")
  nms <- names(df)
  has <- function(...) all(c(...) %in% nms)
  
  # DO (either mg/L or %sat)
  if (has("do_mgl", "do_mgl_adj") || has("do_percsat", "do_percsat_adj")) return("do")
  # Conductivity
  if (has("conduct_uScm", "conduct_uScm_adj")) return("cond")
  # Water level
  if (has("waterlevel_m", "waterlevel_m_adj")) return("wl")
  # Baro (pressure + temps)
  if (has("airpress_kPa", "airpress_kPa_adj") && (("airtemp_C" %in% nms) || ("airtemp_C_adj" %in% nms))) return("baro")
  
  "unknown"
}

format_found <- function(role, arg) sprintf("%s: %s", arg, role)


#' Normalise QA/QC input data frame
#'
#' Filters to a single station, ensures POSIXct timestamps,
#' and orders by time.
#'
#' @param df Data frame or \code{NULL}.
#' @param station Character station code.
#'
#' @return A filtered data frame or \code{NULL} if empty.
#' @keywords internal
#' @noRd
normalize_qaqc_df <- function(df, station) {
  if (is.null(df)) return(NULL)
  
  req_cols <- c("site_station_code", "timestamp")
  missing  <- setdiff(req_cols, names(df))
  if (length(missing) > 0) {
    stop(
      "plot_qaqc_timeseries(): input data is missing required column(s): ",
      paste(missing, collapse = ", ")
    )
  }
  
  if (!inherits(df$timestamp, "POSIXt")) {
    df <- df %>%
      dplyr::mutate(timestamp = lubridate::ymd_hms(.data$timestamp))
  }
  
  df <- df %>%
    dplyr::filter(.data$site_station_code == station) %>%
    dplyr::arrange(.data$timestamp)
  
  if (nrow(df) == 0) return(NULL)
  df
}

#' Build QA/QC code points from *_qaqc_code column
#'
#' @param df Data frame.
#' @param code_col Character, name of code column.
#'
#' @return Data frame with \code{timestamp} and factor \code{code},
#'   or \code{NULL} if no codes.
#' @keywords internal
#' @noRd
build_qaqc_code_points <- function(df, code_col) {
  if (is.null(df) || !code_col %in% names(df)) return(NULL)
  
  code_sym <- rlang::sym(code_col)
  
  out <- df %>%
    dplyr::select("timestamp", !!code_sym) %>%
    dplyr::filter(
      !is.na(!!code_sym),
      !!code_sym != ""
    ) %>%
    dplyr::mutate(code = factor(!!code_sym)) %>%
    dplyr::select("timestamp", "code")
  
  if (nrow(out) == 0) return(NULL)
  out
}

#' Build long-format flag points from logical columns
#'
#' @param df Data frame.
#' @param flag_cols Character vector of candidate flag columns.
#'
#' @return Data frame with \code{timestamp} and factor \code{flag_type},
#'   or \code{NULL} if no TRUE values.
#' @keywords internal
#' @noRd
build_flag_long <- function(df, flag_cols) {
  if (is.null(df)) return(NULL)
  flag_cols <- intersect(flag_cols, names(df))
  if (length(flag_cols) == 0) return(NULL)
  
  out <- df %>%
    dplyr::select("timestamp", dplyr::all_of(flag_cols)) %>%
    tidyr::pivot_longer(
      cols      = dplyr::all_of(flag_cols),
      names_to  = "flag_type",
      values_to = "flag_value"
    ) %>%
    dplyr::filter(.data$flag_value) %>%
    dplyr::mutate(flag_type = factor(.data$flag_type)) %>%
    dplyr::select("timestamp", "flag_type")
  
  if (nrow(out) == 0) return(NULL)
  out
}

