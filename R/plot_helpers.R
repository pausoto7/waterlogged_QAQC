#' Combine multiple ggplot objects into a plotly subplot
#'
#' Internal helper to create a stacked plotly subplot from multiple ggplot
#' objects. Used by \code{plot_qaqc_timeseries()} to combine parameter panels.
#'
#' @param ... One or more ggplot objects to combine.
#' @param heights Numeric vector of relative heights for each subplot. If
#'   \code{NULL} (default), all subplots get equal height.
#' @param shareX Logical; should the x-axis be shared across subplots? Default
#'   is \code{TRUE}.
#' @param margin Numeric; vertical margin between subplots (0-1 scale). Default
#'   is 0.05.
#'
#' @return A plotly object containing the combined subplots.
#'
#' @keywords internal
#' @noRd
make_qaqc_subplot <- function(..., heights = NULL, shareX = TRUE, margin = 0.05) {
  plots <- list(...)
  n     <- length(plots)

  if (n == 0) stop("make_qaqc_subplot(): no plots supplied.", call. = FALSE)
  if (is.null(heights)) heights <- rep(1 / n, n)

  plotly::subplot(
    lapply(plots, function(g) suppressWarnings(plotly::ggplotly(g))),
    nrows   = n,
    heights = heights,
    shareX  = shareX,
    margin  = margin
  )
}
