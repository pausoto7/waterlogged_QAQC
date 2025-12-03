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
