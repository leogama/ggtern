#' @export
#' @rdname geom_hex_tern
#' @inheritParams ggplot2::stat_bin_2d
#' @export
stat_hex_tern <- function(mapping = NULL, data = NULL,
                         geom = "hex_tern", position = "identity",
                         ...,
                         bins = 30,
                         fun = sum,
                         binwidth = NULL,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatHexTern,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      bins = bins,
      binwidth = binwidth,
      na.rm = na.rm,
      fun = sum,
      ...
    )
  )
}

#' @rdname geom_hex_tern
#' @format NULL
#' @usage NULL
#' @export
StatHexTern <- ggproto("StatHexTern", Stat,
    default_aes = aes(value = 1, fill = ..stat..),
    required_aes = c("x", "y","z"),
    compute_group = function(self, data, scales, binwidth = NULL, bins = 30, na.rm = FALSE, fun = sum) {
      ggint$try_require("hexbin", "stat_binhex_tern")
      
      #Transform to cartesian space
      coord       = coord_tern()
      data        = ggtern::tlr2xy(data,coord,inverse=FALSE,scale=TRUE)
      
      binwidth    = binwidth %||% ggint$hex_binwidth(bins, scales)
      value       = data$value %||% rep(1L, nrow(data))
      
      out         = ggint$hexBinSummarise(data$x, data$y, value, binwidth, fun)
      out$stat    = out$value
      out$value   = NULL
      
      #Transform back to ternary space
      ggtern::tlr2xy(out,coord,inverse=TRUE,scale=TRUE)
    }
)

