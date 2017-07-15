#' Draw Text at Relative Position on Viewport
#' 
#' Since it is sometimes counter intuitive for working with ternary or other non-cartesian coordinates in the 
#' event that the the user wishes to place a text-geometry based on visual inspection, this geometry 
#' positions such text item at a fraction from x=[0,1] and y=[0,1] of the viewport in x and y cartesian 
#' coordinates.
#' 
#' @examples
#' library(ggtern)
#' set.seed(1)
#' n  = 10
#' df = data.frame(x=runif(n),y=runif(n),label=sprintf("Label: %s",1:n))
#' ggtern() + 
#'   geom_mask() + 
#'   geom_text_viewport(data=df,aes(x,y,label=label,color=1:n))
#' 
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggtern:::rd_aesthetics("geom","Text")}
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::geom_text
#' @seealso \code{\link{ggplot2::geom_text}}
#' @author Nicholas Hamilton
#' @rdname geom_text_viewport
#' @name geom_text_viewport
#' @export
geom_text_viewport = function (mapping       = NULL, 
                              data          = NULL, 
                              stat          = "identity", 
                              position      = "identity", 
                              ...,
                              hjust         = 'inward',
                              vjust         = 'inward',
                              parse         = FALSE, 
                              check_overlap = FALSE, 
                              na.rm         = FALSE, 
                              show.legend   = NA, 
                              inherit.aes   = TRUE){
  layer(
    data            = data,
    mapping         = mapping,
    stat            = ggplot2::StatIdentity,
    geom            = GeomTextViewport,
    position        = ggplot2::PositionIdentity,
    show.legend     = show.legend,
    inherit.aes     = inherit.aes,
    params = list(
      parse         = parse,
      check_overlap = check_overlap,
      na.rm         = na.rm,
      hjust         = hjust,
      vjust         = vjust,
      ...
    )
  )
}

#' @rdname geom_text_viewport
#' @usage NULL
#' @format NULL
#' @export
GeomTextViewport <- ggproto("GeomTextViewport", GeomText,
  setup_data = function(self, data, params){
    data$hjust = data$hjust %||% params$hjust
    data$vjust = data$vjust %||% params$vjust
    data
  },
  default_aes = defaults(
    aes(x='x',y='y'),
    GeomText$default_aes
  ),
  draw_panel = function(self, data, panel_scales, coord, parse = FALSE, na.rm = FALSE, check_overlap = FALSE) {

    #Check the required aesthetics have been provided
    ggint$check_required_aesthetics(self$required_aes, names(data), ggint$snake_class(self))
    
    #Compute hjust and vjust, if provided in text format
    if (is.character(data$vjust))
      data$vjust <- ggint$compute_just(data$vjust, data$y)
    if (is.character(data$hjust))
      data$hjust <- ggint$compute_just(data$hjust, data$x)
    
    #Bind to the viewport Limits
    data$x = with(data,pmin(pmax(x,0.0),1.0))
    data$y = with(data,pmin(pmax(y,0.0),1.0))
    
    #Remove Duplicated Rows
    uniqueOn = unique(c('PANEL','group',names(self$default_aes),self$required_aes))
    data     = data[!duplicated(data[,intersect(names(data),uniqueOn)]), ]
    
    lab <- data$label
    if (parse)
      lab <- parse(text = as.character(lab))
    
    #Produce the textgrob
    grid::textGrob(
      label         = lab,
      x             = data$x, 
      y             = data$y, 
      default.units = "native",
      hjust         = data$hjust, 
      vjust         = data$vjust,
      rot           = data$angle,
      vp            = grid::viewport(),
      gp = grid::gpar(
        col         = alpha(data$colour, data$alpha),
        fontsize    = data$size * .pt,
        fontfamily  = data$family,
        fontface    = data$fontface,
        lineheight  = data$lineheight
      ),
      check.overlap = check_overlap
    )
  }
)
