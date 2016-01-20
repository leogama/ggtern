#' Key drawing functions
#'
#' Each Geom has an associated function that draws the key when the geom needs
#' to be displayed in a legend. These are the options built into ggplot2.
#'
#' @return A grid grob.
#' @param data A single row data frame containing the scaled aesthetics to
#'   display in this key
#' @param params A list of additional parameters supplied to the geom.
#' @param size Width and height of key in mm.
#' @keywords internal
#' @name draw_key_tern
NULL

#' @export
#' @rdname draw_key_tern
draw_key_crosshair_tern <- function(data, params, size) {
  .ratio = function(){ 0.5*tan(60*pi/180) }
  dx = (1 - .ratio())/2
  segmentsGrob(c(0.5,0.5,0.5),
               c(0.5,0.5,0.5),
               c(dx,dx,1.0-dx),
               c(0.0,1.0,0.5),
               gp = gpar(
                 col = alpha(data$colour, data$alpha),
                 lwd = data$size * .pt,
                 lty = data$linetype,
                 lineend = "butt"
               ),
               arrow = params$arrow
  )
}

#' @export
#' @rdname draw_key_tern
draw_key_Xline <- function(data,params,size){
  draw_key_crosshair_tern(data,params,size)
}

