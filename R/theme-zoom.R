#' Zoom on Plot Region
#' 
#' A series of convenience functions for the zooming in on the middle or apex regions to various degrees.
#' In these convenience functions, a single value of \code{x} is expected, which defines the values of the apex
#' limits other than the point of reference, for example, \code{theme_zoom_T} will fix the \code{T} limit 
#' at \code{1}, and will adjust the balancing limits according to the argument x. Equivalent are also possible for
#' the \code{L} and \code{R} apexes, via the \code{theme_zoom_L} and \code{theme_zoom_R} functions respectively. 
#' Finally, the \code{theme_zoom_center} function will adjust all three apex limits, serving, as the name suggests, 
#' to act as a centred zoom. The examples below are fairly self explanatory. 
#' @param x numeric scalar
#' @examples
#' #Default Plot
#' data(Feldspar)
#' base = ggtern(Feldspar,aes(Ab,An,Or)) +
#'        geom_point() + 
#'        geom_density_tern()
#' base
#' 
#' #Zoom on Left Region
#' base + theme_zoom_L(0.5)
#' 
#' #Zoom on Right Region
#' base + theme_zoom_R(0.5)
#' 
#' #Zoom on Top Region
#' base + theme_zoom_T(0.5)
#' 
#' #Zoom on Center Region
#' base + theme_zoom_center(0.5)
#' 
#' @author Nicholas Hamilton
#' @rdname theme_zoom_X
#' @name theme_zoom_X
NULL

#' @rdname theme_zoom_X
#' @export
theme_zoom_T = function(x = 1.0,...){
  args = list(...); args$L = args$R = x; args$T = 1
  do.call(limit_tern,args=args)
}

#' @rdname theme_zoom_X
#' @export
theme_zoom_L = function(x = 1.0,...){
  args = list(...); args$T = args$R = x; args$L = 1
  do.call(limit_tern,args=args)
}

#' @rdname theme_zoom_X
#' @export
theme_zoom_R = function(x = 1.0,...){
  args = list(...); args$T = args$L = x; args$R = 1
  do.call(limit_tern,args=args)
}

#' @rdname theme_zoom_X
#' @export
theme_zoom_center = function(x=1.0,...){
  args = list(...); args$T = args$L = args$R = x
  do.call(limit_tern,args=args)
}

