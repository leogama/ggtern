#' Create Grid Mesh
#' 
#' Convenience function for creation of a grid mesh of 'n' major breaks.
#' @param n the number of major breaks.
#' @export
theme_mesh = function(n = 5){
  if(!is.numeric(n)) 
    stop("'n' must be numeric")
  l = c(0,1)
  n = max(as.integer(n[1]),1)
  limit_tern(breaks       = getBreaks(limits = l, isMajor = T, n = n), 
             minor_breaks = getBreaks(limits = l, isMajor = F, n = n))
}