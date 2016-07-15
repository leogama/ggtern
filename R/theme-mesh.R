#' Create Grid Mesh
#' 
#' Convenience function for creation of a grid mesh of 'n' major breaks.
#' @param number the number of major breaks.
#' @export
theme_mesh = function(number = 5){
  if(!is.numeric(number)) 
    stop("'number' must be numeric")
  limits = c(0,1)
  number = max(as.integer(number[1]),1)
  limit_tern(breaks       = getBreaks(limits = limits, isMajor = T, nMajor = number), 
             minor_breaks = getBreaks(limits = limits, isMajor = F, nMajor = number))
}