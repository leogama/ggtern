#' @rdname geom_smooth_tern
#' @export
stat_smooth_tern <- function(mapping = NULL, data = NULL,
                        position = "identity", method = "auto",formula = y ~ x,
                        se = TRUE, n = 80, span = 0.75, fullrange = FALSE,
                        level = 0.95, method.args = list(),
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data        = data,
    mapping     = mapping,
    stat        = StatSmoothTern,
    geom        = 'smoothTern',
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      method      = method,
      formula     = formula,
      se          = se,
      n           = n,
      fullrange   = fullrange,
      level       = level,
      na.rm       = na.rm,
      method.args = method.args,
      span        = span,
      ...
    )
  )
}

#' @rdname geom_smooth_tern
#' @format NULL
#' @usage NULL
#' @export
StatSmoothTern <- ggproto("StatSmoothTern", Stat,
  setup_params = function(data, params) {
    params = StatSmooth$setup_params(data,params)
    params$method = lm
    params
  },
  compute_group = function(self,data, scales, method = "auto", formula=y~x,
                           se = TRUE, n = 80, span = 0.75, fullrange = FALSE,
                           xseq = NULL, level = 0.95, method.args = list(),
                           na.rm = FALSE){
    
    #if (!base %in% c('identity','ilr')) stop('base must be either identity or ilr',call.=FALSE)
    if (is.character(method))  method  <- match.fun(method)
    if (is.character(formula)) formula <- as.formula(formula)
    
    #Variables
    coord       = coord_tern()
    raes        = self$required_aes
    
    #Check that the data is valid
    data[,raes] = data.frame(acomp(data[,raes]))
    data        = remove_missing(data,vars=raes,na.rm=TRUE,name=class(self)[1],finite=TRUE)
    if(empty(data))return(zeroGrob())
    
    data = tlr2xy(data,coord,inverse=FALSE,scale=TRUE)
    data = StatSmooth$compute_group(data,scales,method,formula,se,n,span,fullrange,xseq,level,method.args,na.rm)
    if(se){
      data$xmin = data$x; 
      data$xmax = data$x;
    }
    data = tlr2xy(data,coord,inverse=TRUE,scale=TRUE)
    return(data)
  },
  required_aes = c("x", "y","z")
)