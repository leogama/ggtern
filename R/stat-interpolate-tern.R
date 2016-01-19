#' @name geom_interpolate_tern
#' @rdname geom_interpolate_tern
#' @export
stat_interpolate_tern <- function(mapping = NULL, data = NULL, geom = "InterpolateTern",
                         position = "identity", method='auto', na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE,n=80, formula=value~poly(x,y,degree=1), ...) {
  layer(
    data        = data,
    mapping     = mapping,
    stat        = StatInterpolateTern,
    geom        = geom,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      na.rm     = na.rm,
      formula   = formula,
      method    = method,
      n         = n,
      ...
    )
  )
}

#' @name geom_interpolate_tern
#' @rdname geom_interpolate_tern
#' @export
StatInterpolateTern <- ggproto("StatInterpolateTern", 
  Stat,
  retransform   = FALSE,
  required_aes  = c("x", "y", "z", "value"),
  default_aes   = aes(order = ..level..),
  setup_params  = function(data, params) {
    if (identical(params$method, "auto")) {
      max_group <- max(table(data$group))
      if (max_group < 1000) {
        params$method <- "loess"
      } else {
        params$method <- "glm" ##NH
        #params$formula <- y ~ s(x, bs = "cs") ##NH
      }
    }
    #if (identical(params$method, "gam")) { params$method <- mgcv::gam } ##NH
    params
  },
  compute_group = function(self, data, scales, method='auto', bins = NULL, binwidth = NULL, breaks = NULL, 
                           complete  = FALSE, na.rm = FALSE, formula=value~poly(x,y,degree=1), 
                           fullrange = FALSE, n = 80, expand=0.5, method.args=list()) {
    
    #Check required aesthetics
    ggint$check_required_aesthetics(self$required_aes, names(data), ggint$snake_class(self))
    
    #Ensure it is a composition
    data[,self$required_aes[1:3]] = as.data.frame(acomp(data[,self$required_aes[1:3]]))
    
    #Build the ternary grid
    theGrid = .getGrid(data,n,fullrange,expand=0.0)
    
    #Transform the data into the orthonormal space
    data[,self$required_aes[1:2]] = as.data.frame(ilr(data[,self$required_aes[1:3]]))
    
    #Build the model
    base.args = list(quote(formula), data = quote(data))
    model     = do.call(method, c(base.args, method.args))
    
    #New Data to Predict
    xrng      = expand_range(range(theGrid$x),expand)
    yrng      = expand_range(range(theGrid$y),expand)
    
    #Predict the data
    data = predictdf2d(model,
                       xseq = seq(xrng[1],xrng[2],length.out=n),
                       yseq = seq(yrng[1],yrng[2],length.out=n))
    
    #Draw the contours
    result    = StatContour$compute_group(data,scales,bins=bins,binwidth=binwidth,breaks=breaks,complete=complete,na.rm=na.rm)
    
    #Do the prediction
    result[,self$required_aes[1:3]] = as.data.frame(ilrInv(result[,self$required_aes[1:2]]))
    
    #Done
    result
  }
)

.getGrid <- function(data,n,fullrange,expand){

  #Determine the limits
  seqmax = .getSeq(c(0,1),n,expand); 
  seqx = ifthenelse(fullrange,seqmax,.getSeq(range(data$x),n,expand)) 
  seqy = ifthenelse(fullrange,seqmax,.getSeq(range(data$y),n,expand))
  
  #Build the grid
  theGrid   = expand.grid(x=seqx,y=seqy)
  theGrid$z = 1 - theGrid$x - theGrid$y
  
  invalid   = apply(theGrid,1,function(x) max(x) > 1 | min(x) < 0)
  if(length(invalid) > 0) theGrid = theGrid[-which(invalid),]
  
  #Convert to ilr coordinates
  theGrid = remove_missing(as.data.frame(acomp(theGrid)),na.rm=TRUE)
  theGrid = data.frame(ilr(theGrid))
  colnames(theGrid) = c('x','y')
  
  #Done
  theGrid
}

.getSeq = function(x,n,expand){ 
  x = expand_range(x,expand)
  seq(x[1],x[2],length.out = n) 
}



