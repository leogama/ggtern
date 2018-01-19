#' @export
#' @rdname geom_tri_tern
#' @inheritParams ggplot2::stat_bin_2d
#' @param centroid logical to return the centroid of the polygon, rather than the complete polygon
#' @export
stat_tri_tern <- function(mapping = NULL, data = NULL,
                          geom = "tri_tern", position = "identity",
                          ...,
                          bins = 30,
                          fun = sum,
                          centroid = FALSE,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  layer(
    data        = data,
    mapping     = mapping,
    stat        = StatTriTern,
    geom        = geom,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      bins      = bins,
      na.rm     = na.rm,
      fun       = fun,
      centroid  = centroid,
      ...
    )
  )
}

#' @rdname geom_tri_tern
#' @format NULL
#' @usage NULL
#' @export
StatTriTern <- ggproto("StatTriTern", Stat,
    default_aes = aes(value = 1, fill = ..stat..),
    required_aes = c("x", "y","z"),
    setup_data = function(self,data,params){
      ##Ensure it is simplex
      raes        = self$required_aes
      data[,raes] = as.data.frame(acomp(data[,raes]))
      data
    },
    compute_group = function(self, data, scales, bins = 30, na.value = NA, na.rm = FALSE, fun = sum, centroid = FALSE) {
      
      ##For Consistency with ggplo2 hexbin
      value       = rep(1,nrow(data))
      bin         = triBinSummarise(data$x, data$y, data$z, value, bins, sum, na.value = na.value)
      bin$density = as.vector(bin$value / sum(bin$value, na.rm = TRUE))
      
      ##User Defined Stats
      value       = data$value %||% value
      out         = triBinSummarise(data$x, data$y, data$z, value, bins, fun, na.value = na.value)
      
      #Assemble
      out$group   = out$IDPolygon
      out$stat    = out$value
      out$count   = bin$value
      out$density = bin$density
      out$value   = out$IDPolygon = NULL
      
      #Remove NA's
      if(na.rm){
          out         = remove_missing(out,FALSE,
                                       c(self$required_aes, 
                                         c('stat','count','density')),
                                       ggint$snake_class(self),
                                       finite = TRUE)
      }
      
      #If Centroid
      if(centroid){
        out = ddply(out,'group',function(df){
          res = df[1,,drop=FALSE]
          res[,self$required_aes] = colMeans(df[,self$required_aes])
          res
        })
      }
      
      out
    }
)

triMesh <- function(n = 1) {
  n <- as.integer(max(n[1], 1))
  temp <- seq(0, n, 1)
  df <- data.frame(
    x = unlist(sapply((n+1):1, function(i) temp[1:i])),
    y = rep(0:n, (n+1):1)
  )
  df$z <- n - df$x - df$y
  df <- cbind(0:(nrow(df)-1), df / n)
  names(df) <- c('IDPoint', 'x', 'y', 'z')
  return(df)
}

triStat <- function(df, xmin = 0, xmax = 1, ymin = 0, ymax = 1, zmin = 0, zmax = 1,na.value=NA,fun) {
  ret <- df
  ret <- with(ret, ret[xmin <= x & x < xmax,,drop=F])
  ret <- with(ret, ret[ymin <= y & y < ymax,,drop=F])
  ret <- with(ret, ret[zmin <= z & z < zmax,,drop=F])
  
  #If No rows, return NA
  nr  <- nrow(ret)
  if(is.na(nr) || nr == 0) return(na.value[1])
  fun(ret$w)
}

triPoly = function( n = 1 ){
  n     = as.integer(max(n[1],1))
  mesh  = triMesh( n )
  inv   = 1/n
  
  #Upper Triangles
  upper = with(mesh,ddply(mesh,.(IDPoint),function(df){
    x = df$x; y=df$y; p = df$IDPoint[1]
    result   = data.frame(x = c(x,x,x+inv),
                        y = c(y,y+inv,y))
    result$z = with(result,1-x-y)
    if(min(result) < -inv/2 || max(result) > 1 + inv/2) return(data.frame())
    result
  }))
  
  #Lower Triangles
  lower = with(mesh,ddply(mesh,.(IDPoint),function(df,u = max(upper$IDPoint)){
    x = df$x; y=df$y; p = df$IDPoint[1]
    result   = data.frame(x=c(x,x,x-inv),y=c(y,y-inv,y))
    result$z = with(result,1-x-y)
    if(min(result) < -inv/2 || max(result) > 1 + inv/2) return(data.frame())
    result$IDPoint = u + 1 + p
    result
  }))
  
  #Combine
  result   = rbind(upper,lower)
  
  #Determine the balancing composition
  result$z = with(result,1-x-y)
  
  #Now Assemble as polygons
  result$IDPolygon = result$IDPoint
  
  #Remove Columns to do with Mesh and Type
  result$IDPoint = NULL
  
  #Result
  result[,c('IDPolygon','x','y','z')]
}


triBinSummarise = function(x,y,z,w,bins,fun, fun.args=list(), na.value=NA, drop = TRUE){
  poly    = triPoly(bins)
  theBins = with(poly,ddply(poly, .(IDPolygon), here(summarize), 
                            xmin = min(x), xmax = max(x), 
                            ymin = min(y), ymax = max(y), 
                            zmin = min(z), zmax = max(z))
                 )
  df   = data.frame(x,y,z,w)
  stat = with(theBins,ddply(theBins, .(IDPolygon), here(summarize), 
                            value = triStat(df, xmin, xmax, ymin, ymax, zmin, zmax, na.value, fun)))
  merge(poly,stat)
}
