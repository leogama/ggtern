#' Position Legend in Convenient Locations
#' 
#' A convenience function to position the legend at various internal positions
#' @param x the position, valid values are topleft, middleleft, bottomleft, topright, middleright and bottomright
#' @author Nicholas Hamilton
#' @rdname themelegendposition
#' @export
theme_legend_position = function(x='topleft'){
  if(!x %in% c(.pos,.posa)) stop(sprintf("Valid positions are: '%s'",paste(.pos,collapse="', '")))
  .re.xpos = sprintf("(%s)$",paste(.xpos,collapse="|")); 
  .re.ypos = sprintf("^(%s)",paste(.ypos,collapse="|")); 
 
  ypos = gsub(.re.xpos,"",x, ignore.case = TRUE,perl = TRUE);
  xpos = gsub(.re.ypos,"\\2",x, ignore.case = TRUE)
  
  xpos = ifthenelse(xpos=='middle',0.5,ifthenelse(xpos=='right',1,0))
  ypos = ifthenelse(ypos=='middle',0.5,ifthenelse(ypos=='top',  1,0))
  
  theme(legend.position       =c(xpos,ypos),
        legend.justification  =c(xpos,ypos),
        legend.box.just       ='left')
}

.ypos = c('top','middle','bottom')
.xpos = c('left','middle','right')
.pos  = apply(merge(.ypos,.xpos),1,function(x)paste0(x,collapse=''))
.posa = apply(merge(.ypos,.xpos),1,function(x)paste0(substring(x,1,1),collapse=''))
