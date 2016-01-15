#' Complete Themes
#' 
#' \code{ggtern} ships with a number of complete themes:
#' \itemize{
#'  \item{ Black and White Theme:}{ 
#'    \code{\link[=theme_tern_bw]{theme_bw(...)}}
#'  }
#'  \item{Minimal Theme:}{
#'    \code{\link[=theme_tern_minimal]{theme_minimal(...)}}
#'  }
#'  \item{Classic Theme:}{
#'    \code{\link[=theme_tern_classic]{theme_classic(...)}}
#'  }
#'  \item{Gray and White Theme:}{
#'    \code{\link[=theme_tern_gray]{theme_gray(...)}}
#'  }
#'  \item{Red, Green, Blue and White Theme:}{
#'    \code{\link[=theme_tern_rgbw]{theme_rgbw(...)}}
#'  }
#'  \item{Red, Green, Blue and Gray Theme:}{
#'    \code{\link[=theme_tern_rgbg]{theme_rgbg(...)}}
#'  }
#'  \item{Dark Theme:}{
#'    \code{\link[=theme_dark]{theme_dark(...)}}
#'  }
#'  \item{Darker Theme:}{
#'    \code{\link[=theme_darker]{theme_darker(...)}}
#'  }
#'  \item{Light Theme:}{
#'    \code{\link[=theme_light]{theme_light(...)}}
#'  }
#'  \item{Theme with Only Black Lines:}{
#'    \code{\link[=theme_linedraw]{theme_linedraw(...)}}
#'  }
#' }
#' @rdname theme_complete
#' @name theme_complete
#' @author Nicholas Hamilton
NULL

#' ggtern themes
#'
#' Themes set the general aspect of the plot such as the colour of the
#' background, gridlines, the size and colour of fonts.
#'
#' @param base_size base font size
#' @param base_family base font family
#'
#' @details \describe{
#'
#' \item{\code{theme_gray}}{
#' The signature ggplot2 theme with a grey background and white gridlines,
#' designed to put the data forward yet make comparisons easy.}
#'
#' \item{\code{theme_bw}}{
#' The classic dark-on-light ggplot2 theme. May work better for presentations
#' displayed with a projector.}
#'
#' \item{\code{theme_linedraw}}{
#' A theme with only black lines of various widths on white backgrounds,
#' reminiscent of a line drawings. Serves a purpose similar to \code{theme_bw}.
#' Note that this theme has some very thin lines (<< 1 pt) which some journals
#' may refuse.}
#'
#' \item{\code{theme_light}}{
#' A theme similar to \code{theme_linedraw} but with light grey lines and axes,
#' to direct more attention towards the data.}
#'
#' \item{\code{theme_dark}}{
#' The dark cousin of \code{theme_light}, with similar line sizes but a dark background. 
#' Useful to make thin coloured lines pop out.
#' }
#'
#' \item{\code{theme_darker}}{
#' A darker cousing to \code{theme_dark}, with a dark panel background.
#' }
#'
#' \item{\code{theme_minimal}}{
#' A minimalistic theme with no background annotations.
#' }
#'
#' \item{\code{theme_classic}}{
#' A classic-looking theme, with x and y axis lines and no gridlines.
#' }
#' 
#' \item{\code{theme_rgbw}}{
#'  A theme with white background, red, green and blue axes and gridlines
#' }
#' 
#' \item{\code{theme_rgbg}}{
#' A theme with grey background, red, green and blue axes and gridlines
#' }
#'
#' \item{\code{theme_void}}{ 
#' A completely empty theme.
#' }
#' 
#' \item{\code{theme_custom}}{
#' Theme with custom basic colours
#' }
#' }
#'
#' @examples
#' data(Feldspar)
#' p <- ggtern(Feldspar,aes(Ab,An,Or)) + 
#'      geom_point(aes(colour=T.C,size=P.Gpa)) + 
#'      facet_wrap(~Feldspar)
#' 
#' #Uncomment to run
#' p + theme_gray()
#' p + theme_rgbg()
#' p + theme_dark()
#' @aliases theme_tern_gray theme_tern_grey theme_grey theme_tern_bw theme_tern_classic theme_tern_rgbg theme_tern_rgbw theme_tern_minimal
#' @name ggtern_themes
#' @rdname ggtern_themes
NULL

#' @rdname ggtern_themes
#' @export
theme_gray  <- function(base_size = 12, base_family = ""){
  .theme_tern(base_size=base_size, base_family=base_family, 
              panel.background.tern = 'white',
              plot.background.tern  = 'grey92',
              col.T ="gray50",col.L="gray50",col.R="gray50",
              col.axis.T="grey92",col.axis.L="grey92",col.axis.R="grey92",
              col.title.T="black",col.title.L="black",col.title.R="black",
              axis.tern.size = 0.25,ticklength.minor = unit(0,"npc"),
              showarrow=FALSE,
              col.arrow.text.T="black",col.arrow.text.L="black",col.arrow.text.R="black")
}
theme_tern_gray <- function(base_size = 12, base_family = ""){
  tern_dep("1.0.1.3","theme_tern_gray has been superceded by the ggplot2 standard theme_gray")
  theme_gray(base_size,base_family)
}
theme_tern_grey <- theme_tern_gray
theme_grey <- theme_gray

#' @rdname ggtern_themes
#' @export
theme_bw <- function(base_size = 12, base_family = "") {
  # Starts with theme_grey and then modify some parts
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text                  = element_text(size = rel(0.8)),
      axis.tern.line             = element_line(size=0.5),
      axis.ticks                 = element_line(colour = "black"),
      legend.key                 = element_rect(colour = "grey80"),
      panel.background.tern      = element_rect(fill = "white", colour = NA),
      plot.background.tern       = element_rect(fill = NA, colour = 'grey50'),
      panel.border               = element_rect(fill = NA, colour = "grey50",size=0.5),
      panel.grid.tern.major      = element_line(colour = "grey90"),
      panel.grid.tern.major.T    = element_line(),
      panel.grid.tern.major.L    = element_line(),
      panel.grid.tern.major.R    = element_line(),
      panel.grid.tern.minor      = element_line(colour = "grey98", size = 0.25),
      panel.grid.tern.minor.T    = element_line(),
      panel.grid.tern.minor.L    = element_line(),
      panel.grid.tern.minor.R    = element_line(),
      strip.background           = element_rect(fill =  "grey80", colour = "grey50", size = 0.2)
    )
}
theme_tern_bw <- function(base_size = 12, base_family = ""){
  tern_dep("1.0.1.3","theme_tern_bw has been superceded by the ggplot2 standard theme_bw")
  theme_bw(base_size,base_family)
}


#' @rdname ggtern_themes
#' @export
theme_linedraw <- function(base_size = 12, base_family = "") {
  # Starts with theme_grey and then modify some parts
  half_line <- base_size / 2
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      legend.key        = element_rect(colour = "black", size = 0.25),
      strip.background  = element_rect(fill = "black", colour = NA),
      strip.text.x      = element_text(
        colour = "white",
        margin = margin(t = half_line, b = half_line)
      ),
      strip.text.y      = element_text(
        colour = "white",
        angle = 90,
        margin = margin(l = half_line, r = half_line)
      )
    )
}


#' @rdname ggtern_themes
#' @export
theme_classic <- function(base_size = 12, base_family = ""){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
  theme(
    panel.grid.major = element_line(),
    panel.grid.minor = element_line(),
    strip.background = element_rect(colour = "black", size = 0.5),
    legend.key       = element_blank()
  )
}

theme_tern_classic <- function(base_size = 12, base_family = ""){
  tern_dep("1.0.1.3","theme_tern_classic has been superceded by the ggplot2 standard theme_classic")
  theme_classic(base_size,base_family)
}

#' @rdname ggtern_themes
#' @export
theme_void <- function(base_size = 12, base_family = "") {
  theme_bw() %+replace%
  theme(
    line =               element_blank(),
    rect =               element_blank(),
    text =               element_blank(),
    complete = TRUE
  )
}

#' A theme with grey background, red, green and blue axes and white gridlines
#' 
#' \code{theme_rgbg} is a theme with grey background, red, green and blue axes and gridlines
#' @rdname ggtern_themes
#' @export
theme_rgbg  <- function(base_size = 12, base_family = ""){
  theme_rgbw() %+replace%
    theme(
      panel.background.tern    = element_rect(fill='white'),
      plot.background.tern     = element_rect(fill='gray92'),
      panel.grid.tern.minor    = element_line(colour='white')
    )
}
theme_tern_rgbg <- function(base_size = 12, base_family = ""){
  tern_dep("1.0.1.3","theme_tern_rgbg has been superceded by theme_rgbg")
  theme_rgbg(base_size,base_family)
}
theme_rgb <- theme_rgbg

#' @rdname ggtern_themes
#' @export
theme_rgbw  <- function(base_size = 12, base_family = ""){
  .theme_tern(base_size=base_size, base_family=base_family,
              plot.background.tern ="white",
              col.T="darkred",
              col.L="darkblue",
              col.R="darkgreen",
              col.grid.T ="darkred",
              col.grid.L="darkblue",
              col.grid.R="darkgreen",
              col.grid.minor ="gray90",grid.linetype=6,grid.linetype.minor=1,grid.major.size=0.25)
}
theme_tern_rgbw <- function(base_size = 12, base_family = ""){
  tern_dep("1.0.1.3","theme_tern_rgbw has been superceded by theme_rgbw")
  theme_rgbw(base_size,base_family)
}

#' @rdname ggtern_themes
#' @export
theme_minimal <- function(base_size = 12, base_family = "") {
  # Starts with theme_bw and then modify some parts
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      legend.background      = element_blank(),
      legend.key             = element_blank(),
      panel.background       = element_rect(),
      panel.background.tern  = element_rect(fill='white',colour=NA),
      panel.border           = element_blank(),
      strip.background       = element_blank(),
      plot.background        = element_blank(),
      axis.ticks             = element_blank(),
      axis.ticks.length      = unit(0, "lines")
    )
}


#' @rdname ggtern_themes
#' @export
theme_dark <- function(base_size = 12, base_family = "") {
  base = ggplot2::theme_dark()
  base %+replace%
    theme(
      panel.background.tern    = element_rect(fill   = "white", colour = NA),
      plot.background.tern     = base$panel.background,
      panel.grid.tern.major    = base$panel.grid.major,
      panel.grid.tern.major.T  = element_line(),
      panel.grid.tern.major.L  = element_line(),
      panel.grid.tern.major.R  = element_line(),
      panel.grid.tern.minor    = base$panel.grid.minor,
      axis.tern                = base$panel.grid.major,
      axis.tern.line           = element_line(),
      axis.tern.line.T         = element_line(),
      axis.tern.line.L         = element_line(),
      axis.tern.line.R         = element_line()
    )
}

#' @rdname ggtern_themes
#' @export
theme_darker <- function(base_size = 12, base_family = "") {
  base = theme_dark(base_size=base_size,base_family=base_family) 
  base %+replace%
    theme(panel.background.tern = element_rect(fill='grey75'))
}

#' @rdname ggtern_themes
#' @export
theme_light <- function(base_size = 12, base_family = "") {
  base = ggplot2::theme_light(base_size=base_size,base_family=base_family) 
  base %+replace%
    theme(
      panel.background.tern    = element_rect(fill   = "white", colour = NA),
      plot.background.tern     = base$panel.background,
      panel.grid.tern.major    = base$panel.grid.major,
      panel.grid.tern.major.T  = element_line(),
      panel.grid.tern.major.L  = element_line(),
      panel.grid.tern.major.R  = element_line(),
      panel.grid.tern.minor    = base$panel.grid.minor
    )
}


#Internals
#helper function
.theme_tern      <- function(base_size             = 12, 
                             base_family           = "",
                             base_ggplot2_theme    = "theme_gray",
                             panel.background.tern = NA,
                             plot.background.tern  = NA,
                             col.T               = "black",
                             col.L               = "black",
                             col.R               = "black",
                             col.grid.T          = "white",
                             col.grid.L          = "white",
                             col.grid.R          = "white",
                             col.grid.minor      = "grey95",
                             col.axis.T          = col.T,
                             col.axis.L          = col.L,
                             col.axis.R          = col.R,
                             col.arrow.T         = col.T,
                             col.arrow.L         = col.L,
                             col.arrow.R         = col.R,
                             col.title.T         = col.T,
                             col.title.L         = col.L,
                             col.title.R         = col.R,
                             col.arrow.text.T    = col.T,
                             col.arrow.text.L    = col.L,
                             col.arrow.text.R    = col.R,
                             col.ticks.major     = "black",
                             axis.tern.size      = 0.5,
                             showarrow           = getOption("tern.showarrows"),
                             ticks.outside       = getOption("tern.ticks.outside"),
                             ticks.showsecondary = getOption("tern.ticks.showsecondary"),
                             ticks.showprimary   = getOption("tern.ticks.showprimary"),
                             grid.linetype       = 1,
                             grid.linetype.minor = grid.linetype,
                             grid.major.size     = NULL,
                             ticklength.major    = unit(0.010,"npc"),
                             ticklength.minor    = unit(0.005,"npc")){ #,
  #margin.tern         = unit(2,"lines")){  
  #TEXT SIZES
  size.base      <- max(base_size-4,2)
  size.text      <- max(base_size-2,4)
  size.title     <- max(base_size-0,6)
  
  get(base_ggplot2_theme,asNamespace("ggplot2"))(
    base_size=base_size,base_family=base_family)            %+replace%
    theme(
      panel.background.tern      = element_rect(fill=panel.background.tern,color=NA),
      plot.background.tern       = element_rect(fill=plot.background.tern,color=NA),
      axis.tern.clockwise        = getOption("tern.clockwise"),
      axis.tern.showarrows       = showarrow,
      axis.tern.showtitles       = getOption("tern.showtitles"),
      axis.tern.showlabels       = getOption("tern.showlabels"),
      axis.tern.arrowstart       = getOption("tern.arrowstart"),
      axis.tern.arrowfinish      = getOption("tern.arrowfinish"),
      axis.tern.padding          = unit(c(2,0,0,0),"lines"),
      axis.tern.hshift           = getOption("tern.hshift"),
      axis.tern.vshift           = getOption("tern.vshift"),
      axis.tern.arrowsep         = as.numeric(getOption("tern.arrowsep")),
      
      axis.tern               = element_line(size=axis.tern.size,linetype="solid"),
      axis.tern.line          = element_line(),
      axis.tern.line.T        = element_line(colour=col.axis.T),
      axis.tern.line.L        = element_line(colour=col.axis.L),
      axis.tern.line.R        = element_line(colour=col.axis.R),
      
      axis.tern.arrow         = element_line(lineend=arrow(length=unit(2.5,"mm"))),
      axis.tern.arrow.T       = element_line(colour=col.arrow.T),
      axis.tern.arrow.L       = element_line(colour=col.arrow.L),
      axis.tern.arrow.R       = element_line(colour=col.arrow.R),
      
      axis.tern.text          = element_text(size=size.base,face="plain"),
      axis.tern.text.T        = element_text(colour=col.T), #,hjust=-0.2),
      axis.tern.text.L        = element_text(colour=col.L), #,hjust=+1.2),
      axis.tern.text.R        = element_text(colour=col.R), #,hjust=+1.2),
      
      axis.tern.arrow.text    = element_text(size=size.text,face="plain"),
      axis.tern.arrow.text.T  = element_text(colour=col.arrow.text.T),#, vjust=-0.2),
      axis.tern.arrow.text.L  = element_text(colour=col.arrow.text.L),#, vjust=-0.2),
      axis.tern.arrow.text.R  = element_text(colour=col.arrow.text.R),#, vjust= 1.2),
      
      axis.tern.title         = element_text(size=size.title,face="bold"),
      axis.tern.title.T       = element_text(colour=col.title.T),#,vjust=NULL,hjust=NULL),#,vjust= -0.1),
      axis.tern.title.L       = element_text(colour=col.title.L),#,vjust=NULL,hjust=NULL),#,hjust= +1.1),
      axis.tern.title.R       = element_text(colour=col.title.R),#,vjust=NULL,hjust=NULL),#,hjust= -0.1),
      
      panel.grid.tern         = element_line(linetype=grid.linetype),
      panel.grid.tern.major   = element_line(color="black",size=grid.major.size),
      panel.grid.tern.major.T = element_line(colour=col.grid.T),
      panel.grid.tern.major.L = element_line(colour=col.grid.L),
      panel.grid.tern.major.R = element_line(colour=col.grid.R),
      panel.grid.tern.minor   = element_line(size=0.25,colour=col.grid.minor,linetype=grid.linetype.minor),
      
      axis.tern.ticks.outside       = ticks.outside,
      axis.tern.ticks.showsecondary = ticks.showsecondary,
      axis.tern.ticks.showprimary   = ticks.showprimary,
      
      axis.tern.ticklength.major    = ticklength.major,
      axis.tern.ticklength.minor    = ticklength.minor,
      
      axis.tern.ticks         = element_line(),
      axis.tern.ticks.major   = element_line(color=col.ticks.major),
      axis.tern.ticks.major.T = element_line(colour=col.T),
      axis.tern.ticks.major.L = element_line(colour=col.L),
      axis.tern.ticks.major.R = element_line(colour=col.R),
      
      axis.tern.ticks.minor   = element_line(size=0.20),
      axis.tern.ticks.minor.T = element_line(colour=col.T),
      axis.tern.ticks.minor.L = element_line(colour=col.L),
      axis.tern.ticks.minor.R = element_line(colour=col.R),
      
      panel.tern.expand       = getOption('tern.expand'),
      #panel.margin.tern      = margin.tern
      panel.tern.rotate       = 0,
      panel.grid.tern.ontop   = FALSE
    )
}

#' @param col.T colour of top axis, ticks labels and major gridlines
#' @param col.L colour of left axis, ticks, labels and major gridlines
#' @param col.R colour of right axis, ticks, labels and major gridlines
#' @param col.BG colour of the plot background area
#' @param plot.background.tern colour of background colour to plot area
#' @param col.grid.minor the colour of the minor grid
#' \code{theme_custom} is a convenience function to allow the user to control the basic theme colours very easily.
#' @rdname ggtern_themes
#' @export
theme_custom  <- function(base_size = 12,
                          base_family = "",
                          plot.background.tern = 'gray92',
                          col.T='gray95',
                          col.L='gray95',
                          col.R='gray95',
                          col.BG="transparent",
                          col.grid.minor="gray90"){
  .theme_tern(base_size=base_size, base_family=base_family, 
              plot.background.tern = plot.background.tern,
              col.T      = col.T,
              col.L      = col.L,
              col.R      = col.R,
              col.grid.T = col.T,
              col.grid.L = col.L,
              col.grid.R = col.R,
              col.grid.minor = col.grid.minor,
              grid.linetype=6,
              grid.linetype.minor=1,
              grid.major.size=0.25)
}


