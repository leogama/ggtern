.onLoad <- function(libname, pkgname){

  #Set the options
  .setOptionsCurrent()
  .setOptionsDepreciated()
  
  #Set the theme and the last coordinates.
  theme_set(theme_gray())
}

.onAttach <- function(libname, pkgname){
  lines = c("--",
            "Consider donating at: http://ggtern.com",
            "Even small amounts (say $10-50) are very much appreciated!",
            sprintf("Remember to cite, run citation(package = '%s') for further info.",pkgname),
            "--")
  msg = paste(lines,collapse="\n")
  packageStartupMessage(msg)
}

#------------------------------------------------------------------------------
#CURRENT OPTIONS
#------------------------------------------------------------------------------
.setOptionsCurrent <- function(){
  options("tern.expand"                = 0.2)
  options('tern.margin'                = unit(0,'pt'))
  options('tern.arrow'                 = arrow(length=unit(2.5,"mm")))
  options("tern.default.T"             = "y")
  options("tern.default.L"             = "x")
  options("tern.default.R"             = "z")
  options("tern.clockwise"             = FALSE)
  options("tern.title.show"            = TRUE)
  options("tern.text.show"             = TRUE)
  options("tern.axis.ontop"            = FALSE)
  options("tern.arrow.start"           = 0.3)
  options("tern.arrow.finish"          = 0.7)
  options("tern.arrow.show"            = FALSE)
  options('tern.arrow.sep'             = 0.1)
  options('tern.vshift'                = 0.0)
  options('tern.hshift'                = 0.0)
  options("tern.ticks.outside"         = TRUE)
  options("tern.ticks.primary.show"    = TRUE)
  options("tern.ticks.secondary.show"  = FALSE)
  options("tern.breaks.default"        = seq(0.0, 1.0,by=0.2))
  options("tern.breaks.default.minor"  = seq(0.1, 0.9,by=0.2))
  options("tern.grid.major.show"       = TRUE)
  options("tern.grid.minor.show"       = TRUE)
  options("tern.grid.ontop"            = FALSE)
  options("tern.mask.show"             = TRUE)
  options("tern.mask.debug"            = FALSE)
  options("tern.rotate"                = 0)
  options("tern.latex"                 = FALSE)
}

#------------------------------------------------------------------------------
#DEPRECIATED OPTIONS -- ie either not used anymore or in depreciated functions.
#------------------------------------------------------------------------------
.setOptionsDepreciated <- function(){
  options("tern.discard.external"      = TRUE)
  options("tern.expand.contour.inner"  =-0.0005)
  options("tern.dont_transform"        = FALSE)
  options("tern.mesh.buffer"           = 1.50)
  options("tern.mesh.size"             = 200)
}

#------------------------------------------------------------------------------
#MANUAL EXECUTION -- BUILD STATICDOCS
#------------------------------------------------------------------------------
if(FALSE){
  library(staticdocs)
  library(ggtern)
  library(knitr)
  library(whisker)
  library(httr)
  library(lubridate)
  
  makeSitemap = function(site_path=NULL){
    if(is.null(site_path) || !is.character(site_path) || !dir.exists(site_path))
      site_path = "inst/web"
    
    #THE XML TEMPLATE
    tpl <-'<?xml version="1.0" encoding="UTF-8"?>
<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
  {{#links}}
  <url>
    <loc>{{{loc}}}</loc>
    <lastmod>{{{lastmod}}}</lastmod>
    <changefreq>{{{changefreq}}}</changefreq>
    <priority>{{{priority}}}</priority>
  </url>
  {{/links}}
</urlset>'
    
    #Map all the html files to xml records
    dest  <- sprintf('http://www.ggtern.com/d/%s',packageVersion("ggtern"))
    links <- list.files(site_path,pattern = 'html$')
    map_links <- function(l,base = dest,live=FALSE) {
      l   <- sprintf("%s/%s",base,l)
      d   <- if(live) GET(l)$headers[['last-modified']] else now()
      list(loc        = l,
           lastmod    = format(as.Date(d,format="%a, %d %b %Y %H:%M:%S")),
           changefreq = "monthly",
           priority   = "0.8")
    }
    links       <- lapply(links, map_links)
    output_file <- sprintf("%s/sitemap.xml",site_path)
    
    message(sprintf("Writing file: %s",output_file))
    cat(whisker.render(tpl),
        file = output_file)
  }
  
  build_demos = function (pkg = ".") {
    require(stringr)
    require(evaluate)
    pkg <- as.sd_package(pkg)
    demo_dir <- file.path(pkg$path, "demo")
    if (!file.exists(demo_dir)) 
      return()
    message("Rendering demos")
    demos <- readLines(file.path(demo_dir, "00Index"))
    pieces <- str_split_fixed(demos, "\\s+", 2)
    in_path <- str_c(pieces[, 1], ".r")
    filename <- str_c("demo-", pieces[, 1], ".html")
    title <- pieces[, 2]
    for (i in seq_along(title)) {
      demo_code <- readLines(file.path(demo_dir, in_path[i]))
      demo_expr <- evaluate(demo_code, new.env(parent = globalenv()), new_device = FALSE)
      pkg$demo <- staticdocs:::replay_html(demo_expr, pkg = pkg, name = str_c(pieces[i],"-"))
      pkg$pagetitle <- sprintf("Demo: %s",title[i])
      pkg$title <- pkg$pagetitle
      render_page(pkg, "demo", pkg, file.path(pkg$site_path, filename[i]))
    }
    list(demo = unname(apply(cbind(filename,title), 1, as.list)))
  }
  
  #Make the site and the sitemap
  knit(input = "./inst/staticdocs/README.rmd", output = "./inst/staticdocs/README.md")
  build_site(pkg = ".")
  build_demos(pkg = ".")
  makeSitemap()
}



