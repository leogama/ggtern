#Expose some required functions from the parent ggplot2 namespace
.getFunctions <- function(){

.functions.ggplot2   = c('new_panel','train_layout','train_position','train_ranges','expand_default',
                         'map_position','map_layout','map_position','reset_scales','plot_theme',
                         'facet_render','xlabel','ylabel','element_render','message_wrap',
                         'set_last_plot','make_labels','build_guides','is.zero','add_ggplot','labelGrob',
                         'is.layer','is.facet','is.Coord','GeomSegment',
                         '.element_tree','el_def','combine_elements','aes_to_scale',
                         'is.Coord','is.facet','is.layer','make_labels','update_labels','update_guides',
                         'aes_to_scale',
                         'scales_add_missing','scales_list','scales_transform_df','scales_map_df','scales_train_df',
                         'predictdf','contour_lines','check_required_aesthetics','snake_class',
                         'ggname','ggplot_gtable','camelize',
                         'element_grob.element_line','element_grob.element_rect','element_grob.element_text','element_grob.element_blank',
                         'plot_clone')
.functions.gridExtra  = c('latticeGrob')
  .functions          = rbind(data.frame(p='ggplot2',  f=unique(.functions.ggplot2)),
                              data.frame(p='gridExtra',f=unique(.functions.gridExtra)))

  structure(
    mapply(function(f,p){ getFromNamespace(f,p) },as.character(.functions$f), as.character(.functions$p)),
    class=c("internal")
  )
}
ggint <- .getFunctions()


.buildStaticDocs = function(){
  library(staticdocs)
  library(ggtern)
  library(knitr)
  library(whisker)
  library(httr)
  library(lubridate)
  require(stringr)
  require(evaluate)
  
  build_site = function(pkg = "."){
    pkg = as.sd_package(pkg)
    inputRMD = sprintf("%s/README.rmd",pkg$sd_path)
    if(file.exists(inputRMD))
      knit(input=inputRMD,output=sub("rmd$",'md',inputRMD,perl=TRUE))
    invisible(staticdocs::build_site(pkg))
  }
  
  build_demos = function (pkg = ".") {
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
      
      #NH: replay_html is not exported...
      replay_html <- getFromNamespace('replay_html','staticdocs')
      
      pkg$demo      <- replay_html(demo_expr, pkg = pkg, name = str_c(pieces[i],"-"))
      pkg$pagetitle <- sprintf("Demo: %s",title[i])
      
      #NH: Need to set the title attribute...
      pkg$title     <- pkg$pagetitle
      
      render_page(pkg, "demo", pkg, file.path(pkg$site_path, filename[i]))
    }
    invisible(list(demo = unname(apply(cbind(filename,title), 1, as.list))))
  }
  
  #Function to build sitemap
  build_sitemap = function(pkg=".",destination = NULL){
    pkg        <- as.sd_package(pkg)
    site_path <- pkg$site_path
    
    if(!dir.exists(site_path)) 
      build_site(pkg)
    
    #THE XML TEMPLATE
    tpl <- '<?xml version="1.0" encoding="UTF-8"?>
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
    if(is.null(destination))
      destination = sprintf('%s/d/%s',pkg$urls[1],packageVersion("ggtern"))
    links <- list.files(site_path,pattern = 'html$')
    map_links <- function(l,base = baseurl,live=FALSE) {
      l   <- sprintf("%s/%s",destination,l)
      d   <- if(live) GET(l)$headers[['last-modified']] else now()
      list(loc        = l,
           lastmod    = format(as.Date(d,format="%a, %d %b %Y %H:%M:%S")),
           changefreq = "monthly",
           priority   = "0.8")
    }
    links       <- lapply(links, map_links)
    output_file <- sprintf("%s/sitemap.xml",site_path)
    
    message(sprintf("Writing file: %s",output_file))
    invisible(cat(whisker.render(tpl),file = output_file))
  }
  
  #Make the site and the sitemap
  build_site(pkg = ".")
  build_demos(pkg = ".")
  build_sitemap(pkg = ".")
}
