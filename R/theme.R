#' Overloaded ggplot2 functions
#' 
#' @description INTERNAL FUNCTIONS (Overloaded from ggplot2): The source of the following functions originate 
#' from ggplot2, however, minor patches were required in order for them to function under the ggtern framework. 
#' Patches were mainly to do with handling the new theme elements and heirarchies. 
#' @format functions and objects
#' @keywords internal
#' @rdname overloaded
#' @name zzz-overloaded
NULL

#' @description \code{validate_element} is a local copy of the ggplot2 function which checks the validity of a given theme element 
#' against the elements table. Since the \code{.elements_tree} is an internal function, which is not exported, and modifications could not be made, 
#' a new (and equivalent) \code{.element_tree} is created within ggtern to handle the new theme elements created within this package.
#' @param el the element
#' @param elname the element name
#' @author Nicholas Hamilton
#' @rdname overloaded
validate_element <- function(el, elname) {
  eldef <- ggint$.element_tree[[elname]]
  
  if (is.null(eldef)) {
    stop('"', elname, '" is not a valid theme element name.')
  }
  
  # NULL values for elements are OK
  if (is.null(el)) return()
  
  if (eldef$class == "character") {
    # Need to be a bit looser here since sometimes it's a string like "top"
    # but sometimes its a vector like c(0,0)
    if (!is.character(el) && !is.numeric(el))
      stop("Element ", elname, " must be a string or numeric vector.")
  } else if (eldef$class == "margin") {
    if (!is.unit(el) && length(el) == 4)
      stop("Element ", elname, " must be a unit vector of length 4.")
  } else if (!inherits(el, eldef$class) && !inherits(el, "element_blank")) {
    stop("Element ", elname, " must be a ", eldef$class, " object.")
  }
  invisible()
}

#' @rdname overloaded
#' @inheritParams ggplot2::theme_update
#' @seealso \code{\link[ggplot2]{theme_update}}
theme_update <- function(...) {
  # Make a call to theme, then add to theme
  theme_set(theme_get() %+replace% do.call(theme, list(...)))
}

#' Set theme elements (ggtern version)
#' 
#' Use this function to modify theme settings.
#' @inheritParams ggplot2::theme
#' @author Nicholas Hamilton
#' @rdname theme
#' @export
theme <- function(
  line,
  rect,
  text,
  title,
  aspect.ratio,
  axis.title,
  axis.title.x,
  axis.title.x.top,
  axis.title.x.bottom,
  axis.title.y,
  axis.title.y.left,
  axis.title.y.right,
  axis.text,
  axis.text.x,
  axis.text.x.top,
  axis.text.x.bottom,
  axis.text.y,
  axis.text.y.left,
  axis.text.y.right,
  axis.ticks,
  axis.ticks.x,
  axis.ticks.x.top,
  axis.ticks.x.bottom,
  axis.ticks.y,
  axis.ticks.y.left,
  axis.ticks.y.right,
  axis.ticks.length,
  axis.line,
  axis.line.x,
  axis.line.x.top,
  axis.line.x.bottom,
  axis.line.y,
  axis.line.y.left,
  axis.line.y.right,
  legend.background,
  legend.margin,
  legend.spacing,
  legend.spacing.x,
  legend.spacing.y,
  legend.key,
  legend.key.size,
  legend.key.height,
  legend.key.width,
  legend.text,
  legend.text.align,
  legend.title,
  legend.title.align,
  legend.position,
  legend.direction,
  legend.justification,
  legend.box,
  legend.box.just,
  legend.box.margin,
  legend.box.background,
  legend.box.spacing,
  panel.background,
  panel.border,
  panel.spacing,
  panel.spacing.x,
  panel.spacing.y,
  panel.grid,
  panel.grid.major,
  panel.grid.minor,
  panel.grid.major.x,
  panel.grid.major.y,
  panel.grid.minor.x,
  panel.grid.minor.y,
  panel.ontop,
  plot.background,
  plot.title,
  plot.subtitle,
  plot.caption,
  plot.margin,
  strip.background,
  strip.placement,
  strip.text,
  strip.text.x,
  strip.text.y,
  strip.switch.pad.grid,
  strip.switch.pad.wrap,
                  tern.axis.arrow,
                  tern.axis.arrow.finish,
                  tern.axis.arrow.L,
                  tern.axis.arrow.R,
                  tern.axis.arrow.sep,
                  tern.axis.arrow.show,
                  tern.axis.arrow.start,
                  tern.axis.arrow.T,
                  tern.axis.arrow.text,
                  tern.axis.arrow.text.L,
                  tern.axis.arrow.text.R,
                  tern.axis.arrow.text.T,
                  tern.axis.clockwise,
                  tern.axis.hshift,
                  tern.axis.line,
                  tern.axis.line.L,
                  tern.axis.line.ontop,
                  tern.axis.line.R,
                  tern.axis.line.T,
                  tern.axis.text,
                  tern.axis.text.L,
                  tern.axis.text.R,
                  tern.axis.text.show,
                  tern.axis.text.T,
                  tern.axis.ticks,
                  tern.axis.ticks.length.major,
                  tern.axis.ticks.length.minor,
                  tern.axis.ticks.major,
                  tern.axis.ticks.major.L,
                  tern.axis.ticks.major.R,
                  tern.axis.ticks.major.T,
                  tern.axis.ticks.minor,
                  tern.axis.ticks.minor.L,
                  tern.axis.ticks.minor.R,
                  tern.axis.ticks.minor.T,
                  tern.axis.ticks.outside,
                  tern.axis.ticks.primary.show,
                  tern.axis.ticks.secondary.show,
                  tern.axis.title,
                  tern.axis.title.L,
                  tern.axis.title.R,
                  tern.axis.title.show,
                  tern.axis.title.T,
                  tern.axis.vshift,
                  tern.panel.background,
                  tern.panel.expand,
                  tern.panel.grid.major,
                  tern.panel.grid.major.L,
                  tern.panel.grid.major.R,
                  tern.panel.grid.major.T,
                  tern.panel.grid.major.show,
                  tern.panel.grid.minor,
                  tern.panel.grid.minor.L,
                  tern.panel.grid.minor.R,
                  tern.panel.grid.minor.T,
                  tern.panel.grid.minor.show,
                  tern.panel.grid.ontop,
                  tern.panel.mask.show,
                  tern.panel.rotate,
                  tern.plot.background,
                  tern.plot.latex,
                  ..., 
                  complete = FALSE, validate = TRUE) {
  
  elements <- ggint$find_args(..., complete = NULL, validate = NULL)
  
  if (!is.null(elements$axis.ticks.margin)) {
    warning("`axis.ticks.margin` is deprecated. Please set `margin` property ",
            " of `axis.text` instead", call. = FALSE)
    elements$axis.ticks.margin <- NULL
  }
  if (!is.null(elements$panel.margin)) {
    warning("`panel.margin` is deprecated. Please use `panel.spacing` property ",
            "instead", call. = FALSE)
    elements$panel.spacing <- elements$panel.margin
    elements$panel.margin <- NULL
  }
  if (!is.null(elements$panel.margin.x)) {
    warning("`panel.margin.x` is deprecated. Please use `panel.spacing.x` property ",
            "instead", call. = FALSE)
    elements$panel.spacing.x <- elements$panel.margin.x
    elements$panel.margin.x <- NULL
  }
  if (!is.null(elements$panel.margin.y)) {
    warning("`panel.margin` is deprecated. Please use `panel.spacing` property ",
            "instead", call. = FALSE)
    elements$panel.spacing.y <- elements$panel.margin.y
    elements$panel.margin.y <- NULL
  }
  if (is.unit(elements$legend.margin) && !is.margin(elements$legend.margin)) {
    warning("`legend.margin` must be specified using `margin()`. For the old ",
            "behavior use legend.spacing", call. = FALSE)
    elements$legend.spacing <- elements$legend.margin
    elements$legend.margin <- margin()
  }
  
  # Check that all elements have the correct class (element_text, unit, etc)
  if (validate) {
    mapply(validate_element, elements, names(elements))
  }
  
  structure(elements, 
            class    = c("theme", "gg"),
            complete = complete, 
            validate = validate)
}

#' \code{plot_theme} is a local copy of the method that determines the net theme between a plot and the current global theme.
#' @param x gg object
#' @rdname overloaded
plot_theme <- function(x) {defaults(x$theme, theme_get())}


#' \code{add_theme} is a local copy of the ggplot2 function which modifies the current theme, by a proposed theme. 
#' It is slightly modified to handle 'logical' values the same way it handles 'character' or 'numeric' values, 
#' which do not inherit from 'element' objects.
#' @inheritParams ggplot2::add_theme
#' @seealso \code{\link[ggplot2]{add_theme}}
#' @rdname overloaded
add_theme <- function(t1, t2, t2name) {
  if (!is.theme(t2)) {
    stop("Don't know how to add ", t2name, " to a theme object",
         call. = FALSE)
  }
  
  # Iterate over the elements that are to be updated
  for (item in names(t2)) {
    x <- t1[[item]]
    y <- t2[[item]]
    
    if (is.null(x) || inherits(x, "element_blank")) {
      # If x is NULL or element_blank, then just assign it y
      x <- y
    } else if (is.null(y) || is.character(y) || is.numeric(y) || is.logical(y) ||
               inherits(y, "element_blank")) {
      # If y is NULL, or a string or numeric vector, or is element_blank, just replace x
      x <- y
    } else {
      # If x is not NULL, then copy over the non-NULL properties from y
      # Get logical vector of non-NULL properties in y
      idx <- !vapply(y, is.null, logical(1))
      # Get the names of TRUE items
      idx <- names(idx[idx])
      
      # Update non-NULL items
      x[idx] <- y[idx]
    }
    
    # Assign it back to t1
    # This is like doing t1[[item]] <- x, except that it preserves NULLs.
    # The other form will simply drop NULL values
    t1[item] <- list(x)
  }
  
  # If either theme is complete, then the combined theme is complete
  attr(t1, "complete") <- attr(t1, "complete") || attr(t2, "complete")
  t1
}

#' \code{"\%+replace\%"} is a local copy of the ggplot2 replace operator, no different other than being exported from the ggtern namespace.
#' @rdname overloaded 
"%+replace%" <- function(e1, e2) {
  if (!is.theme(e1) || !is.theme(e2)) {
    stop("%+replace% requires two theme objects", call. = FALSE)
  }
  # Can't use modifyList here since it works recursively and drops NULLs
  e1[names(e2)] <- e2
  e1
}

#' \code{update_theme} is a local copy of a ggplot2 function, which copies elements from the new theme into an old theme.
#' @param oldtheme previous theme object
#' @param newtheme new theme object
#' @rdname overloaded
update_theme <- function(oldtheme, newtheme) {
  if (attr(newtheme, "complete")) 
    return(newtheme)
  newitems <- !names(newtheme) %in% names(oldtheme)
  newitem_names <- names(newtheme)[newitems]
  oldtheme[newitem_names] <- theme_get()[newitem_names]
  old.validate <- isTRUE(attr(oldtheme, "validate"))
  new.validate <- isTRUE(attr(newtheme, "validate"))
  oldtheme <- do.call(theme, c(oldtheme, complete = isTRUE(attr(oldtheme, 
                                                                "complete")), validate = old.validate & new.validate))
  oldtheme + newtheme
}


#' \code{calc_element} is a local copy of the ggplot2 function which determines the net element based on inheritances, given input theme.
#' @inheritParams ggplot2::calc_element
#' @rdname overloaded
#' @export
calc_element <- function (element, theme, verbose = FALSE) {
  if (verbose) 
    message(element, " --> ", appendLF = FALSE)
  if (inherits(theme[[element]], "element_blank")) {
    if (verbose) 
      message("element_blank (no inheritance)")
    return(theme[[element]])
  }
  #.element_tree = ggint$.element_tree ##NH
  if (!is.null(theme[[element]]) && !inherits(theme[[element]], 
                                              ggint$.element_tree[[element]]$class)) {
    stop(element, " should have class ", ggint$.element_tree[[element]]$class)
  }
  pnames <- ggint$.element_tree[[element]]$inherit ##NH
  if (is.null(pnames)) {
    nullprops <- vapply(theme[[element]], is.null, logical(1))
    if (any(nullprops)) {
      stop("Theme element '", element, "' has NULL property: ", 
           paste(names(nullprops)[nullprops], collapse = ", "))
    }
    if (verbose) 
      message("nothing (top level)")
    return(theme[[element]])
  }
  if (verbose) 
    message(paste(pnames, collapse = ", "))
  parents <- lapply(pnames, calc_element, theme, verbose)
  Reduce(ggint$combine_elements, parents, theme[[element]])
}
