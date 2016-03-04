#' Get Scales List 
#'
#' Scales object encapsulates multiple scales.
#' All input and output done with data.frames to facilitate
#' multiple input and output variables
#' @rdname undocumented
#' @export
scales_list <- function() {
  ggproto(NULL, ScalesList)
}

ScalesList <- ggproto("ScalesList", NULL,
                      scales = NULL,
                      
                      find = function(self, aesthetic) {
                        vapply(self$scales, function(x) any(aesthetic %in% x$aesthetics), logical(1))
                      },
                      
                      has_scale = function(self, aesthetic) {
                        any(self$find(aesthetic))
                      },
                      
                      add = function(self, scale) {
                        prev_aes <- self$find(scale$aesthetics)
                        if (any(prev_aes)) {
                          # Get only the first aesthetic name in the returned vector -- it can
                          # sometimes be c("x", "xmin", "xmax", ....)
                          scalename <- self$scales[prev_aes][[1]]$aesthetics[1]
                          ggint$message_wrap("Scale for '", scalename, ##NH
                                       "' is already present. Adding another scale for '", scalename,
                                       "', which will replace the existing scale.")
                        }
                        
                        # Remove old scale for this aesthetic (if it exists)
                        self$scales <- c(self$scales[!prev_aes], list(scale))
                      },
                      
                      n = function(self) {
                        length(self$scales)
                      },
                      
                      input = function(self) {
                        unlist(lapply(self$scales, "[[", "aesthetics"))
                      },
                      
                      # This actually makes a descendant of self, which is functionally the same
                      # as a actually clone for most purposes.
                      clone = function(self) {
                        ggproto(NULL, self, scales = lapply(self$scales, function(s) s$clone()))
                      },
                      
                      non_position_scales = function(self) {
                        ggproto(NULL, self, scales = self$scales[!self$find("x") & !self$find("y")])
                      },
                      
                      get_scales = function(self, output) {
                        scale <- self$scales[self$find(output)]
                        if (length(scale) == 0) return()
                        scale[[1]]
                      }
)

# Look for object first in parent environment and if not found, then in
# ggplot2 namespace environment.  This makes it possible to override default
# scales by setting them in the parent environment.
find_global <- function(name, env, mode = "any") {
  if (exists(name, envir = env, mode = mode)) {
    return(get(name, envir = env, mode = mode))
  }
  
  nsenv <- asNamespace("ggplot2")
  if (exists(name, envir = nsenv, mode = mode)) {
    return(get(name, envir = nsenv, mode = mode))
  }
  
  NULL
}


# Determine default type of a scale
scale_type <- function(x) UseMethod("scale_type")

#' @export
scale_type.default <- function(x) {
  message("Don't know how to automatically pick scale for object of type ",
          paste(class(x), collapse = "/"), ". Defaulting to continuous")
  "continuous"
}

#' @export
scale_type.logical <- function(x) "discrete"

#' @export
scale_type.character <- function(x) "discrete"

#' @export
scale_type.factor <- function(x) "discrete"

#' @export
scale_type.POSIXt <- function(x) "datetime"

#' @export
scale_type.Date <- function(x) "date"

#' @export
scale_type.numeric <- function(x) "continuous"
