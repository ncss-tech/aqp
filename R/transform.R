#' @title Transform a SPC with expressions based on site or horizon level attributes
#' 
#' @rdname transform
#' 
#' @aliases mutate mutate,SoilProfileCollection-method
#' @description \code{transform()} is a function used for modifying columns in SoilProfileCollections. 
#' 
#' It allows the user to specify an arbitrary number of expressions that resolve to the (re-)calculation of one or more site or horizon level attributes. For instance: \code{mutate(spc, thickness = hzdepb - hzdept)}. The expressions may depend on one another, and are evaluated from left to right.
#' 
#' @param _data A SoilProfileCollection
#' 
#' @param ... Comma-separated set of R expressions e.g. \code{thickness = hzdepb - hzdept, hzdepm = hzdept + round(thk / 2)}
#' 
#' @return A SoilProfileCollection
#' @author Andrew G. Brown.
#'
#' @export 
setMethod("transform", signature(`_data` = "SoilProfileCollection"), 
          function(`_data`, ...)  {

  # create composite object to facilitate eval
  .data <- compositeSPC(`_data`)

  # capture expression(s) at function
  .dots <- substitute(list(...))
  .dots <- .dots[2:length(.dots)]
  .names <- names(.dots)

  if (is.null(.names))
    .names <- as.character(.dots)

  for(n in 1:length(.dots)) {
    foo <- .data_dots(.data, eval(.dots[[n]]))[[1]]
    `_data`[[.names[n]]] <- foo
    .data[[.names[n]]] <- foo
  }

  return(`_data`)
})

# if (!isGeneric("mutate"))
  setGeneric("mutate", function(object, ...) standardGeneric("mutate"))

setMethod("mutate", signature(object = "SoilProfileCollection"), function(object, ...) {
   .Deprecated("transform")
  aqp::transform(object, ...)
})
