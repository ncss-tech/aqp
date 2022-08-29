
#' Store groupings within a profile collection.
#' @name groupSPC
#' @param object SoilProfileCollection.
#' @param ... One or more expressions evaluated within the context of \code{object} that resolve to vectors that can be coerced to factor "groups."
#' @export
groupSPC <- function(object, ...) {

    # capture expression(s) at function
    x <- substitute(list(...))

    if (is.null(names(x)))
      names(x) <- as.character(x)

    # create composite object to facilitate eval_tidy
    data <- compositeSPC(object)

    # evaluate n expressions to get n grouping vars
    foo <- NULL

    for(n in 2:length(x)) {

      bar <- as.character(x[[n]])

      if(inherits(foo, 'try-error'))
        stop("groupSPC expects expressions that resolve to one or more site or horizon level column names", call.=FALSE)

      if(any(is.na(bar))) {
        stop(sprintf("groupSPC expects no NA values in '%s'", n), call.=FALSE)
      } else if(length(bar) == length(object)){
        foo <- c(foo, n)
      } else if(bar %in% siteNames(object)) {
        foo <- c(foo, bar)
      } else {
        stop("groupSPC expects expressions that resolve to one or more site or horizon level attributes", call.=FALSE)
      }
    }

    # TODO: safe getter/setter metod
    # set value of metadata "SPC group" - which defines one or more
    #   site (or horizon) level attrs that are used for grouping in certain functions
    #   in order of precedence, pipe-delimited.
    object@metadata$aqp_group_by <- paste0(foo, collapse = "|")

    return(object)
}

