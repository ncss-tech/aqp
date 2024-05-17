#' @title Transform a SPC (by profile) with a set of expressions
#' @name mutate_profile
#' @aliases mutate_profile,SoilProfileCollection-method
#' @description \code{mutate_profile()} is a function used for transforming SoilProfileCollections. Each expression is applied to site or horizon level attributes of individual profiles. This distinguishes this function from \code{transform}, which is applied to all values in a collection, regardless of which profile they came from.
#' @param object A SoilProfileCollection
#' @param ... A set of comma-delimited R expressions that resolve to a transformation to be applied to a single profile e.g \code{mutate_profile(hzdept = max(hzdept) - hzdept)}
#' @param col_names character. Optional column names. Should match the number of expressions in `...`.
#' @param horizon_level logical. If `TRUE` results of expressions are added to the SoilProfileCollection's horizon slot, if `FALSE` the results are added to the site slot. If `NULL` (default) the results are stored in the site or horizon slot based on the number of rows in each slot compared to the length of the result calculated from the _first_ and _last_ profile in the collection.
#' 
#' @details If the length an expression's result matches the number of horizons, the result is stored as a horizon-level variable. If the result has length 1, it is stored as a site-level variable. In the ambiguous case where the first and last profile have only _one_ horizon, the results are stored in the horizon slot by default. To force results into site slot use `horizon_level = FALSE`.
#' @return A SoilProfileCollection.
#' @author Andrew G. Brown.
#'
#' @rdname mutate_profile
#' @export mutate_profile
#' @examples
#' 
#' data(sp4)
#' depths(sp4) <- id ~ top + bottom
#' 
#' mutate_profile(sp4, clay_wtd_average = weighted.mean(clay, bottom - top))
#' 
# if (!isGeneric("mutate_profile"))
setGeneric("mutate_profile", function(object, ..., col_names = NULL, horizon_level = NULL) standardGeneric("mutate_profile"))

setMethod("mutate_profile", signature(object = "SoilProfileCollection"), 
          function(object, ..., col_names = NULL, horizon_level = NULL) {

    # capture expression(s) at function
    .dots <- substitute(list(...))
    .dots <- .dots[2:length(.dots)]
    .names <- names(.dots)
    
    if (is.null(.names)) {
      if (!is.null(col_names) && length(col_names) == length(.dots)) {
        .names <- col_names
      } else {
        .names <- as.character(.dots)
      }
    }
    
    mutate_profile_raw(object, .dots, .names, horizon_level = horizon_level)
})

#' @param expr A list of expressions in terms of column names in site or horizon table of `object`
#' @rdname mutate_profile
#' @export mutate_profile
#' @examples
#' data(jacobs2000)
#' 
#' set.seed(123)
#' 
#' ## col_names allows for column names to be calculated
#' x <- mutate_profile(jacobs2000, bottom - top / sum(bottom - top),
#'                     col_names = paste0("relthk", floor(runif(1, 0, 100))))
#' x$relthk28
#' 
#' # mutate_profile_raw allows for lists of expressions to be evaluated
#' master_desgn <- c("O", "A", "E", "B", "C", "R", "L", "M")
#' thk_names <- paste0("thk_", master_desgn)
#' 
#' # calculate thickness for each horizon
#' x$thk <- x$bottom - x$top
#' 
#' ## construct an arbitrary number of expressions using variable inputs
#' ops <- lapply(master_desgn, function(x) {
#'   substitute(sum(thk[grepl(PATTERN, name)], na.rm = TRUE), list(PATTERN = x))
#' })
#' names(ops) <- thk_names
#' 
#' # do mutation
#' y <- mutate_profile_raw(x, ops)
#' 
#' site(y)[c(idname(y), thk_names)]
mutate_profile_raw <- function(object, expr, col_names = NULL, horizon_level = NULL) {
  
  idn <- idname(object)
  hzidn <- hzidname(object)
  
  # preference is use col_names if specified
  
  # if no col_names, use the names of expr list
  if (is.null(col_names)) {
    col_names <- names(expr)
  }
  
  # if expr is unnamed, use character conversion of expression
  if (is.null(col_names)) {
    col_names <- as.character(expr)
  }
  
  # cleaner to have horizon_level be applied to each expression independently
  hzin <- horizon_level
  if (is.null(horizon_level) || !is.logical(horizon_level)) {
    horizon_level <- rep(FALSE, length(expr))
  }
  
  x <- data.table::data.table(object@site)[object@horizons, on = idn]
  o1 <- object[1, ]
  o2 <- object[nrow(object), ]  
  o1c <- compositeSPC(o1)
  o2c <- compositeSPC(o2)
  
  # iterate over expressions left to right
  for (i in 1:length(expr)) {
    
    # default is to create site-level properties unless result matches number of horizons
    # decide whether we are adding/modifying a site or horizon level variable so
    #  that degenerate cases do not create identical columns in site and horizon table or get put in unexpected slot
    #  2021-10-29: updated to use first and last profile, and allowing user override via argument
    di <- expr[[i]]
    res_eval1 <- .data_dots(o1c, eval(di))[[1]]
    res_eval2 <- .data_dots(o2c, eval(di))[[1]]
    
    # allow user to override the determination
    # check length of first/last profile result against number of horizons
    if (length(res_eval1) == nrow(o1) && length(res_eval2) == nrow(o2)) {
      horizon_level[i] <- TRUE
    }
    .SD <- NULL
    
    # remove any existing columnnames before joining in result
    if (any(col_names[i] %in% names(object))) {
      for (n in col_names[i]) {
        object[[n]] <- NULL
      }
    }
    
    if (isFALSE(hzin) || !horizon_level[i]) {
      res <- x[, list(eval(expr[[i]], envir = .SD)), by = c(idn)]
      if (length(res[[2]]) > length(object)) {
        stop("mutate_profile: some profiles returned more than one result and `horizon_level=FALSE`", call. = FALSE)
      }
      colnames(res) <- c(idn, col_names[i])
      site(object) <- res
    } else {
      res <- x[, list(.hzidname = .SD[[hzidn]], eval(expr[[i]], envir = .SD)), by = c(idn)]
      colnames(res) <- c(idn, hzidn, col_names[i])
      horizons(object) <- res
    }
    
  }
  
  return(object)
}
