#' Split a SoilProfileCollection into a list based on types of horizon logic errors
#'
#' Uses \code{checkHzDepthLogic} to identify presence of depth logic errors, same depths, missing depths, and overlaps/gaps between the horizons of each profile in a SoilProfileCollection.
#' 
#' @param object A SoilProfileCollection
#' @param interact Calculate interaction between the four logic errors for groups? Default: \code{FALSE} always returns 4 groups, one for each logic error type.
#' @param ... Additional arguments to \code{split.default}, called when \code{interact = TRUE}
#'
#' @return A named list of SoilProfileCollections (or \code{NULL}), with names: "depthLogic", "sameDepth", "missingDepth", "overlapOrGap". If \code{interact = TRUE} then the list elements groups determined by \code{interaction()} of the error types.
#' @export
#'
#' @examples
#' 
#' data(sp4)
#' depths(sp4) <- id ~ top + bottom
#' 
#' # no errors (all four list elements return NULL)
#' splitLogicErrors(sp4)
#' 
#' # NA in top depth triggers depth logic and missing depth errors
#' data(sp4)
#' sp4$top[1] <- NA
#' depths(sp4) <- id ~ top + bottom
#' 
#' splitLogicErrors(sp4)
#' 
#' # interact = TRUE gets errors for profile 1 in same group
#' #  and allows you to pass extra arguments to split.default()
#' splitLogicErrors(sp4, interact = TRUE, sep = "_", drop = TRUE)
#' 
splitLogicErrors <- function(object, interact = FALSE, ...) {
  
  # do check logic on logic for each profile
  f.logic <- checkHzDepthLogic(object)
  logicNames <- c("depthLogic", "sameDepth", "missingDepth", "overlapOrGap")
  names.idx <- match(logicNames, names(f.logic))
  
  # reformat errors
  f.errors <- do.call('rbind', apply(f.logic, 1, function(x) {
    as.data.frame(t(ifelse(as.logical(x[names.idx]), logicNames, "")))
  }))
  f.errors[] <- lapply(f.errors, factor)
  colnames(f.errors) <- logicNames
  
  if (interact == TRUE) {
    # interact == TRUE returns list elements based on interaction of f.errors
    f.goodbad <- split.default(object, f = f.errors, ...)
    f.goodbad <- lapply(f.goodbad, function(x) {
                                      if (length(x) == 0)
                                        return(NULL)
                                      return(x)
                                    })
  } else {
    # iterate over the four error types individually, ensuring no interaction
    f.goodbad <- lapply(f.errors, function(x) {
                                      res <- split(object, f = x)
                                      if (length(res) == 2)
                                        return(res[[2]])
                                      return(NULL)
                                    })
  }
  
  # return result as list of SPCs
  return(f.goodbad)
}
