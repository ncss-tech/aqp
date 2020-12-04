library(data.table)
library(aqp, warn.conflicts = FALSE)

data(sp4)
depths(sp4) <- id ~ top + bottom

aqp_df_class(sp4) <- "data.table"
sp4 <- rebuildSPC(sp4)

# raw data.table; just validity boolean
f1 <-  function() horizons(sp4)[, all(!hzDepthTests(top, bottom)), by = id]$V1 

# current aqp implementation
f2 <-  function() checkHzDepthLogic(sp4)$valid 

# fastish check of all logic; 
#   fast = TRUE: no individual test results;
#   fast = FALSE: same as checkHzDepthLogic output
f3 <-  function(fast = TRUE) checkAllLogic(sp4, fast)$valid 

#' Fast checks of horizon depth logic in entire SoilProfileCollection
#' @param object A SoilProfileCollection
#' @param fast If details about specific test results are not needed, the operation can run approximately 5x faster. Default: TRUE.
#'
#' @return A data.table containing profile IDs, validity boolean and test results if \code{fast = FALSE}
#' @export
checkAllLogic <- function(object, fast = TRUE) {
  h <- horizons(object)
  hzd <- horizonDepths(object)
  
  hby <- substitute(idname(object))
  top <- substitute(hzd[1])
  bottom <- substitute(hzd[2])
  
  if (!fast) {
    res <- h[, .(tests = list(tests = data.frame(t(hzDepthTests(eval(top), eval(bottom)))))), by = c(eval(hby))][ 
             , .(tests = tests, valid = all(!tests[[1]])), by = c(eval(hby))]
    res <- cbind(res, rbindlist(res$tests))
    res$tests <- NULL
    return(res)
  } else {
    res <- h[, all(!hzDepthTests(eval(top), eval(bottom))), by = c(eval(hby))]
    colnames(res) <- c(idname(object), "valid")
    return(res)
  }
}

checkAllLogic(sp4)

bench::mark(x <- f1(),             # raw data.table
            x <- f2(),             # checkHzDepthLogic
            x <- f3(),             # checkAllLogic(fast=TRUE)
            x <- f3(fast = FALSE)) # checkAllLogic(fast=FALSE)
