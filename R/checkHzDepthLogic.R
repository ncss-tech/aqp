
## related issues:
# ## https://github.com/ncss-tech/aqp/issues/65

## general-purpose hz depth logic check
# assumes that data are sorted ID, top ASC
# x: SoilProfileCollection object to check
checkHzDepthLogic <- function(x) {
  
  # used inside / outside of scope of .check()
  htb <- horizonDepths(x)
  idn <- idname(x)
  
  .check <- function(i) {
    # extract pieces
    h <- horizons(i)
    
    # convenience vars
    ID.i <- h[[idn]][1]
    .top <- h[[htb[1]]]
    .bottom <- h[[htb[2]]]
    
    # hzTests takes two numeric vectors and returns named logical
    test <- hzDepthTests(.top, .bottom)
    
    # pack into DF, 1 row per profile 
    res <- data.frame(
      .id=ID.i,
      depthLogic=test[1], 
      sameDepth=test[2], 
      missingDepth=test[3],
      overlapOrGap=test[4],
      stringsAsFactors = FALSE
    )
    
    # re-name .id -> idname(x)
    names(res)[1] <- idn
    
    return(res)
  }
  
  # iterate over profiles, result is safely packed into a DF ready for splicing into @site
  res <- profileApply(x, .check, simplify = FALSE, frameify = TRUE)
  
  # add 'valid' flag for simple filtering
  res[['valid']] <- !apply(res[, -1], 1, any)
  
  return(res)
}

#' @title Tests of horizon depth logic
#' 
#' @description Function used internally by `checkHzDepthLogic()`, `glom()` and various other functions that operate on horizon data from single soil profiles and require a priori depth logic checks. Checks for bottom depths less than top depth / bad top depth order ("depthLogic"), bottom depths equal to top depth ("sameDepth"), overlaps/gaps ("overlapOrGap") and missing depths ("missingDepth"). Use `names(res)[res]` on result `res` of `hzDepthTest()` to to determine type of logic error(s) found -- see examples below. 
#' 
#' @param top A numeric vector containing horizon top depths.
#' @param bottom A numeric vector containing horizon bottom depths.
#' 
#' @return A named logical vector containing TRUE for each type of horizon logic error found in the given data. 
#' @author Andrew G. Brown & Dylan E. Beaudette
#' @examples 
#' 
#' # no logic errors
#' res <- hzDepthTests(top = c(0,10,20,30), bottom = c(10,20,30,50))
#' names(res)[res]
#' 
#' # bottom < top
#' hzDepthTests(top = c(10,20,30,50), bottom = c(0,10,20,30))
#' names(res)[res]
#' 
#' # bottom == top
#' hzDepthTests(top = c(10,20,30,50), bottom = c(0,20,20,30))
#' names(res)[res]
#' 
#' # overlap
#' hzDepthTests(top = c(0,5,20,30), bottom = c(10,20,30,50))
#' names(res)[res]
#' 
#' # gap
#' hzDepthTests(top = c(0,15,20,30), bottom = c(10,20,30,50))
#' names(res)[res]
#' 
#' # missing
#' hzDepthTests(c(0,15,NA,30),c(10,NA,30,50))
#' names(res)[res]
#' 
#' @rdname hzDepthTests
#' @export hzDepthTests
hzDepthTests <- function(top, bottom) {
  n <- length(top)
  
  # sanity checks, since this will be exported provide a little checking
  #   for most internal usesF these errors will never trigger...
  # but in case of corrupted hz data or bad inputs... anything can happen
  if (length(top) != length(bottom)) {
    stop("cannot evaluate horizon depth logic: vectors do not have same length")
  }
  
  # bottom depth < top depth? or horizons not in top-depth order?
  test.1 <- any(bottom < top, na.rm = TRUE) | any(suppressWarnings(sort(top) != top))
  
  # bottom depth == top depth
  test.2 <- any(top == bottom, na.rm = TRUE)
  
  # NA depths
  test.3 <- any(is.na(top) | is.na(bottom), na.rm = TRUE)
  
  # bottom != next top
  test.4 <- any(bottom[-n] != top[-1], na.rm = TRUE)
  
  res <- as.logical(c(test.1, test.2, test.3, test.4))
  names(res) <- c("depthLogic","sameDepth","missingDepth","overlapOrGap")
  return(res)
}
