
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
    test <- .hzTests(.top, .bottom)
    
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
  res[['valid']] <- ! apply(res[, -1], 1, any)
  
  return(res)
}

.hzTests <- function(top, bottom) {
  n <- length(top)
  
  # bottom depth < top depth?
  test.1 <- any(bottom < top, na.rm = TRUE)
  
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