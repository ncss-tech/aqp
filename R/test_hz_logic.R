# this should be further documented
test_hz_logic <- function(i, topcol, bottomcol, test.NA=TRUE, strict=FALSE)
  {
  
  # test for na
  if(test.NA) { 
    if(any(c(is.na(i[[topcol]])), is.na(i[[bottomcol]]))) {
      res <- FALSE
      names(res) <- 'hz_logic_pass'
      return(res)
    }
  }
  
  # test for illogical horizon boundaries
  # note that this will fail with non-contiguous slices!
  if(strict) {
    n <- nrow(i)
    res <- all.equal(i[[topcol]][-1], i[[bottomcol]][-n])
    if(res != TRUE)
      res <- FALSE
    names(res) <- 'hz_logic_pass'
    return(res)
  }
  
  # PASSES for now
  else {
    res <- TRUE
    names(res) <- 'hz_logic_pass'
    return(res)
  }
}
