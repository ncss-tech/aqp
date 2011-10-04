


test_hz_logic <- function(i, topcol, bottomcol)
  {
  # test for na
  if(any(c(is.na(i[[topcol]])), is.na(i[[bottomcol]])))
    return(FALSE)
    
  # test hz logic
  n <- nrow(i)
  res <- all.equal(i[[topcol]][-1], i[[bottomcol]][-n])
  if(res != TRUE)
    res <- FALSE
  
  return(res)
  }


