
# a very basic function for testing horizon logic
# no NA boundaries allowed
test_hz_logic <- function(i)
  {
  # test for na
  if(any(c(is.na(i$top)), is.na(i$bottom)))
    return(FALSE)
    
  # test hz logic
  n <- nrow(i)
  res <- all.equal(i$top[-1], i$bottom[-n])
  if(res != TRUE)
    res <- FALSE
  
  return(res)
  }


