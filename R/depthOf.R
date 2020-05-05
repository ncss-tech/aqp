#depthOf.R
# these are helper functions to calculate min/maximum depth of occurence of a specified horizon designation pattern

#  it is used primarily in the place of functions that are capable of reasoning over taxonomic criteria for things
#  like calcic, spodic, natric horizons

# maxDepthOf is a wrapper around depthOf to return a single, maximum value
maxDepthOf <- function(p, pattern, top = TRUE, hzdesgn = guessHzDesgnName(p), 
                         no.contact.depth = 0, 
                         no.contact.assigned = NA, na.rm = TRUE) {
  
  # depthOf returns all top or bottom depths of horizons matthing `hzdesgn`
  res <- depthOf(p, pattern, top, hzdesgn, no.contact.depth, no.contact.assigned)
  
  # otherwise, return the maximum value from result
  res2 <- suppressWarnings(max(res, na.rm = na.rm))
  
  # if not found, depth is infinite
  if(is.infinite(res2)) {
    return(no.contact.assigned)
  }
  
  return(res2)
}

# minDepthOf is a wrapper around depthOf to return a single, minimum value
minDepthOf <- function(p, pattern, top = TRUE, hzdesgn = guessHzDesgnName(p), 
                         no.contact.depth = 0, 
                         no.contact.assigned = NA, na.rm = TRUE) {
  
  # depthOf returns all top or bottom depths of horizons matthing `hzdesgn`
  res <- depthOf(p, pattern, top, hzdesgn, no.contact.depth, no.contact.assigned)  
  
  # otherwise, return the minimum value from result
  res2 <- suppressWarnings(min(res, na.rm = na.rm))
  
  # if not found, depth is infinite
  if(is.infinite(res2)) {
    return(no.contact.assigned)
  }
  
  return(res2)
}

# depthOf returns all top or bottom depths of horizons matthing `hzdesgn`
depthOf <- function(p, pattern, top = TRUE, hzdesgn = guessHzDesgnName(p), 
                     no.contact.depth = 0, no.contact.assigned = NA) {
  
  # get horizons matching designation pattern
  hz.match <- horizons(p)[grepl(pattern, p[[hzdesgn]]),]
  
  # if no horizons match, return `no.contact.assigned`
  if(nrow(hz.match) == 0) {
    return(no.contact.assigned)
  }
  
  # get top or bottom depth, based on `top` argument
  res <- hz.match[[horizonDepths(p)[ifelse(top, 1, 2)]]]
  
  # remove results greater than the cutoff depth: `no.contact.depth`
  if(any(res > no.contact.depth)) {
    res <- res[-which(res > no.contact.depth)]
  }
  
  # if there are non-NA results, return all of them
  if(length(res) > 0 & any(!is.na(res))) {
    return(res)
  }
  
  # otherwise:
  return(no.contact.assigned)
}
