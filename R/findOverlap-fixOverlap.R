
#' @title Find Overlap within a Sequence
#' @description Establish which elements within a vector of horizontal positions overlap beyond a given threshold
#'
#' @param x vector of relative horizontal positions, one for each profile
#' @param thresh threshold defining "overlap", typically < 1, ideal values likely in (0.3, 0.8)
#' 
#' @return Unique indices of affected (overlapping) elements in `x`
#' 
#' @note This is a very naive function and may fail to converge on a reasonable
#' solution. SANN would be a much more robust framework.
#' 
#' @export
#'
#' @examples 
#' 
#' x <- c(1, 2, 3, 3.4, 3.5, 5, 6, 10)
#' 
#' findOverlap(x, thresh = 0.5)
#'

## TODO: consider weighted overlap information, for more granular minimization
findOverlap <- function(x, thresh) {
  # all pair-wise distance
  d <- dist(x)
  m <- as.matrix(d)
  # diagonal isn't used here
  diag(m) <- NA
  # find matrix elements
  idx <- which(m < thresh)
  # use upper-triangle indexes to find elements in original vector
  # only uniquely affected elements
  col.idx <- unique(col(m)[idx])
  # done
  return(col.idx)
}




#' @title Fix Overlap within a Sequence
#' 
#' @description This function attempts to iteratively adjust a sequence until values are no longer within a given threshold of each other, or until `maxIter` is reached. Rank order and boundary conditions are preserved.
#' 
#' @param x vector of horizontal positions
#' 
#' @param thresh horizontal threshold defining "overlap" or distance between elements of `x`. For adjusting soil profile sketches values are typically < 1 and likely in (0.3, 0.8).
#' 
#' @param adj specifies the size of perturbations within `runif(min = adj * -1, max = adj)`. Larger values will sometimes reduce the number of iterations required to solve particularly difficult overlap conditions. See `coolingRate` argument when `adj` is large
#' 
#' @param min.x left-side boundary condition, consider expanding if a solution cannot be found within `maxIter`.
#' 
#' @param max.x right-side boundary condition, consider expanding if a solution cannot be found within `maxIter`.
#' 
#' @param coolingRate rate at which `adj` is decreased after a successful iteration (fewer overlapping elements in `x`)
#' 
#' @param maxIter maximum number of iterations to attempt before giving up and returning a regularly-spaced sequence
#' 
#' @param trace print diagnostics, result is a `list` vs `vector`
#' 
#' @return When `trace = FALSE`, a vector of the same length as `x`, preserving rank-ordering and boundary conditions. When `trace = TRUE` a list containing the new sequence along with the number of overlapping elements at each iteration.
#' 
#' @author D.E. Beaudette
#' @export
#'
#' @examples 
#' 
#' x <- c(1, 2, 3, 3.4, 3.5, 5, 6, 10)
#' 
#' # easy
#' fixOverlap(x, thresh = 0.2, trace = TRUE)
#'
#' # harder
#' fixOverlap(x, thresh = 0.6, trace = TRUE)
#' 
#' # much harder
#' fixOverlap(x, thresh = 0.9, trace = TRUE)
#'
fixOverlap <- function(x, thresh = 0.6, adj = thresh * 2/3, min.x = min(x) - 0.2, max.x = max(x) + 0.2, coolingRate = 0.95, maxIter = 1000, trace = FALSE) {
  
  # initial configuration
  ov <- findOverlap(x, thresh)
  n <- length(ov)
  
  # save original for testing rank order
  x.orig <- x
  
  # counter to prevent run-away while-loop
  i <- 1
  
  # keep track of number of overlaps
  stats <- rep(NA, times = maxIter)
  
  # original adjustment value
  adj.orig <- adj
  
  # short-circuit: only proceed if there is overlap
  if(n <  1) {
    return(x)
  }
    
  # iterate...
  while(n > 0) {
    
    # fail-safe
    if(i > maxIter) {
      message('maximum number of iterations reached, using regular sequence')
      s <- seq(from = min(x.orig), to = max(x.orig), length.out = length(x.orig))
      
      if(trace) {
        return(list(
          x = s,
          stats = as.vector(na.omit(stats))
        ))
      }
      return(s)
    }
    
    # generate random perturbations to affected indices
    perturb <- runif(n = length(ov), min = adj * -1, max = adj)
    
    # attempt perturbation
    x.test <- x
    x.test[ov] <- x.test[ov] + perturb
    
    
    ## TODO: this may make the algorithm worse off
    ## consider some cutoff like n + 2
    # 
    # # if the results are worse, then skip and try again
    # if(length(findOverlap(x.test, thresh)) > n + 1) {
    #   stats[i] <- n
    #   i <- i + 1
    #   next
    # }
    
    # enforce boundary conditions
    if(any(x.test < min.x) | any(x.test > max.x)) {
      # print('boundary condition')
      stats[i] <- n
      i <- i + 1
      next
    }
    
    # enforce rank ordering
    if(any(rank(x.orig) != rank(x.test))) {
      # print('rank violation')
      stats[i] <- n
      i <- i + 1
      next
    }
    
    
    # apply perturbation to working copy
    x <- x.test
    
    # save previous number of overlaps
    n.old <- n
    
    # eval overlap and try again
    ov <- findOverlap(x, thresh)
    
    # keep track of OF
    n <- length(ov)
    stats[i] <- n
    
    ## not sure if this helps
    # simulated annealing cooling parameter
    # reduce adj if there are fewer overlaps
    if(n < n.old) {
      # print('cooling!')
      adj <- adj * coolingRate
    }
    
    # re-heating: always helps in difficult problems
    if (n > n.old) {
      # print('heating!')
      adj <- adj.orig
    }
    
    # increment iteration counter
    i <- i + 1
  }
  
  
  message(sprintf("%s iterations", i))
  
  # full output
  if(trace) {
    return(list(
      x = x,
      stats = as.vector(na.omit(stats))
    ))
  }
  
  
  return(x)
}

