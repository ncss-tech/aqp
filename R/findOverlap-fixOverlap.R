
#' @title Find Overlap within a Sequence
#' @description Establish which elements within a vector of horizontal positions overlap beyond a given threshold
#'
#' @param x vector of relative horizontal positions, one for each profile
#' @param thresh threshold defining "overlap", typically < 1, ideal values likely in (0.3, 0.8)
#' 
#' @return Unique indices of affected (overlapping) elements in `x`
#' 
#' @export
#'
#' @examples 
#' 
#' x <- c(1, 2, 3, 3.4, 3.5, 5, 6, 10)
#' 
#' findOverlap(x, thresh = 0.5)
#'

## TODO: consider weighted overlap information, for more granular minimization:
# this would be a new function overlapMetrics()

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



# possible energy / cost function
# these are all length-1 vectors
# n0: starting number of overlaps 
# n1: resulting overlaps after adjustment i
# Te: temperature (decreases over time)
# k: cooling constant
.P <- function(n0, n1, Te, k = 1) {
  if(n1 < n0) {
    return(1)
  } else {
    # delta-E: n1 - n0
    return(exp(-(n1 - n0) / Te * k))
  }
}

# safe vectorization
.P <- Vectorize(.P)


## background:
# https://en.wikipedia.org/wiki/Simulated_annealing
# https://www.r-bloggers.com/2014/09/the-traveling-salesman-with-simulated-annealing-r-and-shiny/
# http://umsl.edu/~adhikarib/cs4130-fall2017/slides/11%20-%20The%20Simulated%20Annealing%20Algorithm.pdf
#

## Ideas:
# * there is probably a LP solution to this in ~ 5 lines of code...
# * dual-energy / cost function: number of overlaps + distance from original config
# * adjusting `thresh` with time
# * keep track of states, so that the "best" state becomes the restart point (EVAL)
# * exponential cost function 
# * refactor around T0 -> cooling rate + maxIter 
# * re-name `coolingRate`, this is no longer appropriate

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
#' @param restartRate optimization is restarted when an iteration results in `length(findOverlap(x.i, thresh)) > restartRate`
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
#' z <- fixOverlap(x, thresh = 0.2, trace = TRUE)
#'
#' # harder
#' z <- fixOverlap(x, thresh = 0.6, trace = TRUE)
#' 
#' # much harder
#' z <- fixOverlap(x, thresh = 0.9, trace = TRUE)
#'
fixOverlap <- function(x, thresh = 0.6, adj = thresh * 2/3, min.x = min(x) - 0.2, max.x = max(x) + 0.2, coolingRate = 0.95, restartRate = 2.5, maxIter = 1000, trace = FALSE) {
  
  # initial configuration
  ov <- findOverlap(x, thresh)
  n <- length(ov)
  
  # save original for testing rank order
  x.orig <- x
  # original cost
  n.orig <- n
  # original adjustment value
  adj.orig <- adj
  
  # counter to prevent run-away while-loop
  i <- 1
  
  ## trace details
  # number of overlaps "cost"
  stats <- rep(NA, times = maxIter)
  
  # algorithm adjustment steps:
  # R: restart
  # B: boundary violation
  # O: ordering (rank) violation
  # C: cooling
  # H: heating
  log <- rep(NA, times = maxIter)
  
  # states
  states <- matrix(data = NA, nrow = maxIter, ncol = length(x))
  
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
        
        log <- factor(as.vector(na.omit(log)), levels = c('R', 'B', 'O', 'C', 'H'))
        stats <- as.vector(na.omit(stats))
        
        return(list(
          x = s,
          stats = stats,
          log = log,
          converged = FALSE
        ))
      }
      return(s)
    }
    
    # generate random perturbations to affected indices
    perturb <- runif(n = length(ov), min = adj * -1, max = adj)
    
    # attempt perturbation
    x.test <- x
    x.test[ov] <- x.test[ov] + perturb
    
    
    ## TODO: fully evaluate re-starts, consider resetting iteration counter
    ## TODO: `n` should be re-calculated around here
    
    ## `restartRate` empirically determined:
    # too low: algorithm fails to converge on difficult problems
    # too high: restarts don't happen
    if(length(findOverlap(x.test, thresh)) > (restartRate * n.orig)) {
      
      # restart at original configuration
      # x <- x.orig
      
      # restart at last-best state
      x <- states[which.min(stats), ]
      
      # restart at original adjustment rate
      adj <- adj.orig
      
      # keep track and move on
      log[i] <- 'R'
      stats[i] <- n
      
      i <- i + 1
      next
    }
    
    # enforce boundary conditions
    if(any(x.test < min.x) | any(x.test > max.x)) {
      # print('boundary condition')
      log[i] <- 'B'
      stats[i] <- n
      i <- i + 1
      next
    }
    
    # enforce rank ordering
    if(any(rank(x.orig) != rank(x.test))) {
      # print('rank violation')
      log[i] <- 'O'
      stats[i] <- n
      i <- i + 1
      next
    }
    
    
    # apply perturbation to working copy
    x <- x.test
    
    # save state
    states[i, ] <- x
    
    # save previous number of overlaps
    n.old <- n
    
    # eval overlap and try again
    ov <- findOverlap(x, thresh)
    
    # keep track of OF
    n <- length(ov)
    stats[i] <- n
    
    ## not sure if this helps
    # reduce adj if there are fewer overlaps
    if(n < n.old) {
      # print('cooling!')
      log[i] <- 'C'
      adj <- adj * coolingRate
    }
    
    # re-heating: always helps in difficult problems
    if (n > n.old) {
      # print('heating!')
      log[i] <- 'H'
      adj <- adj.orig
    }
    
    ## TODO: eval SANN framework
    # print(.P(n0 = n.old, n1 = n, Te = maxIter - i, k = 10))
    
    # increment iteration counter
    i <- i + 1
  }
  
  
  # done with iterations
  
  message(sprintf("%s iterations", i))
  
  # full output
  if(trace) {
    
    log <- factor(as.vector(na.omit(log)), levels = c('R', 'B', 'O', 'C', 'H'))
    stats <- as.vector(na.omit(stats))
    
    ## finish: ensure row-ordering of states
    states <- na.omit(states)
    attr(states, "na.action") <- NULL
    
    return(list(
      x = x,
      stats = stats,
      log = log,
      converged = TRUE,
      states = states
    ))
  }
  
  
  return(x)
}

