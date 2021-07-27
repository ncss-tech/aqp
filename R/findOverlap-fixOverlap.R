
#' @title Find Overlap within a Sequence
#' @description Establish which elements within a vector of horizontal positions overlap beyond a given threshold
#'
#' @param x vector of relative horizontal positions, one for each profile
#' @param thresh threshold defining "overlap", typically < 1
#' 
#' @return unique index to affected (overlapping) elements in `x`
#' 
#' @export
#'
#' @examples 
#' 
#' x <- c(1, 2, 3, 3.4, 3.5, 5, 6, 10)
#' 
#' findOverlap(x, thresh = 0.5)
#'
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
  
  return(col.idx)
}



## TODO: this will replace findOverlap()

#' @title Find and Quantify Overlap within a 1D Sequence
#' 
#' @description Desc.
#' 
#' @param x vector of relative horizontal positions, one for each profile
#' @param thresh threshold defining "overlap", typically < 1
#' 
#'  @return a `list`:
#'   * `idx`: unique index to overlapping elements in `x`
#'   * `ov`: normalized overlap (see details)
#'   
#' 
#' @export
#'
#' @examples 
#' 
#' x <- c(1, 2, 3, 3.4, 3.5, 5, 6, 10)
#' 
#' overlapMetrics(x, thresh = 0.5)
overlapMetrics <- function(x, thresh) {
  
  
  ## TODO: 
  # convert to diff(x) vs. dist(x)
  
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
  
  # overlap = (thresh - distance[i,j]) when d < thresh, otherwise overlap = 0
  # using full matrix, elements are mirrored over diagonal so divide by 2
  ov <- sum(thresh - m[idx]) / 2
  
  # normalize overlap by dividing by total possible overlap
  # all elements overlapping results in values > 1
  # ov_norm = ov / thresh * length(x)
  ov <- ov / (thresh * length(x))
  
  res <- list(
    idx = col.idx,
    ov = ov
  )
  
  return(res)
}





## TODO: document this as part of fixOverlap()
## system energy ~ probability ~ metropolis step
# energy / cost function
# these are all length-1 vectors
# n0: starting cost 
# n1: resulting cost adjustment i
# Te: temperature 
# k: cooling constant (empirically determined)
.P <- function(n0, n1, Te, k = 1) {
  if(n1 < n0) {
    return(1)
  } else {
    # delta-E: n1 - n0
    return(exp(-(n1 - n0) / Te * k))
  }
}



## background:
# https://en.wikipedia.org/wiki/Simulated_annealing
# https://www.r-bloggers.com/2014/09/the-traveling-salesman-with-simulated-annealing-r-and-shiny/
# http://umsl.edu/~adhikarib/cs4130-fall2017/slides/11%20-%20The%20Simulated%20Annealing%20Algorithm.pdf
#

## Ideas:
# * there is probably a LP solution to this in ~ 5 lines of code... (work wih Keith on this)
# * secondary objective function: as close as possible to original configuration
# * cleanup variable names: stats -> overlap, log -> x.log, etc.

#' @title Fix Overlap within a Sequence via Simulated Annealing
#' 
#' @description This function makes small adjustments to elements of `x` until overlap defined by `thresh` is removed, or until `maxIter` is reached. Rank order and boundary conditions (defined by `min.x` and `max.x`) are preserved. The underlying algorithm is based on simulated annealing. The "cooling schedule" parameters `T0` and `k` can be used to tune the algorithm for specific applications.
#' 
#' @details Ideas for solving difficult overlap scenarios:
#'   * widen the boundary conditions by adjusting `min.x` and `max.x` beyond the original scale of `x`
#'   * reduce the allowable overlap threshold `thresh`
#'   
#'   * reduce the magnitude of perturbations (`adj`) and increase `maxIter`
#'   
#'   * increase `k`
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
#' @param maxIter maximum number of iterations to attempt before giving up and returning a regularly-spaced sequence
#' 
#' @param trace print diagnostics, result is a `list` vs `vector`
#' 
#' @param tiny the smallest allowable overlap
#' 
#' @param T0 starting temperature
#' 
#' @param k cooling constant
#' 
#' @return When `trace = FALSE`, a vector of the same length as `x`, preserving rank-ordering and boundary conditions. When `trace = TRUE` a list containing the new sequence along with information about objective functions and decisions made during iteration.
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
#'
#' # interpret `trace` output
#' 
#' # relatively challenging
#' x <- c(1, 2, 3.4, 3.4, 3.4, 3.4, 6, 8, 10, 12, 13, 13, 15, 15.5)
#' 
#' # fix overlap, return debugging information
#' set.seed(10101)
#' z <- fixOverlap(x, thresh = 0.8, trace = TRUE)
#' 
#' # setup plot device
#' par(mar = c(4, 4, 1, 1))
#' layout(matrix(c(1,2,3)), widths = 1, heights = c(1,1,2))
#' 
#' # objective function = overlap + SSD
#' plot(
#'   seq_along(z$stats), z$stats, 
#'   type = 'h', las = 1,
#'   xlab = 'Iteration', ylab = 'Overlap',
#'   cex.axis = 0.8
#' )
#' 
#' # SSD: deviation from original configuration 
#' plot(
#'   seq_along(z$ssd), z$ssd, 
#'   type = 'h', las = 1,
#'   xlab = 'Iteration', ylab = 'Deviation',
#'   cex.axis = 0.8
#' )
#' # adjustments at each iteration
#' matplot(
#'   z$states, type = 'l', 
#'   lty = 1, las = 1, 
#'   xlab = 'Iteration', ylab = 'x-position'
#' )
#' 
#' # trace log
#' # B: boundary condition violation
#' # O: rank (order) violation
#' # +: accepted perturbation
#' # -: rejected perturbation
#' table(z$log)
#' 
fixOverlap <- function(x, thresh = 0.6, adj = thresh * 2/3, min.x = min(x) - 0.2, max.x = max(x) + 0.2, maxIter = 1000, trace = FALSE, tiny = 0.0001, T0 = 500, k = 10) {
  
  
  # sanity check: cannot have perfect overlap (duplicates) in the initial configuration
  # jitter duplicates will resolve the problem
  if(any(table(x) > 1)) {
    x <- jitter(x)
    if(trace) {
      message('duplicates in `x`, applying jitter')
    }
  }
  
  ## TODO: consider a cost related to the preservation of "character", relative clustering
  # initial value = 0
  # deviation <- 1 - cor(dist(x), dist(x.test), method = 'spearman')
  
  ## TODO: this may not actually be a good "cost" metric
  # SSD: sum squared differences between two configurations (via distance matrix)
  # worst-possible SSD due to regular sequence
  ssd.max <- sum((dist(x) - dist(seq(from = 1, to = length(x), by = 1)))^2)
  
  # initial configuration
  m <- overlapMetrics(x, thresh)
  
  # initial cost
  # at this point SSD = 0
  cost <- m$ov
  
  # save original for testing rank order
  x.orig <- x
  
  # original adjustment value
  adj.orig <- adj
  
  # counter to prevent run-away while-loop
  i <- 1
  
  ## trace details
  # overlap cost (total overlap)
  stats <- rep(NA, times = maxIter)
  
  # deviation from original configuration
  # sum of squared differences between dist(x.orig), dist(x.new)
  ssd <- rep(NA, times = maxIter)
  
  # algorithm adjustment steps:
  # B: boundary violation
  # O: ordering (rank) violation
  # +: accept adjustments
  # -: reject adjustments
  log <- rep(NA, times = maxIter)
  
  # states
  states <- matrix(data = NA, nrow = maxIter, ncol = length(x))
  
  # short-circuit: only proceed if there is overlap
  if(m$ov <  tiny) {
    return(x)
  }
    
  # continue while total overlap > small number
  while(m$ov > tiny) {
    
    # fail-safe
    if(i > maxIter) {
      message('maximum number of iterations reached, using regular sequence')
      s <- seq(from = min(x.orig), to = max(x.orig), length.out = length(x.orig))
      
      if(trace) {
        
        log <- factor(as.vector(na.omit(log)), levels = c('B', 'O', '+', '-'))
        stats <- as.vector(na.omit(stats))
        ssd <- as.vector(na.omit(ssd))
        
        states <- na.omit(states)
        attr(states, "na.action") <- NULL
        
        return(list(
          x = s,
          stats = stats,
          ssd = ssd,
          log = log,
          converged = FALSE,
          states = states
        ))
      }
      return(s)
    }
    
    # generate random perturbations to affected indices
    perturb <- runif(n = length(m$idx), min = adj * -1, max = adj)
    
    # attempt perturbation
    x.test <- x
    x.test[m$idx] <- x.test[m$idx] + perturb
    
    # re-evaluate metrics
    m.test <- overlapMetrics(x.test, thresh)
    
    ## TODO: consider correlation vs. SSD
    # deviation <- 1 - cor(dist(x), dist(x.test), method = 'spearman')
    
    # SSD
    ssd.test <- sum((dist(x) - dist(x.test))^2)
    # normalize
    ssd.test <- ssd.test / ssd.max
    
    # combined cost: overlap + SSD
    cost.test <- m.test$ov + ssd.test
    
    # enforce boundary conditions
    if(any(x.test < min.x) | any(x.test > max.x)) {
      # print('boundary condition')
      log[i] <- 'B'
      stats[i] <- m.test$ov
      ssd[i] <- ssd.test
      states[i, ] <- x.test
      i <- i + 1
      next
    }
    
    # enforce rank ordering
    if(any(rank(x.orig) != rank(x.test))) {
      # print('rank violation')
      log[i] <- 'O'
      stats[i] <- m.test$ov
      ssd[i] <- ssd.test
      states[i, ] <- x.test
      i <- i + 1
      next
    }
    
    
    # compute current temperature
    # differs from literature where i starts from 0
    Temp <- T0 / i
    
    # acceptance probability
    # n0 = previous cost
    # n1 = current cost
    # Te = current temperature
    # k = cooling constant
    p <- .P(n0 = cost, n1 = cost.test, Te = Temp, k = k)
    
    # accept a more costly proposition if randomly selected
    p.acc <- p > runif(n = 1, min = 0, max = 1)
    
    if( (cost.test < cost) | p.acc) {
      # keep new state
      log[i] <- '+'
      
      # apply perturbation to working copy
      x <- x.test
      
      # save state
      states[i, ] <- x
      
      # keep track of overlap cost
      stats[i] <- m.test$ov
      
      # SSD
      ssd[i] <- ssd.test
      
      # re-evaluate overlap for while() loop
      m <- overlapMetrics(x, thresh)
      
      # re-compute total cost
      cost <- m$ov + ssd.test
      
      # increment iteration counter
      i <- i + 1
    } else {
      # reject proposed state
      log[i] <- '-'
      
      # save state
      states[i, ] <- x.test
      
      # keep track of overlap cost
      stats[i] <- m.test$ov
      
      # SSD
      ssd[i] <- ssd.test
      
      i <- i + 1
      next
    }
    
    
  }
  
  
  # done with iterations
  message(sprintf("%s iterations", i))
  
  # full output
  if(trace) {
    
    log <- factor(as.vector(na.omit(log)), levels = c('B', 'O', '+', '-'))
    stats <- as.vector(na.omit(stats))
    ssd <- as.vector(na.omit(ssd))
    
    states <- na.omit(states)
    attr(states, "na.action") <- NULL
    
    return(list(
      x = x,
      stats = stats,
      ssd = ssd,
      log = log,
      converged = TRUE,
      states = states
    ))
  }
  
  
  return(x)
}

