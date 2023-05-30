

## TODO: this will be replaced by overlapMetrics()

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
#' @export
#'
#' @examples 
#' 
#' x <- c(1, 2, 3, 3.4, 3.5, 5, 6, 10)
#' 
#' overlapMetrics(x, thresh = 0.5)
#' 
#' 
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




#' @title Simulation of electrostatic force between two charged particles
#' @description This function computes a "force" (attraction or repulsion) between two charged "particles" (usually labels or other graphical elements), using a modification of the 1D electrostatic force equation. This function is used internally for label placement in the presence of overlap, as in [fixOverlap()].
#'
#' @param Q1 numeric, charge on particle 1 (e.g. a label)
#' @param Q2 numeric, charge on particle 2 (e.g. another label)
#' @param Qk numeric, empirical constant
#' @param d numeric, distance between particles
#' @param tiny numeric, number used to represent very small distances (avoid division by 0)
#' @param ex numeric, exponent used in distance-weighting
#' @param const numeric, constant used in distance weighting function, increase to dampen oscillation 
#' 
#' @return numeric
#'
#' @author D.E. Beaudette and K.C. Thompson
#' 
#' @noRd
#' 
#' @seealso [fixOverlap()]
#' 
#' @examples 
#' 
#' aqp:::.electricForce(Q1 = 1, Q2 = 1, Qk = 0.5, d = 5)
#'
.electricForce <- function(Q1, Q2, Qk, d, tiny = 0.0001, ex = 2, const = 0.25) {
  
  # fix for 0-distance where force is infinite
  d <- ifelse(d < tiny, tiny, d)
  
  # traditional definition of electrostatic force
  # (Qk * Q1 * Q2) / (d^ex + const)
  
  # modified version, c/o K.C. Thompson
  # increase const --> dampen ringing
  res <- (Qk * Q1 * Q2 ) / (d^ex + const)
  
  return(res)
}




#' @title Label placement based on a simulation of electrostatic forces
#'
#' @param x numeric vector, typically integers, ideally sorted, describing 1D label (particle) configuration
#' 
#' @param thresh numeric, overlap threshold, same as in [fixOverlap()]
#' 
#' @param q numeric, electrical charge
#' 
#' @param chargeDecayRate numeric, exponential decay rate constant for `q` as a function of iteration `i`
#' 
#' @param QkA_GrowthRate numeric, growth rate constant for `Qk` applied to attraction to uniform spacing of labels, invoked when rank order is violated during the simulation
#' 
#' @param maxIter integer, maximum number of iterations before giving up
#' 
#' @param tiny numeric, 0-values replaced by this number to avoid division by 0 and infinite forces
#' 
#' @param const numeric, empirical constant added to the 1D electrostatic force equation to dampen oscillation: `(Qk * Q1 * Q2) / (d^ex + const)`
#' 
#' @param trace logical, include diagnostic output
#' 
#' @param \dots not used, absorbs additional arguments to [fixOverlap()]
#' 
#' @author D.E. Beaudette and K.C. Thompson
#'
#' @return When `trace = TRUE` as `list`, otherwise numeric vector.
#' 
#' @export
#' 
#' @examples 
#' 
#' # vector of object locations, with potential overlap
#' x <- c(1, 2, 3, 3, 5, 6, 7, 8, 9, 10)
#' 
#' # full diagnostic output
#' z <- electroStatics_1D(x, thresh = 0.65, trace = TRUE, q = 1)
#' txt <- sprintf("Converged %s (%s iterations)", z$converged, length(z$cost))
#' 
#' plot(
#' seq_along(z$cost),
#' z$cost, 
#' las = 1, 
#' xlab = 'Iteration', 
#' ylab = 'Overlap Cost', 
#' type = 'b', 
#' main = txt
#' )
#' 
#' abline(h = 0, lty = 2, col = 2)
#' 
#' # final configuration only
#' xnew <- electroStatics_1D(x, thresh = 0.65, q = 1)
#' 
#' cbind(x, round(xnew, 2))
#' 

## TODO: what is a reasonable starting value for q?
##       -> too low, not enough perturbation, does not converge
##       -> too high, chaos

## TODO: re-scale to common [0,1] interval, perform adjustments, return to original scale

## TODO: preserve original ordering


electroStatics_1D <- function(x, thresh, q = 1, chargeDecayRate = 0.01, QkA_GrowthRate = 0.10, maxIter = 100, tiny = 0.0001, const = thresh * 2, trace = FALSE, ...) {
  
  # original configuration
  x.orig <- x
  x.n <- length(x)
  r <- range(x)
  
  # storage for new configurations / total force / const
  xnew <- list()
  F_total <- rep(NA_real_, times = maxIter)
  cost <- rep(NA_real_, times = maxIter)
  
  ## constants used to control effect of force on charged particles
  # mass
  .m <- 10
  
  # time step
  .t <- 1
  
  # initial attractive force constant (Qk) to uniform spacing
  .Fu <- 2
  
  # exponential decay schedule for particle charge
  # from t = 1 -> maxIter
  .q <- q * exp(-1 * chargeDecayRate * 1:maxIter)
  
  # simulation
  for(i in 1:maxIter) {
    
    # compute initial overlap metrics at threshold
    .om <- overlapMetrics(x, thresh = thresh)
    
    # constraints:
    # rank order
    .rank_test <- all(rank(x) == rank(x.orig))
    # overlap test
    .overlap_test <- length(.om$idx) < 1
    
    # stop simulation if there is nothing left to do
    if(.rank_test & .overlap_test) {
      xnew[[i]] <- x
      break
    }
    
    # if rank order is violated
    # progressively increase Qk for attractive force to uniform spacing
    # this will progressively pull particles into the "right" order
    if(!.rank_test) {
      .Fu <- .Fu + (.Fu * QkA_GrowthRate)
    }
    
    # pair-wise distances
    m <- as.matrix(dist(x))
    
    # remove distance to self
    diag(m) <- NA
    
    # repelling forces (same charge) between all particles
    .F <- .electricForce(Q1 = q, Q2 = q, Qk = 1, d = m, tiny = tiny, const = const)
    
    # negative forces are to the left
    # lower triangle is used for particles to the right of any given position
    .F[lower.tri(.F)] <- - .F[lower.tri(.F)]
    
    # net repelling force on each particle
    # force vector: negative <--- | ---> positive
    .F_repl <- colSums(.F, na.rm = TRUE)
    
    # attractive forces between each particle <--> uniform spacing, rank order
    # weaker than repelling forces
    
    # uniform spacing distances
    .offset <- seq(from = r[1], to = r[2], length.out = x.n)
    
    # direction is based on offset vector
    .direction <- sign(.offset)
    
    # force vector: negative <--- | ---> positive
    # weaker than repelling forces, set via Qk arg
    .F_attr <- .electricForce(Q1 = q, Q2 = q, Qk = .Fu, d = abs(.offset), tiny = tiny, const = const)
    
    # sum attractive + repelling forces
    .F_net <- (.direction * .F_attr) + .F_repl
    
    ## debugging
    # print(
    #   rbind(x, x.orig, .direction, .F_repl, .F_attr, .F_net)
    # )
    
    # displacement vector: d = 1/2 F/m * t
    # negative <--- | ---> positive
    .d <- 1/2 * (.F_net / .m) * .t^1
    
    # displacement is only applied to overlapping points
    .ignore <- setdiff(seq_along(x), .om$idx)
    .d[.ignore] <- 0
    
    # displacement of boundary points is always 0
    .d[1] <- 0
    .d[x.n] <- 0
    
    # keep track of new locations at time step i
    # displacement can't be outside of bounds
    xnew[[i]] <- pmin(pmax(x + .d, x.orig[1]), x.orig[x.n])
    
    # keep track of total force in system at time step i
    F_total[i] <- sum(abs(.F_net))
    
    # update locations
    x <- xnew[[i]]
    
    # exponential decay of all charges with each iteration
    q <- .q[i]
    
    # compute overlap metrics at threshold, after displacement
    .om <- overlapMetrics(x, thresh = thresh)
    cost[i] <- .om$ov
    
    # stop simulation if there is no overlap in this iteration
    # AND rank order is preserved
    if(all(rank(x) == rank(x.orig)) & length(.om$idx) < 1) {
      break
    }
    
  }
  
  # done with iterations
  message(sprintf("%s iterations", i))
  
  # flatten
  xnew <- do.call('rbind', xnew)
  
  ## TODO: optionally return to lowest cost configuration
  ## TODO: optionally re-run with lower / higher q
  
  ## TODO: return to original sorting order
  
  # check for convergence
  # 1. no overlap
  # 2. no change in rank order
  .converged <- all(rank(xnew[nrow(xnew), ]) == rank(x.orig) & length(.om$idx) < 1)
  
  if(trace) {
    # compile full results
    .res <- list(
      F_total = as.vector(na.omit(F_total)), 
      cost = as.vector(na.omit(cost)),
      xnew = xnew, 
      converged = .converged
    )
  } else {
    # only the final configuration
    .res <- as.vector(xnew[nrow(xnew), ])
  }
  
  
  return(.res)
}















## background:
# https://en.wikipedia.org/wiki/Simulated_annealing
# https://www.r-bloggers.com/2014/09/the-traveling-salesman-with-simulated-annealing-r-and-shiny/
# http://umsl.edu/~adhikarib/cs4130-fall2017/slides/11%20-%20The%20Simulated%20Annealing%20Algorithm.pdf
#

## TODO:
# * enforce rank in degenerate cases
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
#' @param x vector of horizontal positions, pre-sorted
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
#' @param \dots not used, absorbs additional arguments to [fixOverlap()]
#' 
#' @return When `trace = FALSE`, a vector of the same length as `x`, preserving rank-ordering and boundary conditions. When `trace = TRUE` a list containing the new sequence along with information about objective functions and decisions made during iteration.
#' 
#' @author D.E. Beaudette and K.C. Thompson
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
SANN_1D <- function(x, thresh = 0.6, adj = thresh * 2/3, min.x = min(x) - 0.2, max.x = max(x) + 0.2, maxIter = 1000, trace = FALSE, tiny = 0.0001, T0 = 500, k = 10, ...) {
  
  # system energy ~ probability ~ metropolis step
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
  } else {
    # best configuration only
    return(x)
  }
  
}





#' @title Fix Overlap within a Sequence
#'
#' @param x vector of initial positions, pre-sorted
#' @param thresh numeric, overlap threshold defined on the same scale as `x`
#' @param method character vector, 'S' for simulated annealing via [SANN_1D()] or 'E' for electrostatic simulation via [electroStatics_1D()]
#' @param trace logical, return full output
#' @param \dots additional arguments to [SANN_1D()] or [electroStatics_1D()]
#'
#' @return When `trace = FALSE`, a vector of the same length as `x`, preserving rank-ordering and boundary conditions. When `trace = TRUE` a list containing the new sequence along with information about objective functions and decisions made during adjustment of `x`.
#' 
#' @export
#'
#' @examples
#' 
#' s <- c(1, 2, 2.3, 4, 5, 5, 7)
#' 
#' # simulated annealing, solution is non-deterministic
#' fixOverlap(s, thresh = 0.6, method = 'S')
#' 
#' # electrostatics-inspired simulation of particles
#' # solution is deterministic
#' fixOverlap(s, thresh = 0.6, method = 'E')
#' 
fixOverlap <- function(x, thresh = 0.6, method = c('S', 'E'), trace = FALSE, ...) {
  
  # sanity checks on method
  method <- tolower(match.arg(method))
  
  .res <- switch(method, 
                 'e' = {
                   electroStatics_1D(x = x, thresh = thresh, trace = trace, ...) 
                 },
                 
                 's' = {
                   SANN_1D(x = x, thresh = thresh, trace = trace, ...) 
                 }
  )
  
  return(.res)
  
}





