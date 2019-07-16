

# simple function to convert horizon boundary distinctness codes into vertical (+/-) offsets
# based on "red book" version 3.0
hzDistinctnessCodeToOffset <- function(x, codes=c('A','C','G','D'), offset=c(0.5, 1.5, 5, 10)) {	
  x <- as.character(x)
  x.code <- match(x, codes)
  x.offset <- offset[x.code]
  x.offset <- ifelse(is.na(x.offset), 0, x.offset)
  return(x.offset)
}

# establish which elements within a vector of horizontal positions overlap beyond a given threshold
# x: vector of horizontal positions
# thresh: threshold
findOverlap <- function(x, thresh) {
  # all pair-wise distance
  d <- dist(x)
  m <- as.matrix(d)
  # diagonal isn't used here
  diag(m) <- NA
  # find matrix elements
  idx <- which(m < thresh)
  # use upper-triangle indexes to find elements in original vector
  col.idx <- col(m)[idx]
  # done
  return(col.idx)
}

# fix overlap via random perterbation of affected elements
# x: vector of horizontal positions
fixOverlap <- function(x, thresh=0.6, adj=0.1, min.x=0.8, max.x=length(x)+0.2, maxIter=1000, trace=FALSE) {
  
  # initial configuration
  ov <- findOverlap(x, thresh)
  
  # save original
  x.orig <- x
  
  # counter to prevent run-away while-loop
  i <- 1
  
  # short-circuit: only proceed if there is overlap
  if(length(ov) > 0) {
    
    # iterate...
    while(length(ov) > 0) {
      
      # fail-safe
      if(i > maxIter) {
        message('maximum number of iterations reached, using integer sequence')
        return(1:length(x))
      }
      
      # generate random perturbations to affected indices
      perturb <- runif(n = length(ov), min = adj * -1, max = adj)
      
      # attempt perturbation
      x.test <- x
      x.test[ov] <- x.test[ov] + perturb
      
      # enforce boundary conditions
      if(any(x.test < min.x) | any(x.test > max.x)) {
        # print('boundary condition')
        i <- i + 1
        next
      }
      
      # enforce rank ordering
      if(any(rank(x.orig) != rank(x.test))) {
        # print('rank violation')
        i <- i + 1
        next
      }
      
      # apply perturbation to working copy
      x <- x.test
      
      # eval overlap and try again
      ov <- findOverlap(x, thresh)
      i <- i + 1
    }
  }
  
  if(trace) {
    message(sprintf("%s iterations", i))
  }
  
  return(x)
}

