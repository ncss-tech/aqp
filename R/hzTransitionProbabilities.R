
# generate transition probability matrix from horizon designations
hzTransitionProbabilities <- function(x, name, loopTerminalStates=FALSE) {
  # get all horizons and split by profile
  h <- horizons(x)
  h.l <- split(h, h[[idname(x)]])
  # get all hz names
  hz.names <- sort(unique(h[[name]]))
  n.names <- length(hz.names)
  # get profile IDs and depth column names
  pIDs <- profile_id(x)
  dc <- horizonDepths(x)
  # init TP matrix with 0's
  m <- matrix(ncol=n.names, nrow=n.names, data = 0)
  # row / col names are entire set of hz names
  dimnames(m) <- list(hz.names, hz.names)
  
  # iterate over profiles
  for(i in pIDs) {
    # the current profile: hz names and top depths
    this.profile <- h.l[[i]][, c(name, dc[1])]
    # sort names by top depth, ascending order
    z <- this.profile[[name]][order(this.profile[, 2])]
    
    # remove NA
    z <- na.omit(z)
    
    # if all NA, then we are done with this iteration
    if(length(z) > 0) {
      # iterate over names
      for(j in 1:(length(z)-1)){
        # increment the current transition by 1
        m[z[j], z[j+1]] <- m[z[j], z[j+1]] + 1
      }
    }
  }
  
  # convert to probabilities by row
  m <- sweep(m, 1, rowSums(m), '/')
  
  # rows wih all NaN are terminal states: they transition to nothing
  # optionally, create a loop (A -> A)
  # this ensures compatibility with markovchain package
  if(loopTerminalStates) {
    loop.hz <- names(which(apply(m, 1, function(i) all(is.nan(i)))))
    # if there are some loops, then set the diagonal for these states to 1
    if(length(loop.hz) > 0) {
      if(length(loop.hz) < 2)
        m[loop.hz, loop.hz] <- 1 # single replacement, no diagonal
      else
        diag(m[loop.hz, loop.hz]) <- 1 # multiple replacements, use diagonal
    }
    
  }
  
  # replace NaN with 0
  m[which(is.nan(m))] <- 0
  
  return(m)
}
