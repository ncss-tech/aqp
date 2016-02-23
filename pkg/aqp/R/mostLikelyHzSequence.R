## requires the markovchain library
## not imported, so list in suggests and address function names with markovchain::

# what is the most likely sequence, given a markovchain and initial state
# the result isn't likely correct when there are non-zero ties in tp
mostLikelyHzSequence <- function(mc, t0, maxIterations=10) {
  
  if(!requireNamespace('markovchain'))
    stop('pleast install the `markovchain` package.', call.=FALSE)
  
  # check for ties
  if(!is.null(attr(as(mc, 'matrix'), 'ties')))
    if(attr(as(mc, 'matrix'), 'ties'))
      warning('ties in transition probability matrix, results may not be reliable', call. = FALSE)
  
  # store sequence here
  s <- vector(mode = 'character')
  # save the intial state
  i <- 1
  s[i] <- t0
  # compute probabilities for the next state
  cd <- markovchain::conditionalDistribution(mc, t0)
  
  # search for all but the current state 
  not.this.state <- which(!names(cd) == t0)
  
  ## TODO: this doesn't work when there are ties
  next.state <- names(which.max(cd[not.this.state]))
  i <- i + 1
  s[i] <- next.state
  # continue searching for the most likely next state
  # until reaching the second absorbing state
  # the first absorbing state is retained in 's'
  while(! next.state %in% markovchain::absorbingStates(mc)) {
    cd <- markovchain::conditionalDistribution(mc, next.state)
    # search for all but the current state 
    not.this.state <- which(!names(cd) == next.state)
    next.state <- names(which.max(cd[not.this.state]))
    i <- i + 1
    s[i] <- next.state
    # trap run-away iteration
    if(i > maxIterations)
      break
  }
  return(s)
}

