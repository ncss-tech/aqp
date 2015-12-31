## requires the markovchain library
## not imported, so list in suggests and address function names with markovchain::

# what is the most likely sequence, given a markovchain and initial state
mostLikelyHzSequence <- function(mc, t0) {
  
  if(!requireNamespace('markovchain'))
    stop('pleast install the `markovchain` package.', call.=FALSE)
  
  # store sequence here
  s <- vector(mode = 'character')
  # save the intial state
  i <- 1
  s[i] <- t0
  # compute probabilities for the next state
  cd <- markovchain::conditionalDistribution(mc, t0)
  next.state <- names(which.max(cd))
  i <- i + 1
  # continue searching for the most likely next state
  # until reaching the second absorbing state
  # the first absorbing state is retained in 's'
  while(! next.state %in% markovchain::absorbingStates(mc)) {
    s[i] <- next.state
    cd <- markovchain::conditionalDistribution(mc, next.state)
    next.state <- names(which.max(cd))
    i <- i + 1
    s[i] <- next.state
  }
  return(s)
}

