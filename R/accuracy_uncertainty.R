
## TODO: test that sum(x) == 1
# Shannon Entropy
# x: vector of probabilities {0,1}, must sum to 1, should not contain NA
# b: logarithm base
# when b = length(x) result is the normalized Shannon entropy (Kempen et al, 2009)
shannonEntropy <- function(x, b=2) {
  # 0s automatically removed by na.rm=TRUE (0 * log(0) = Nan)
  res <- -1 * sum(x * log(x, base=b), na.rm=TRUE)
  return(res)
}

# confusion index (Burrough et al., 1997)
# x: vector of probabilities {0,1}, should not contain NA
confusionIndex <- function(x) {
  x <- sort(x, decreasing = TRUE)
  res <- 1 - (x[1] - x[2])
  return(res)
}

# multinominal Brier score
# x: data.frame, rows are predictions/observations, columns contain classes
# classLabels: vector of class labels, corrosponding to column names in x.i
# actual: name of column containing the observed class
brierScore <- function(x, classLabels, actual='actual') {
  # number of observations
  n <- nrow(x)
  
  # extract vector of observed classes
  x.actual <- x[[actual]]
  
  # keep only probabilities as matrix
  x <- as.matrix(x[, classLabels, drop=FALSE])
  
  # init new matrix to store most-likely class
  m <- matrix(0, ncol=ncol(x), nrow=n)
  # same structure as x.pr
  dimnames(m)[[2]] <- classLabels
  
  # set cells of actual observed outcome to 1
  for(i in 1:n) {
    x.i <- x.actual[i]
    m[i, x.i] <- 1
  }
  
  # compute multinominal brier score
  # 1/n * sum((x - m)^2)
  # x: matrix of predictions
  # m: indicator matrix of outcomes
  bs <- (1/n) * sum((x - m)^2, na.rm=TRUE)
  return(bs)
}

