library(aqp)



x <- c(1, 2, 3, 3.4, 3.5, 5, 6, 10)
s <- seq(from = 0.1, to = 2, by = 0.1)

of <- function(i, thresh) {
  length(findOverlap(i, thresh = thresh))
}

res <- sapply(s, function(i) {
  of(x, i)
})

plot(s, res, type = 'b', las = 1, xlab = 'Threshold', ylab = 'Number Affected Elements')


findOverlap(x, thresh = 0.2)

findOverlap(x, thresh = 0.5)


fixOverlap(x, thresh = 1)


## TODO: use of optim(... method = 'SANN') or optimise()
##
## constraints:
##  * enforce boundary conditions
##  * enforce rank ordering
##  * minimize of()


of <- function(x, par, thresh, min.x, max.x) {
  
  ov <- findOverlap(x, thresh)
  
  # zero-out non-affected elements?
  par[-ov] <- 0
  
  ## TODO: how to determine which parameters need to be modified (usually not all)
  x.test <- x
  x.test[ov] <- x[ov] + par[ov]
  
  # enforce boundary conditions
  if(any(x.test < min.x) | any(x.test > max.x)) {
    # print('boundary condition')
    return(100)
  }
  
  # enforce rank ordering
  if(any(rank(x) != rank(x.test))) {
    # print('rank violation')
    return(100)
  }
  
  # this is the main objective: minimize num affected elements
  res <- length(findOverlap(x.test, thresh = thresh))
  
  # or?
  res <- sum(par)
  
  return(res)
}

## this is not right
o <- optim(
  fn = of,
  par = rep(0.5, times = length(x)),
  method = 'SANN',
  x = x,
  thresh = 0.5,
  min.x = 1, 
  max.x = 10
)

o




