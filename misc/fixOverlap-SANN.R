library(aqp)
library(lattice)
library(viridis)



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


.P(n0 = 3, n1 = 10, Te = 1)


n0 <- 3
n1 <- 10
Te <- 100:1
plot(Te, .P(n0, n1, Te), type = 'l', xlim = c(100, 0), las = 1)

g <- expand.grid(n0 = 5, n1 = 5:10, Te = Te)

g$P <- .P(n0 = g$n0, n1 = g$n1, Te = g$Te)

hist(g$P, breaks = 30, las = 1)

levelplot(P ~ Te * n1, data = g, col.regions = viridis, contour = TRUE, xlim = c(105, -5))

xyplot(P ~ Te, groups = factor(n1), data = g, type = 'l', as.table = TRUE, auto.key = list(lines = TRUE, points = FALSE, space = 'right'), par.settings = tactile::tactile.theme(), xlim = c(105, -5))


x <- c(1, 2, 3, 3.4, 3.5, 5, 6, 10)
s <- seq(from = 0.1, to = 2, by = 0.1)

of <- function(i, thresh) {
  length(findOverlap(i, thresh = thresh))
}

res <- sapply(s, function(i) {
  of(x, i)
})

plot(s, res, type = 'b', las = 1, xlab = 'Threshold', ylab = 'Number Affected Elements')

# simple
fixOverlap(x, thresh = 0.2, trace = TRUE)

# nearly impossible
length(findOverlap(x, thresh = 1))
z <- fixOverlap(x, thresh = 1, trace = TRUE)
plot(seq_along(z$stats), z$stats, type = 'h', las = 1)
table(z$log)


f1 <- function(i) {
  o <- fixOverlap(x, thresh = 1, trace = TRUE, restartRate = 5)
  return(o$converged)
}

f2 <- function(i) {
  o <- fixOverlap(x, thresh = 1, trace = TRUE, restartRate = 2)
  return(o$converged)
}

m1 <- sapply(1:100, FUN = f1)
m2 <- sapply(1:100, FUN = f2)

table(m1)
table(m2)




set.seed(10101)
z <- fixOverlap(x, thresh = 0.6, coolingRate = 0.85, trace = TRUE)
plot(seq_along(z$stats), z$stats, type = 'h', las = 1)
table(z$log)

set.seed(10101)
z <- fixOverlap(x, thresh = 0.6, coolingRate = 0.95, trace = TRUE)
plot(seq_along(z$stats), z$stats, type = 'h', las = 1)
table(z$log)

set.seed(10101)
z <- fixOverlap(x, thresh = 0.6, coolingRate = 1, trace = TRUE)
plot(seq_along(z$stats), z$stats, type = 'h', las = 1)
table(z$log)

set.seed(10101)
z <- fixOverlap(x, thresh = 0.9, coolingRate = 0.9, trace = TRUE)
plot(seq_along(z$stats), z$stats, type = 'h', las = 1)
table(z$log)


f1 <- function(i) { 
  o <- fixOverlap(x, thresh = 0.9, coolingRate = 0.9, trace = TRUE)
  return(length(o$stats))
}

f2 <- function(i) { 
  o <- fixOverlap(x, thresh = 0.9, coolingRate = 1, trace = TRUE)
  return(length(o$stats))
}

f3 <- function(i) { 
  o <- fixOverlap(x, thresh = 0.9, coolingRate = 1.1, trace = TRUE)
  return(length(o$stats))
}

m1 <- sapply(1:100, FUN = f1)
m2 <- sapply(1:100, FUN = f2)
m3 <- sapply(1:100, FUN = f3)

z <- list(
  `cooling = 0.9` = m1, 
  `cooling = 1` = m2, 
  `cooling = 1.1` = m3
)

par(mar = c(4.5, 6, 1, 1))
boxplot(z, horizontal = TRUE, las = 1, cex.axis = 0.85, xlab = 'Number of Iterations', notch = TRUE)







# 
# 
# ## this doesn't work
# 
# 
# 
# findOverlap(x, thresh = 0.2)
# 
# findOverlap(x, thresh = 0.5)
# 
# 
# fixOverlap(x, thresh = 0.5, min.x = 1, max.x = 10)
# 
# fixOverlap(x, thresh = 0.5, min.x = 1, max.x = 10) - x
# 
# 
# ## TODO: use of optim(... method = 'SANN') or optimise()
# ##
# ## constraints:
# ##  * enforce boundary conditions
# ##  * enforce rank ordering
# ##  * minimize of()
# 
# 
# of <- function(par, x, thresh, min.x, max.x) {
#   
#   # print(par)
#   
#   # x.test[ov] <- x[ov] + par[ov]
#   x.test <- x + par
#   
#   # enforce boundary conditions
#   if(any(x.test < min.x) | any(x.test > max.x)) {
#     # print('boundary condition')
#     return(length(x))
#   }
#   
#   # enforce rank ordering
#   if(any(rank(x) != rank(x.test))) {
#     # print('rank violation')
#     return(length(x))
#   }
#   
#   # this is the main objective: minimize num affected elements
#   r1 <- length(findOverlap(x.test, thresh = thresh))
#   
#   r2 <- sum(abs(x - x.test))
#   
#   res <- r1 + r2
#   
#   return(res)
# }
# 
# ## this is not right
# o <- optim(
#   fn = of,
#   par = rep(0.25, times = length(x)),
#   method = 'SANN',
#   x = x,
#   thresh = 0.5,
#   min.x = 1, 
#   max.x = 15,
#   control = list(trace = 1)
# )
# 
# o$par
# 
# 
# 
# of(
#   par = rep(1, times = length(x)),
#   x = x,
#   thresh = 0.5,
#   min.x = 1, 
#   max.x = 10
#   )
# 


