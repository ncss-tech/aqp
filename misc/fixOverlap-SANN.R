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


fixOverlap(x, thresh = 0.2, trace = TRUE)

fixOverlap(x, thresh = 1, trace = TRUE)

set.seed(10101)
z <- fixOverlap(x, thresh = 0.6, coolingRate = 0.85, trace = TRUE)
plot(seq_along(z$stats), z$stats, type = 'h', las = 1)

set.seed(10101)
z <- fixOverlap(x, thresh = 0.6, coolingRate = 0.95, trace = TRUE)
plot(seq_along(z$stats), z$stats, type = 'h', las = 1)


set.seed(10101)
z <- fixOverlap(x, thresh = 0.6, coolingRate = 1, trace = TRUE)
plot(seq_along(z$stats), z$stats, type = 'h', las = 1)


set.seed(10101)
z <- fixOverlap(x, thresh = 0.9, coolingRate = 0.9, trace = TRUE)
plot(seq_along(z$stats), z$stats, type = 'h', las = 1)


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


