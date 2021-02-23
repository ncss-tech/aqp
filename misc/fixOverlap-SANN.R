library(aqp)
library(lattice)
library(viridis)


# relatively challenging
x <- c(1, 2, 3.4, 3.4, 3.4, 3.4, 6, 8, 10, 12, 13, 13, 15, 15.5)

z <- fixOverlap(x, thresh = 0.8, trace = TRUE)

par(mar = c(4, 4, 1, 1))
layout(matrix(c(1,2)), widths = 1, heights = c(1,2))
plot(seq_along(z$stats), z$stats, type = 'h', las = 1)
matplot(z$states, type = 'l', lty = 1, las = 1, xlab = 'Iteration', ylab = 'x-position')

table(z$log)


dev.off()

## TODO: consider using a better function name
aqp:::.P(n0 = 3, n1 = 10, Te = 100 / (1 + 1), k = 1)


n0 <- 3
n1 <- 4
i <- 1:1000
T0 <- 500
Te <- T0 / (i + 1)
plot(Te, aqp:::.P(n0, n1, Te, k = 1), type = 'l', xlim = c(250, 0), las = 1)

g <- expand.grid(n0 = 5, n1 = 5:10, Te = Te)

g$P <- aqp:::.P(n0 = g$n0, n1 = g$n1, Te = g$Te, k = 1)

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


# much better
of <- function(i, thresh) {
  overlapMetrics(i, thresh = thresh)$ov
}

res <- sapply(s, function(i) {
  of(x, i)
})

plot(s, res, type = 'b', las = 1, xlab = 'Threshold', ylab = 'Overlap Qty.')

# simple
fixOverlap(x, thresh = 0.2, trace = TRUE)

# nearly impossible
length(findOverlap(x, thresh = 1))
overlapMetrics(x, thresh = 1)

z <- fixOverlap(x, thresh = 1, trace = TRUE)
plot(seq_along(z$stats), z$stats, type = 'h', las = 1)
matplot(z$states, type = 'l', lty = 1, las = 1, xlab = 'Iteration', ylab = 'x-position')
table(z$log)


# eval success rate
f1 <- function(i) {
  o <- fixOverlap(x, thresh = 1, trace = TRUE)
  return(o$converged)
}

m1 <- sapply(1:100, FUN = f1)

table(m1)




set.seed(10101)
z <- fixOverlap(x, thresh = 0.6, trace = TRUE)
plot(seq_along(z$stats), z$stats, type = 'h', las = 1)
matplot(z$states, type = 'l', lty = 1, las = 1)
table(z$log)

set.seed(10101)
z <- fixOverlap(x, thresh = 0.9, trace = TRUE)
plot(seq_along(z$stats), z$stats, type = 'h', las = 1)
matplot(z$states, type = 'l', lty = 1, las = 1)
# text(seq_along(z$log), y = 8, z$log, cex = 0.45)

table(z$log)
boxplot(z$stats ~ z$log, las = 1)


f1 <- function(i) {
  o <- fixOverlap(x, thresh = 0.9, trace = TRUE)
  return(length(o$stats))
}

f2 <- function(i) { 
  o <- fixOverlap(x, thresh = 0.6, trace = TRUE)
  return(length(o$stats))
}

f3 <- function(i) { 
  o <- fixOverlap(x, thresh = 0.3, trace = TRUE)
  return(length(o$stats))
}

m1 <- sapply(1:100, FUN = f1)
m2 <- sapply(1:100, FUN = f2)
m3 <- sapply(1:100, FUN = f3)

z <- list(
  `thresh = 0.9` = m1, 
  `thresh = 0.5` = m2, 
  `thresh = 0.3` = m3
)

par(mar = c(4.5, 6, 1, 1))
boxplot(z, horizontal = TRUE, las = 1, cex.axis = 0.85, xlab = 'Number of Iterations', notch = TRUE)


## TODO: what is the ideal interplay between T0 and k?
#
# * harder problems benefit from slower cooling schedules (large T0, small k)
# * simpler problems benefit from faster cooling

f1 <- function(i) {
  o <- fixOverlap(x, thresh = 0.5, k = 1, trace = TRUE)
  return(length(o$stats))
}

f2 <- function(i) { 
  o <- fixOverlap(x, thresh = 0.5, k = 5, trace = TRUE)
  return(length(o$stats))
}

f3 <- function(i) { 
  o <- fixOverlap(x, thresh = 0.5, k = 10, trace = TRUE)
  return(length(o$stats))
}

m1 <- sapply(1:100, FUN = f1)
m2 <- sapply(1:100, FUN = f2)
m3 <- sapply(1:100, FUN = f3)

z <- list(
  `k = 1` = m1, 
  `k = 5` = m2, 
  `k = 10` = m3
)

par(mar = c(4.5, 6, 1, 1))
boxplot(z, horizontal = TRUE, las = 1, cex.axis = 0.85, xlab = 'Number of Iterations', notch = TRUE)





