library(aqp)
library(lattice)
library(viridis)


## explore correlation between dist(initial configuration) and dist(proposed configuration)
x <- c(1, 2, 3.4, 3.4, 3.4, 3.4, 6, 8, 10, 12, 13, 13, 15, 15.5)
z <- fixOverlap(x, thresh = 0.6, trace = TRUE)

# better metrics?
z$cor <- sapply(seq_along(z$stats), FUN = function(i) {
  1 - cor(dist(x), dist(z$states[i, ]), method = 'spearman')
})

z$mantel <- sapply(seq_along(z$stats), FUN = function(i) {
  ape::mantel.test(as.matrix(dist(x)), as.matrix(dist(z$states[i, ])))$z.stat
})



# hmm.
plot(seq_along(z$stats), z$cor, type = 'l', las = 1)

# hmm
plot(seq_along(z$stats), z$mantel, type = 'l', las = 1)

# plot(dist(x), dist(z$states[1, ]))






tracePlot <- function(x, z, cex.axis.labels = 0.85) {
  # setup plot device
  par(mar = c(4, 4, 1, 1), bg = 'black', fg = 'white')
  layout(matrix(c(1,2,3)), widths = 1, heights = c(1,1,2))
  
  # B, O, +, -
  cols <- c(grey(0.5), grey(0.85), 'royalblue', 'firebrick')
  
  # total overlap (objective function) progress
  plot(
    seq_along(z$stats), z$stats, 
    type = 'h', las = 1,
    xlab = 'Iteration', ylab = 'Total Overlap',
    axes = FALSE,
    col = cols[as.numeric(z$log)]
  )
  
  axis(side = 2, cex.axis = cex.axis.labels, col.axis = 'white', las = 1, line = -2)
  mtext('Overlap', side = 2, line = 2, cex = cex.axis.labels, font = 2)
  
  # deviation from original configuration
  plot(
    seq_along(z$stats), z$ssd, 
    type = 'h', las = 1,
    xlab = 'Iteration', ylab = 'Deviation',
    axes = FALSE,
    col = cols[as.numeric(z$log)]
  )
  
  axis(side = 2, cex.axis = cex.axis.labels, col.axis = 'white', las = 1, line = -2)
  mtext('Deviation', side = 2, line = 2, cex = cex.axis.labels, font = 2)
  
  
  # adjustments at each iteration
  matplot(
    z$states, type = 'l', 
    lty = 1, las = 1, 
    xlab = 'Iteration', ylab = 'x-position',
    axes = FALSE,
    col = 2:6
  )
  
  axis(side = 2, cex.axis = cex.axis.labels, col.axis = 'white', las = 1, at = x, labels = seq_along(z$x))
  axis(side = 4, cex.axis = cex.axis.labels, col.axis = 'white', las = 1, at = z$x, labels = seq_along(z$x), line = -2)
  mtext('Position', side = 2, line = 2.5, cex = cex.axis.labels, font = 2)
  
  axis(side = 1, cex.axis = 1, col.axis = 'white', line = 0)
  mtext('Iteration', side = 1, line = 2.5, cex = cex.axis.labels, font = 2)
  
}




# relatively challenging
x <- c(1, 2, 3.4, 3.4, 3.4, 3.4, 6, 8, 10, 12, 13, 13, 15, 15.5)

overlapMetrics(x, thresh = 0.6)

# fix overlap, return debugging information
# set.seed(10101)
z <- fixOverlap(x, thresh = 0.6, trace = TRUE)

tracePlot(x, z)
# trace log
table(z$log)

dev.off()

plot(z$stats, z$ssd)



# nearly impossible
x <- runif(10, min = 2.5, max = 3.5)
# widen boundary conditions
z <- fixOverlap(x, thresh = 0.2, trace = TRUE, min.x = 0, max.x = 10, maxIter = 2000, adj = 0.05)

tracePlot(x, z)
table(z$log)

dev.off()





##

# evalute over many replications
# pct of iterations accepted
res <- replicate(10, expr = fixOverlap(x, thresh = 0.6, trace = TRUE)$log)

##







x <- c(1, 1.1, 3.4, 3.5, 4.1, 5, 6, 7.8, 8, 9, 10, 12)

d <- lapply(letters[1:length(x)], random_profile, SPC = TRUE)
d <- combine(d)

par(mar = c(6, 0, 0, 0))
plotSPC(d, relative.pos = x, plot.depth.axis = FALSE, name.style = 'center-center', hz.depths = TRUE)
axis(side = 1, at = x, labels = x)

x.fixed <- fixOverlap(x, thresh = 0.6, trace = TRUE, k = 1)

plotSPC(d, relative.pos = x.fixed$x, plot.depth.axis = FALSE, name.style = 'center-center', hz.depths = TRUE)
axis(side = 1, at = x, labels = profile_id(d))
axis(side = 1, at = x.fixed$x, labels = profile_id(d), line = 3)



### 

# safe vectorization
P <- Vectorize(aqp:::.P)

P(n0 = 3, n1 = 10, Te = 100 / (1 + 1), k = 1)

n0 <- 3
n1 <- 4
i <- 1:1000
T0 <- 500
Te <- T0 / i

plot(i, P(n0, n1, Te, k = 1), type = 'l', las = 1)
plot(i, Te, type = 'l', las = 1)

g <- expand.grid(n0 = 5, n1 = seq(5, 6, by = 0.2), Te = Te)

g$P <- P(n0 = g$n0, n1 = g$n1, Te = g$Te, k = 1)

hist(g$P, breaks = 30, las = 1)

levelplot(P ~ Te * n1, data = g, col.regions = viridis, contour = TRUE, xlim = c(500, -5))

xyplot(P ~ Te, groups = factor(n1), data = g, type = 'l', as.table = TRUE, auto.key = list(lines = TRUE, points = FALSE, space = 'right'), par.settings = tactile::tactile.theme(), xlim = c(500, -5))


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
plot(seq_along(z$stats), z$ssd, type = 'h', las = 1)
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
  o <- fixOverlap(x, thresh = 0.6, k = 1, trace = TRUE)
  return(length(o$stats))
}

f2 <- function(i) { 
  o <- fixOverlap(x, thresh = 0.6, k = 5, trace = TRUE)
  return(length(o$stats))
}

f3 <- function(i) { 
  o <- fixOverlap(x, thresh = 0.6, k = 10, trace = TRUE)
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





