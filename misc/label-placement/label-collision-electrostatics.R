library(aqp)
library(RColorBrewer)
library(viridisLite)


##
## nutty idea: label collision fixes via simulation of electrostatic charged particles
##


## TODO: 
## thanks Keith !!!

# compare final configurations
# allow electrostatic version to "stop" when badness is minimized


## input must be sorted ASC

# x <- c(1, 2, 3, 3.4, 3.5, 5, 6, 6.1, 10)

# x <- c(1, 2, 3.4, 3.4, 3.4, 3.4, 6, 8, 10, 12, 13, 13, 15, 15.5)

# x <- c(1, rep(5, times = 10), 12)

# x <- sort(1:15 + abs(rnorm(15, mean = 0, sd = 2)))

# x <- c(1, 2, 3, rep(4:5, each = 2), 7, 9)

x <- sort(c(1, 12, 5, 5, 4, 4, 6, 6, 6, 6, 6))

# x <- c(1, 2, 3, 3.4, 3.5, 5, 6, 10)

# results are visually interesting
# consider adjusting exponent and constant
# x <- jitter(c(1, rep(25, times = 48), 50), factor = 10)




# cols <- viridisLite::viridis(length(x))
# cols <- mako(length(x))

cols <- brewer.pal(9, 'Spectral')
cols <- colorRampPalette(cols)(length(x))


## TODO: animate this

system.time(z <- aqp:::.simParticles(x, k.start = 0.5, maxIter = 500))
.n <- nrow(z$xnew)

par(mar = c(0, 2, 1, 0.5), bg = 'black', fg = 'white')
layout(matrix(c(1, 2, 3, 4), ncol = 2, nrow = 2), heights = c(0.33, 0.66))

plot(z$cost, las = 1, type = 'b', axes = FALSE, cex = 0.66, xlim = c(0, .n))
mtext(text = sprintf("Converged (%s): %s", .n, z$converged), at = 0, side = 3, line = 0, cex = 0.75, font = 3, adj = 0)
matplot(z$xnew, type = 'l', lty = 1, las = 1, axes = FALSE, col = cols, lwd = 1)

points(x = rep(1, times = length(x)), y = x, cex = 0.66, pch = 16, col = cols)
points(x = rep(.n, times = length(x)), y = z$xnew[.n, ], cex = 0.66, pch = 16, col = cols)

text(x = 1, y = x, col = cols, labels = seq_along(x), cex = 0.66, font = 2, pos = 2)
text(x = .n, y = z$xnew[.n, ], col = cols, labels = seq_along(x), cex = 0.66, font = 2, pos = 4)

axis(side = 2, at = unique(x), labels = round(unique(x), 1), col.axis = 'white', las = 1, cex.axis = 0.6)



## fixOverlap doesn't always preserve rank ordering
##  ->> maybe impossible with ties in x?

system.time(z <- fixOverlap(x, trace = TRUE, maxIter = 1000))
.n <- nrow(z$states)

plot(z$stats, las = 1, type = 'b', axes = FALSE, cex = 0.66, xlim = c(0, .n))
mtext(text = sprintf("Converged (%s): %s", .n, z$converged), at = 0, side = 3, line = 0, cex = 0.75, font = 3, adj = 0)

matplot(z$states, type = 'l', lty = 1, las = 1, axes = FALSE, col = cols)

points(x = rep(1, times = length(x)), y = z$states[1, ], cex = 0.66, pch = 16, col = cols)
points(x = rep(.n, times = length(x)), y = z$x, cex = 0.66, pch = 16, col = cols)

text(x = 1, y = z$states[1, ], col = cols, labels = seq_along(x), cex = 0.66, font = 2, pos = 2)
text(x = .n, y = z$x, col = cols, labels = seq_along(x), cex = 0.66, font = 2, pos = 4)

axis(side = 2, at = unique(x), labels = round(unique(x), 1), col.axis = 'white', las = 1, cex.axis = 0.6)


