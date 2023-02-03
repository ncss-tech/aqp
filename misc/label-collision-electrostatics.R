library(aqp)
library(RColorBrewer)
library(viridisLite)


##
## nutty idea: label collision fixes via simulation of electrostatic charged particles
##


## input must be sorted ASC

# x <- c(1, 2, 3, 3.4, 3.5, 5, 6, 6.1, 10)
# x <- c(1, 2, 3.4, 3.4, 3.4, 3.4, 6, 8, 10, 12, 13, 13, 15, 15.5)
# x <- c(1, rep(5, times = 10), 12)
x <- sort(1:15 + rnorm(15, mean = 0, sd = 2))

# x <- c(1, 2, 3, rep(4:5, each = 2), 7, 9)

length(x)

# static electrical force between two charged particles
electricForce <- function(Q1, Q2, k, d, tiny = 0.1) {
  
  # if 0-distance, force is infinite
  # use a small number
  d <- ifelse(d < tiny, tiny, d)
  
  res <- (k * Q1 * Q2 ) / d^2
  return(res)
}




simParticles <- function(x, k.start = 0.1, n = 100) {
  
  x.orig <- x
  x.n <- length(x)
  
  xnew <- list()
  F_total <- rep(NA_real_, times = n)
  
  for(i in 1:n) {
    
    # pair-wise distances
    m <- as.matrix(dist(x))
    
    # remove distance to self
    diag(m) <- NA
    
    # repelling forces (same charge) between all particles
    .F <- electricForce(1, 1, k = k.start, d = m)
    
    # negative forces are to the left
    # lower triangle is used for particles to the right of any given position
    .F[lower.tri(.F)] <- - .F[lower.tri(.F)]
    
    # net repelling force on each particle
    # force vector: negative <--- | ---> positive
    .F_repl <- colSums(.F, na.rm = TRUE)
    
    # attractive forces between each particle and its original position
    # weaker than repelling forces
    .offset <- x.orig - x
    # direction is based on offset vector
    .direction <- sign(.offset)
    # force vector: negative <--- | ---> positive
    .F_attr <- electricForce(0.5, 0.5, k = k.start, d = abs(.offset), tiny = 0.1)
    
    ## hack
    # if attractive force is > repelling force, set repelling force to 0
    .F_repl <- ifelse(abs(.F_attr) > abs(.F_repl), 0, .F_repl)
    
    # sum attractive + repelling forces
    .F_net <- (.direction * .F_attr) + .F_repl
    
    # debugging 
    # rbind(x, x.orig, .direction, .F_repl, .F_attr, .F_net)
    
    
    # mass
    .m <- 10
    
    # time step
    .t <- 1
    
    # displacement vector
    # negative <--- | ---> positive
    .d <- 1/2 * (.F_net/.m) * .t^1
    
    # displacement of boundary points is always 0
    .d[1] <- 0
    .d[x.n] <- 0
    
    # keep track of new locations at time step i
    # displacement can't be outside of bounds
    xnew[[i]] <- pmin(pmax(x + .d, x.orig[1]), x.orig[x.n])
    
    # rank can't change... how can you fix that?
    
    # keep track of total force in system at time step i
    F_total[i] <- sum(abs(.F_net))
    
    # update locations
    x <- xnew[[i]]
    
    # stop when change in total force is very small
    if(i > 2) {
      if(min(abs(diff(F_total)), na.rm = TRUE) < 0.001) {
        break
      }  
    }
    
  }
  
  xnew <- do.call('rbind', xnew)
  
  .converged <- all(rank(xnew[nrow(xnew), ]) == seq_along(x))
  
  return(list(F_total = na.omit(F_total), xnew = xnew, converged = .converged))
}


# cols <- viridisLite::viridis(length(x))

cols <- brewer.pal(9, 'Spectral')

# cols <- mako(length(x))

cols <- colorRampPalette(cols)(length(x))


## TODO: animate this

z <- simParticles(x, k.start = 0.5, n = 1000)
.n <- nrow(z$xnew)

par(mar = c(0, 2, 1, 0.5), bg = 'black', fg = 'white')
layout(matrix(c(1, 2, 3, 4), ncol = 2, nrow = 2), heights = c(0.33, 0.66))

plot(z$F_total, las = 1, type = 'b', axes = FALSE, cex = 0.66, xlim = c(0, .n))
mtext(text = sprintf("Converged (%s): %s", .n, z$converged), at = 0, side = 3, line = 0, cex = 0.75, font = 3, adj = 0)
matplot(z$xnew, type = 'l', lty = 1, las = 1, axes = FALSE, col = cols)

points(x = rep(1, times = length(x)), y = x, cex = 0.66, pch = 16, col = cols)
points(x = rep(.n, times = length(x)), y = z$xnew[.n, ], cex = 0.66, pch = 16, col = cols)

text(x = 1, y = x, col = cols, labels = seq_along(x), cex = 0.66, font = 2, pos = 2)
text(x = .n, y = z$xnew[.n, ], col = cols, labels = seq_along(x), cex = 0.66, font = 2, pos = 4)

axis(side = 2, at = unique(x), labels = round(unique(x), 1), col.axis = 'white', las = 1, cex.axis = 0.6)



## fixOverlap doesn't always preserve rank ordering
##  ->> maybe impossible with ties in x?

z <- fixOverlap(x, trace = TRUE)
.n <- nrow(z$states)

plot(z$stats, las = 1, type = 'b', axes = FALSE, cex = 0.66, xlim = c(0, .n))
mtext(text = sprintf("Converged (%s): %s", .n, z$converged), at = 0, side = 3, line = 0, cex = 0.75, font = 3, adj = 0)

matplot(z$states, type = 'l', lty = 1, las = 1, axes = FALSE, col = cols)

points(x = rep(1, times = length(x)), y = z$states[1, ], cex = 0.66, pch = 16, col = cols)
points(x = rep(.n, times = length(x)), y = z$x, cex = 0.66, pch = 16, col = cols)

text(x = 1, y = z$states[1, ], col = cols, labels = seq_along(x), cex = 0.66, font = 2, pos = 2)
text(x = .n, y = z$x, col = cols, labels = seq_along(x), cex = 0.66, font = 2, pos = 4)

axis(side = 2, at = unique(x), labels = round(unique(x), 1), col.axis = 'white', las = 1, cex.axis = 0.6)


