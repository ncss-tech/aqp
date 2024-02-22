# devtools::load_all()

.npanel <- 4

# ragg::agg_png(file = 'art.png', width = 1600, height = 900, scaling = 1.5)

par(mar = c(0.1, 0.1, 0.1, 0.1), bg = 'black', fg = 'white', mfrow = c(.npanel, .npanel))

# results are visually interesting
# consider adjusting exponent and constant
set.seed(10101)
x <- c(1, rep(25, times = 30), 50)
x <- abs(jitter(x, factor = 5))

## artistic effects more interesting without pre-sort
# data should be pre-sorted
# (x <- sort(x))

## effect of q
# for(i in 1:(.npanel^2)) {
for(i in seq(0.1, 1.5, length.out = .npanel^2)) {
  
  
  cols <- hcl.colors(n = 9, palette = 'Zissou 1', rev = TRUE)
  cols <- colorRampPalette(cols)(length(x))
  
  ## TODO: argument for uniform spacing attractive force schedule
  ## TODO: argument for pre-sorting
  
  z <- fixOverlap(x, thresh = 2, q = i, chargeDecay = 0, QkA_GrowthRate = 0, method = 'E', maxIter = 100, trace = TRUE)
  .n <- nrow(z$states)
  
  matplot(rbind(x, z$states), type = 'l', lty = 1, las = 1, axes = FALSE, col = cols, lwd = 1, log = 'x')
  
  # points(x = rep(1, times = length(x)), y = x, cex = 0.66, pch = 16, col = cols)
  # points(x = rep(.n + 1, times = length(x)), y = z$states[.n, ], cex = 0.66, pch = 16, col = cols)
  
}

# dev.off()


for(i in seq(0, 0.5, length.out = .npanel^2)) {
  
  
  cols <- hcl.colors(n = 9, palette = 'Zissou 1', rev = TRUE)
  cols <- colorRampPalette(cols)(length(x))
  
  ## TODO: argument for uniform spacing attractive force schedule
  ## TODO: argument for pre-sorting
  
  z <- fixOverlap(x, thresh = 2, q = 2, chargeDecay = i, QkA_GrowthRate = 0.05, method = 'E', maxIter = 100, trace = TRUE)
  .n <- nrow(z$states)
  
  matplot(rbind(x, z$states), type = 'l', lty = 1, las = 1, axes = FALSE, col = cols, lwd = 1, log = 'x')
  
  # points(x = rep(1, times = length(x)), y = x, cex = 0.66, pch = 16, col = cols)
  # points(x = rep(.n + 1, times = length(x)), y = z$states[.n, ], cex = 0.66, pch = 16, col = cols)
  
}



for(i in seq(0, 0.5, length.out = .npanel^2)) {
  
  
  cols <- hcl.colors(n = 9, palette = 'Zissou 1', rev = TRUE)
  cols <- colorRampPalette(cols)(length(x))
  
  ## TODO: argument for uniform spacing attractive force schedule
  ## TODO: argument for pre-sorting
  
  z <- fixOverlap(x, thresh = 2, q = 1, chargeDecay = 0.05, QkA_GrowthRate = i, method = 'E', maxIter = 100, trace = TRUE)
  .n <- nrow(z$states)
  
  matplot(rbind(x, z$states), type = 'l', lty = 1, las = 1, axes = FALSE, col = cols, lwd = 1, log = 'x')
  
  # points(x = rep(1, times = length(x)), y = x, cex = 0.66, pch = 16, col = cols)
  # points(x = rep(.n + 1, times = length(x)), y = z$states[.n, ], cex = 0.66, pch = 16, col = cols)
  
}


## threshold effect
for(i in seq(0.5, 4, length.out = .npanel^2)) {
  
  
  cols <- hcl.colors(n = 9, palette = 'Zissou 1', rev = TRUE)
  cols <- colorRampPalette(cols)(length(x))
  
  ## TODO: argument for uniform spacing attractive force schedule
  ## TODO: argument for pre-sorting
  
  z <- fixOverlap(x, thresh = i, q = 1, chargeDecay = 0.01, QkA_GrowthRate = 0.05, method = 'E', maxIter = 100, trace = TRUE)
  .n <- nrow(z$states)
  
  matplot(rbind(x, z$states), type = 'l', lty = 1, las = 1, axes = FALSE, col = cols, lwd = 1, log = 'x')
  
  # points(x = rep(1, times = length(x)), y = x, cex = 0.66, pch = 16, col = cols)
  # points(x = rep(.n + 1, times = length(x)), y = z$states[.n, ], cex = 0.66, pch = 16, col = cols)
  
}



# dev.off()
