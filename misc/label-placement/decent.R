# devtools::load_all()

.npanel <- 4

ragg::agg_png(file = 'decent-into-madness.png', width = 1600, height = 900, scaling = 1.8)

par(mar = c(0.1, 0.1, 0.1, 0.1), bg = 'black', fg = 'white', mfrow = c(.npanel, .npanel))

# results are visually interesting
# consider adjusting exponent and constant
set.seed(10101)
x <- c(1, rep(25, times = 30), 50)
x <- abs(jitter(x, factor = 5))

## artistic effects more interesting without pre-sort
# (x <- sort(x))

## effect of q
for(i in seq(0.1, 1.5, length.out = .npanel^2)) {
  
  cols <- hcl.colors(n = 9, palette = 'Zissou 1', rev = TRUE)
  cols <- colorRampPalette(cols)(length(x))
  
  z <- fixOverlap(x, thresh = 2, q = i, chargeDecay = 0, QkA_GrowthRate = 0, method = 'E', maxIter = 100, trace = TRUE)
  .n <- nrow(z$states)
  
  matplot(rbind(x, z$states), type = 'l', lty = 1, las = 1, axes = FALSE, col = cols, lwd = 1, log = 'x')
  
}

dev.off()
