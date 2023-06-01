devtools::load_all()


evalMethods <- function(x, thresh, q, ...) {
  
  cols <- hcl.colors(n = 9, palette = 'Zissou 1', rev = TRUE)
  cols <- colorRampPalette(cols)(length(x))
  
  z <- fixOverlap(x, thresh = thresh, method = 'E', maxIter = 100, trace = TRUE, q = q, ...)
  .n <- nrow(z$xnew)
  
  par(mar = c(0, 2, 1, 0.5), bg = 'black', fg = 'white')
  layout(matrix(c(1, 2, 3, 4), ncol = 2, nrow = 2), heights = c(0.33, 0.66))
  
  plot(seq_along(z$cost), z$cost, las = 1, type = 'b', axes = FALSE, cex = 0.66, xlim = c(1, .n))
  mtext(text = sprintf("Converged (%s): %s", .n, z$converged), at = 0, side = 3, line = 0, cex = 0.75, font = 3, adj = 0)
  matplot(rbind(x, z$xnew), type = 'l', lty = 1, las = 1, axes = FALSE, col = cols, lwd = 1)
  
  points(x = rep(1, times = length(x)), y = x, cex = 0.66, pch = 16, col = cols)
  points(x = rep(.n + 1, times = length(x)), y = z$xnew[.n, ], cex = 0.66, pch = 16, col = cols)
  
  text(x = 1, y = x, col = cols, labels = seq_along(x), cex = 0.66, font = 2, pos = 2)
  text(x = .n + 1, y = z$xnew[.n, ], col = cols, labels = seq_along(x), cex = 0.66, font = 2, pos = 4)
  
  axis(side = 2, at = unique(x), labels = round(unique(x), 1), col.axis = par('fg'), las = 1, cex.axis = 0.6)
  
  
  ## SANN_1D doesn't always preserve rank ordering
  ##  ->> not designed to use unsorted input
  ##  ->> maybe impossible with ties in x?
  
  z <- fixOverlap(x, thresh = thresh, method = 'S', trace = TRUE, maxIter = 1000)
  .n <- nrow(z$states)
  
  plot(seq_along(z$stats), z$stats, las = 1, type = 'b', axes = FALSE, cex = 0.66, xlim = c(1, .n))
  mtext(text = sprintf("Converged (%s): %s", .n, z$converged), at = 0, side = 3, line = 0, cex = 0.75, font = 3, adj = 0)
  
  matplot(z$states, type = 'l', lty = 1, las = 1, axes = FALSE, col = cols)
  
  points(x = rep(1, times = length(x)), y = z$states[1, ], cex = 0.66, pch = 16, col = cols)
  points(x = rep(.n, times = length(x)), y = z$x, cex = 0.66, pch = 16, col = cols)
  
  text(x = 1, y = z$states[1, ], col = cols, labels = seq_along(x), cex = 0.66, font = 2, pos = 2)
  text(x = .n, y = z$x, col = cols, labels = seq_along(x), cex = 0.66, font = 2, pos = 4)
  
  axis(side = 2, at = unique(x), labels = round(unique(x), 1), col.axis = par('fg'), las = 1, cex.axis = 0.6)
  
}



##
## note: in general, as the complexity increases (larger thresholds, more overlap), q need to be larger
##

x <- c(0, 2, 5, 12, 18, 20, 35, 40, 50, 56, 90, 120, 145, 150)
evalMethods(x, thresh = 5, q = 1.1)
evalMethods(x, thresh = 5, q = 1.8)
evalMethods(x, thresh = 5, q = 3)
evalMethods(x, thresh = 5, q = 4)
evalMethods(x, thresh = 5, q = 5)

evalMethods(x, thresh = 10, q = 4)


x <- c(0, 5, 12, 18, 20, 35, 40, 55, 90, 120, 145, 150)
evalMethods(x, thresh = 9, q = 2)

x <- c(0, 3, 20, 35, 40, 55, 90, 120, 145, 150)
evalMethods(x, thresh = 6, q = 1)


x <- sort(c(0, rep(25, 5), 150))
evalMethods(x, thresh = 6, q = 3)



