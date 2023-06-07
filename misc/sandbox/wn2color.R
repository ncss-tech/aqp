# http://www.midnightkite.com/color.html
# https://aty.sdsu.edu/explain/optics/rendering.html


## simulate the color of materials that reflect light (D65) within very narrow bands of select wavelength(s)

library(aqp)
library(gifski)

?spec2Munsell
cols <- c('10YR 6/2', '5YR 5/6', '10B 4/4')
res <- mixMunsell(cols, keepMixedSpec = TRUE, mixingMethod = 'reference')

spec2Munsell(res$spec)



w2c <- function(.w, .sd = rep(10, times = length(.w)), .max = rep(0.75, times = length(.w)), .cex = 0.8) {
  
  # sanity check on argument lengths
  
  
  # spectra sequence, 5nm resolution
  .m <- seq(from = 380, to = 730, length.out = 71)
  
  
  # iterate over wavelengths
  .specList <- list()
  for(i in seq_along(.w)) {
    # index to nearest bin
    .idx <- which.min(abs(.m - .w[i]))
    
    # Gaussian kernel
    .s_i <- dnorm(.m, mean = .m[.idx], sd = .sd[i]) * 10
    .s_i <- scales::rescale(.s_i, to = c(0, .max[i]))
    .s_i <- zapsmall(.s_i)
    
    .specList[[i]] <- .s_i
  }
  
  # add spectra
  .s <- Reduce('+', .specList)
  
  # convert spectra -> sRGB vector
  .res <- spec2Munsell(.s, convert = FALSE, res = 5)
  .col <- rgb(.res)
  
  # label colors
  .labcol <- invertLabelColor(.col, threshold = 0.8)
  
  # figure details
  .fgcol <- par('fg')
  .lab.x <- 375
  .lab.y <- max(.s) - (max(.s) * 0.05)
  
  
  plot(.m, .s, type = 'h', xlab = '', ylab = '', axes = FALSE, col = .col, lwd = 7, lend = 1)
  
  axis(1, at = seq(from = 380, to = 730, by = 10), cex.axis = 0.75 * .cex, las = 3, col.axis = .fgcol, col.ticks = .fgcol)
  
  rect(xleft = 370, ybottom = max(.s) * 0.85, xright = 410, ytop = max(.s) + (0.05 * max(.s)), pch = 15, col = .col)
  
  text(.lab.x, .lab.y, .col, font = 2, cex = 1 * .cex, col = .labcol, adj = 0)
  
  ## TODO: add Munsell color
  
  # text(.lab.x, .lab.y, sprintf("std.dev: %s", .sd), font = 1, cex = 0.8 * .cex, col = .labcol, pos = 1, offset = c(0, 0.5))
  # 
  # text(.lab.x, .lab.y, .col, font = 1, cex = 1 * .cex, col = .labcol, pos = 1, offset = c(0, 1.6))
  
  abline(v = .w, lty = 3)
  
  
  ## TODO: simplify title when length(.w) > 1
  if(length(.w) < 2) {
    .txt <- sprintf('Estimated Color @ %snm [std.dev %snm]\nD65 Illuminant / CIE1931 Standard Observer', .w, .sd)
  } else {
    .txt <- 'Estimated Color\nD65 Illuminant / CIE1931 Standard Observer'
  }
  
  title(.txt, cex.main = 0.9 * .cex, col.main = .fgcol)
  
  # closer annotation of axes
  mtext('Reflectance', side = 2, line = 1.5)
  mtext('Wavelength (nm)', side = 1, line = 2.5)
  
  box()
  
  return(.col)
}


w2c(.w = 415)


w2c(.w = 490)

w2c(.w = 600)

w2c(.w = c(480, 620), .sd = c(10, 20), .max = c(0.8, 0.5))

w2c(.w = c(490, 535, 650), .sd = c(20, 20, 20), .max = c(1, 0.5, 1))

par(mar = c(4.25, 1, 3, 1), mfrow = c(2, 1))

w2c(440)
w2c(520)

par(mar = c(4.25, 1, 3, 1), mfrow = c(3, 1))
w2c(532, .sd = 5)
w2c(532, .sd = 10)
w2c(532, .sd = 20)

par(mar = c(4.25, 1, 3, 1), mfrow = c(1, 1))
w2c(600)

par(mar = c(4.25, 1, 3, 1), mfrow = c(1, 1))
w2c(600, .sd = 10)
w2c(600, .sd = 30)
w2c(600, .sd = 50)

w2c(450, .sd = 20)

w2c(390, .sd = 20)
w2c(680, .sd = 20)

w2c(480, .sd = 20)





.plotIt <- function(i, ...) {
  
  par(mar = c(4.25, 1, 3, 1), mfrow = c(1, 1))
  w2c(.w = i, ...)
  
}

.seq <- seq(from = 380, to = 730, by = 5)
gifski::save_gif(sapply(.seq, .plotIt, .max = 1), gif_file = 'e:/working_copies/spec1.gif', delay = 0.1)


.seq <- seq(from = 1, to = 50, by = 1)
.seq <- c(.seq, rev(.seq))
gifski::save_gif(
  {
    sapply(.seq, function(i) {
      par(mar = c(4.25, 2.5, 3, 0.25), mfrow = c(1, 1), bg = 'black', fg = 'white')
      w2c(.w = 480, .sd = i, .max = 0.9, .cex = 1.1)
    })
    
    sapply(.seq, function(i) {
      par(mar = c(4.25, 2.5, 3, 0.25), mfrow = c(1, 1), bg = 'black', fg = 'white')
      w2c(.w = 600, .sd = i, .max = 0.9, .cex = 1.1)
    })
    
    sapply(.seq, function(i) {
      par(mar = c(4.25, 2.5, 3, 0.25), mfrow = c(1, 1), bg = 'black', fg = 'white')
      w2c(.w = 530, .sd = i, .max = 0.9, .cex = 1.1)
    })
    
    sapply(.seq, function(i) {
      par(mar = c(4.25, 2.5, 3, 0.25), mfrow = c(1, 1), bg = 'black', fg = 'white')
      w2c(.w = 680, .sd = i, .max = 0.9, .cex = 1.1)
    })
  }, 
  gif_file = 'e:/working_copies/spec2.gif', 
  delay = 0.02, 
  width = 800, 
  height = 400
)



g <- expand.grid(sd = seq(from = 1, to = 50, by = 5), w = seq(from = 380, to = 730, by = 10))

gifski::save_gif(
  sapply(1:nrow(g), function(i) {
    par(mar = c(4.25, 1.5, 3, 0.25), mfrow = c(1, 1))
    w2c(.w = g$w[i], .sd = g$sd[i], .max = 0.9, .cex = 1.1)
  }), 
  gif_file = 'e:/working_copies/spec3.gif', 
  delay = 0.1, 
  width = 800, 
  height = 400
)

