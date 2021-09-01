library(aqp)
library(soilDB)
library(lattice)
library(tactile)

data("ROSETTA.centroids")

x <- ROSETTA.centroids

# hmm.. why does this give NA?
estimateAWC('si', omcl = 1)


m <- data.frame(
  names = SoilTextureLevels(which = 'names'),
  codes = SoilTextureLevels(which = 'codes')
)

idx <- match(x$texture, m$names)
x$txtcl <- m$codes[idx]


x$est.awc <- estimateAWC(x$txtcl, omcl = rep(1, times = nrow(x)))


xyplot(
  est.awc ~ awc, 
  data = x, 
  xlim = c(-0.025, 0.23),
  ylim = c(-0.025, 0.23),
  xlab = 'AWC via ROSETTA Centroids',
  ylab = 'Estimated AWC',
  par.settings = tactile.theme(), 
  asp = 1, 
  scales = list(alternating = 3, tick.number = 10), 
  pch = 16,
  panel = function(...) {
    panel.grid(-1, -1)
    panel.xyplot(...)
    panel.text(x = x$awc, y = x$est.awc, labels = x$texture, cex = 0.75 , pos = 1)
    panel.abline(lm(est.awc ~ awc, data = x))
  })



x$diff <- with(x, awc - est.awc)

dotplot(
  texture ~ diff, 
  data = x, 
  xlab = 'ROSETTA AWC - Estimated AWC',
  par.settings = tactile.theme(), 
  cex = 1.5,
  panel = function(...) {
    panel.abline(v = 0, lty = 2)
    panel.dotplot(...)
  })


