library(aqp)
library(lattice)
library(latticeExtra)
library(tactile)


## assumptions: 
##              * current CIELAB definitions of neutral chips are accurate
##              * neutral chips have relatively "flat" spectra (is that true?)



# local minima!
library(pracma)


data("munsell.spectra")
table(munsell.spectra$hue)


# all hues, limit to specific hue / chroma slice
x <- munsell.spectra[munsell.spectra$value  == 2 & munsell.spectra$chroma == 1, ]

# each Munsell chip has a 36-element spectra
# ranging from 380-730 nm
# table(x$munsell)

# spectra IDs
x$ID <- factor(x$munsell)
# create a color / chip
cols <- parseMunsell(as.character(levels(x$ID)))

# plot style
tps <- tactile.theme(superpose.line = list(col = cols, lwd = 2))

# final figure
xyplot(
  reflectance ~ wavelength, groups = ID, data = x, 
  par.settings = tps,
  main = 'Value 6 / Chroma 8',
  type = c('l', 'g'),
  ylab = 'Reflectance',
  xlab = 'Wavelength (nm)',
  scales = list(tick.number = 12),
  xlim = c(370, 740)
)



# N 9/
sn <- rep(0.5, times = 36)
spec2Munsell(sn)


# N 8/
sn <- rep(0.35, times = 36)
spec2Munsell(sn)

# N 7/
sn <- rep(0.15, times = 36)
spec2Munsell(sn)

# N 6/
sn <- rep(0.06, times = 36)
spec2Munsell(sn)

# N 5/
sn <- rep(0.025, times = 36)
spec2Munsell(sn)

# N 4/
sn <- rep(0.01, times = 36)
spec2Munsell(sn)

# N 3/
sn <- rep(0.005, times = 36)
spec2Munsell(sn)

# N 2/
sn <- rep(0.003, times = 36)
spec2Munsell(sn)

# sn <- rep(0.0001, times = 36)
# spec2Munsell(sn)
# 
# sn <- rep(0.000001, times = 36)
# spec2Munsell(sn)



obj <- function(r) {
  
  .max <- 10
  
  if(r > 1 | r < 0) {
    return(.max)
  }
  
  # init "flat" spectra with target reflectance
  .s <- rep(r, times = 36)
  
  # find the closest Munsell chip
  .o <- spec2Munsell(.s)
  
  # print(.o)
  
  # max obj fun value:
  # not a neutral chip
  if(!.o$hue == 'N') {
    return(.max)
  }
  # # not the target value
  # if(!.o$value == v) {
  #   return(.max)
  # }
  # 
  # dE00 is the objective function value
  return(.o$sigma)
}

obj2 <- function(r, v) {
  
  .max <- 10
  
  if(r > 1 | r < 0) {
    return(.max)
  }
  
  # init "flat" spectra with target reflectance
  .s <- rep(r, times = 36)
  
  # find the closest Munsell chip
  .o <- spec2Munsell(.s)
  
  # print(.o)
  
  # max obj fun value:
  # not a neutral chip
  if(!.o$hue == 'N') {
    return(.max)
  }
  # not the target value
  if(!.o$value == v) {
    return(.max)
  }

  # dE00 is the objective function value
  return(.o$sigma)
}

obj(0.001)

obj2(0.001, v = 2)
obj2(0.001, v = 5)

# x <- seq(0, 0.6,  by = 0.001)

# more efficient
x <- logseq(0.0005, 0.6, n = 500)
y <- sapply(x, obj)

d <- data.frame(x, y)
d <- subset(d, subset = y < 10)

plot(y ~ x, data = d, type = 'l', cex = 0.1,  log = 'x', las = 1)

# my original estimates
abline(v = c(0.003, 0.005, 0.01, 0.025, 0.06, 0.15, 0.35, 0.5), col = 'red')


# xyplot(y ~ x, data = d, type = 'b', cex = 0.1, scales = list(x = list(log = 10, at = seq(0.001, 0.6, by = 0.01))), par.settings = tactile.theme())



## search for reasonable spectra for neutral chips
# assumption: spectra are approximately "flat"
n.2 <- findmins(obj2, v = 2, a = 0, b = 0.1)
n.25 <- findmins(obj2, v = 2.5, a = 0, b = 0.1)
n.3 <- findmins(obj2, v = 3, a = 0, b = 0.6)
n.4 <- findmins(obj2, v = 4, a = 0, b = 0.6)
n.5 <- findmins(obj2, v = 5, a = 0, b = 0.6)
n.6 <- findmins(obj2, v = 6, a = 0, b = 0.6)
n.7 <- findmins(obj2, v = 7, a = 0, b = 0.6)
n.8 <- findmins(obj2, v = 8, a = 0, b = 0.6)
n.9 <- findmins(obj2, v = 9, a = 0, b = 0.6)

n <- c(n.2, n.25,  n.3, n.4, n.5, n.6, n.7, n.8, n.9)

.chips <- c(2, 2.5, 3:9)
m <- sprintf("N %s/", c(2, 2.5, 3:9))

cols <- parseMunsell(m)


layout(matrix(c(1, 2), nrow = 2), heights = c(3, 1))

par(mar = c(4, 4, 1, 0.25))

plot(y ~ x, data = d, type = 'l', cex = 0.1,  log = 'x', ylab = 'dE00 from requested neutral chip', xlab = 'constant reflectance value', axes = FALSE, xlim = c(0.0025, 0.65))
abline(v = n, col = 'royalblue', lty = 3)
abline(h = 1.5, col = 'firebrick', lty = 2)
axis(side = 2, las = 1)
axis(side = 1, at = n, labels = round(n, 3))
text(x = n, y = 5, labels = m, cex = 1, font = 2)


par(mar = c(1, 1, 1, 1))
soilPalette(cols, lab = m)













# TODO: why doesn't this work?
# # N 2/
# optim(par = list(r = 0.001),  fn = obj, v = 2, method = 'BFGS')
# 
# optim(par = list(r = 0.001),  fn = obj, v = 3, method = 'BFGS')
# 






