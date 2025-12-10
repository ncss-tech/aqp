
# always use the latest version, even if not yet installed
devtools::load_all()

library(lattice)
library(latticeExtra)
library(tactile)


# locate local minima
library(pracma)


## assumptions: 
##              * current CIELAB definitions of neutral chips are accurate
##              * neutral chips have relatively "flat" spectra

## notes:
##              * each Munsell chip has a 36-element spectra


# objective function: dE00(reflectance)
obj1 <- function(r) {
  
  # maximum dE00, helps keep local minimum identification more stable
  .max <- 10
  
  # reflectance must be  within [0, 1]
  if(r > 1 | r < 0) {
    return(.max)
  }
  
  # init "flat" spectra with target reflectance
  .s <- rep(r, times = 36)
  
  # find the closest Munsell chip
  .o <- spec2Munsell(.s)
  
  # not a neutral chip -> max dE00
  if(!.o$hue == 'N') {
    return(.max)
  }
  
  # otherwise, dE00 between flat spectra at r and target N chip
  return(.o$sigma)
}


# objective function: dE00(reflectance, neutral chip munsell value)
obj2 <- function(r, v) {
  
  # maximum dE00, helps keep local minimum identification more stable
  .max <- 10
  
  # reflectance must be  within [0, 1]
  if(r > 1 | r < 0) {
    return(.max)
  }
  
  # init "flat" spectra with target reflectance
  .s <- rep(r, times = 36)
  
  # find the closest Munsell chip
  .o <- spec2Munsell(.s)
  
  # not a neutral chip -> max dE00
  if(!.o$hue == 'N') {
    return(.max)
  }
  
  # not the target value -> max dE00
  if(!.o$value == v) {
    return(.max)
  }
  
  # otherwise, dE00 between flat spectra at r and target N chip
  return(.o$sigma)
}

# test: ok
obj1(0.03)

obj2(0.03, v = 2)
obj2(0.2, v = 5)
obj2(0.85, v = 9.5)

# log-sequence covering estimated range
.minr <- 0.003
.maxr <- 0.9
x <- logseq(.minr, .maxr, n = 500)

# eval objective function over full range of reflectance, for any neutral chip
y <- sapply(x, obj1)

# viz objective function 1 over range of possible reflectance
d <- data.frame(x, y)

# ignore obvious incorrect values
d <- subset(d, subset = y < 10)

# looking for local minima
# perfect
plot(y ~ x, data = d, type = 'l', cex = 0.1,  log = 'x', las = 1)
plot(y ~ x, data = d, type = 'l', cex = 0.1, las = 1)


## search for reasonable spectra for neutral chips
# slower, but search entire range
# ~ 1 minute
system.time( {
  n.2 <- findmins(obj2, v = 2, a = .minr, b = .maxr)
  n.25 <- findmins(obj2, v = 2.5, a = .minr, b = .maxr)
  n.3 <- findmins(obj2, v = 3, a = .minr, b = .maxr)
  n.4 <- findmins(obj2, v = 4, a = .minr, b = .maxr)
  n.5 <- findmins(obj2, v = 5, a = .minr, b = .maxr)
  n.6 <- findmins(obj2, v = 6, a = .minr, b = .maxr)
  n.7 <- findmins(obj2, v = 7, a = .minr, b = .maxr)
  n.8 <- findmins(obj2, v = 8, a = .minr, b = .maxr)
  n.85 <- findmins(obj2, v = 8.5, a = .minr, b = .maxr)
  n.9 <- findmins(obj2, v = 9, a = .minr, b = .maxr)
  n.95 <- findmins(obj2, v = 9.5, a = .minr, b = .maxr)
}
)

# combine local minima
# looks good
(n <- c(n.2, n.25,  n.3, n.4, n.5, n.6, n.7, n.8, n.85, n.9, n.95))

# prepare chip colors and labels 
.chips <- c(2, 2.5, 3, 4, 5, 6, 7, 8, 8.5, 9, 9.5)
.m <- sprintf("N %s/", .chips)
.cols <- parseMunsell(.m)


# composite figure
layout(matrix(c(1, 2), nrow = 2), heights = c(3, 1))

par(mar = c(4, 4, 1, 0.25))

plot(y ~ x, data = d, type = 'l', cex = 0.1,  log = 'x', ylab = 'dE00 from requested neutral chip', xlab = 'constant reflectance value', axes = FALSE, xlim = c(0.025, 1))
abline(v = n, col = 'royalblue', lty = 3)
abline(h = 1.5, col = 'firebrick', lty = 2)
axis(side = 2, las = 1)
axis(side = 1, at = n, labels = round(n, 3), las = 2)
text(x = n, y = 5, labels = .m, cex = 0.75, font = 2)


par(mar = c(1, 1, 1, 1))
soilPalette(.cols, lab = .m)


# check: ok!
# dE00 all < 1
n.dE00 <- sapply(n, obj1)
summary(n.dE00)

# these should all be <1, perhaps source data were changed?
stopifnot(all(n.dE00 < 1))

# compile results and save
res <- data.frame(
  V = .chips,
  reflectance = n
)

saveRDS(res, file = 'neutral-reflectance-estimates.rds')



## cleanup
rm(list = ls())
gc(reset = TRUE)




# 
# 
# ## TODO: use intermediate objects vs. final .rda
# 
# data("munsell.spectra")
# 
# table(munsell.spectra$hue)
# 
# 
# # all hues, limit to specific hue / chroma slice
# x <- munsell.spectra[munsell.spectra$value  == 2 & munsell.spectra$chroma == 1, ]
# 
# # each Munsell chip has a 36-element spectra
# # ranging from 380-730 nm
# # table(x$munsell)
# 
# # spectra IDs
# x$ID <- factor(x$munsell)
# # create a color / chip
# cols <- parseMunsell(as.character(levels(x$ID)))
# 
# # plot style
# tps <- tactile.theme(superpose.line = list(col = cols, lwd = 2))
# 
# # final figure
# xyplot(
#   reflectance ~ wavelength, groups = ID, data = x, 
#   par.settings = tps,
#   main = 'Value 6 / Chroma 8',
#   type = c('l', 'g'),
#   ylab = 'Reflectance',
#   xlab = 'Wavelength (nm)',
#   scales = list(tick.number = 12),
#   xlim = c(370, 740)
# )




