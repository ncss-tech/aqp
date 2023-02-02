library(aqp)
library(soilDB)
library(compositions)
library(soiltexture)
library(dplyr)


# sand loam
.sand <- jitter(rep(65, times = 10), amount = 5)
.clay <- jitter(rep(12, times = 10), amount = 5)
.silt <- 100 - .sand - .clay

ssc <- data.frame(SAND = .sand, SILT = .silt, CLAY = .clay)

# simulate 20 samples
.n <- 20
s.d <- bootstrapSoilTexture(ssc, n = .n, method = 'dirichlet')$samples
s.n <- bootstrapSoilTexture(ssc, n = .n, method = 'normal')$samples

# empty soil texture triangle
TT <- TT.plot(
  class.sys= "USDA-NCSS.TT",
  main= "",
  tri.sum.tst=FALSE,
  cex.lab=0.75,
  cex.axis=0.75,
  frame.bg.col='white',
  class.lab.col='black',
  lwd.axis=1.5,
  arrows.show=TRUE,
  new.mar = c(3, 0, 0, 0)
)

# add original / simulated data
TT.points(tri.data = s.d, geo = TT, col='firebrick', pch = 3, cex = 0.5, lwd = 1, tri.sum.tst = FALSE)
TT.points(tri.data = s.n, geo = TT, col='darkgreen', pch = 1, cex = 0.5, lwd = 1, tri.sum.tst = FALSE)
TT.points(tri.data = ssc, geo = TT, bg='royalblue', pch = 22, cex = 1, lwd = 1, tri.sum.tst = FALSE)

legend('top', legend = c('Source', 'Dirichlet', 'Multivariate Normal'), pch = c(22, 3, 1), col = c('black', 'firebrick', 'darkgreen'), pt.bg = c('royalblue', NA, NA), horiz = TRUE, bty = 'n')




# sample data
data('sp4')
depths(sp4) <- id ~ top + bottom

# I still like this
ssc <- horizons(sp4)[grep('^Bt', sp4$name), c('sand', 'silt', 'clay')]
names(ssc) <- toupper(names(ssc))

# ok fine, I'll try dplyr
ssc <- horizons(sp4) %>%
  filter(grepl('^Bt', x = name)) %>%
  select(
    SAND = sand,
    SILT = silt,
    CLAY = clay
  )

# simulate 100 samples
s.d <- bootstrapSoilTexture(ssc, n = 100, method = 'dirichlet')$samples
s.n <- bootstrapSoilTexture(ssc, n = 100, method = 'normal')$samples


# empty soil texture triangle
TT <- TT.plot(
  class.sys= "USDA-NCSS.TT",
  main= "",
  tri.sum.tst=FALSE,
  cex.lab=0.75,
  cex.axis=0.75,
  frame.bg.col='white',
  class.lab.col='black',
  lwd.axis=1.5,
  arrows.show=TRUE,
  new.mar = c(3, 0, 0, 0)
)

# add original / simulated data
TT.points(tri.data = s.d, geo = TT, col='firebrick', pch = 3, cex = 0.5, lwd = 1, tri.sum.tst = FALSE)
TT.points(tri.data = s.n, geo = TT, col='darkgreen', pch = 1, cex = 0.5, lwd = 1, tri.sum.tst = FALSE)
TT.points(tri.data = ssc, geo = TT, bg='royalblue', pch = 22, cex = 1, lwd = 1, tri.sum.tst = FALSE)

legend('top', legend = c('Source', 'Dirichlet', 'Multivariate Normal'), pch = c(22, 3, 1), col = c('black', 'firebrick', 'darkgreen'), pt.bg = c('royalblue', NA, NA), horiz = TRUE, bty = 'n')

# source data
(ssc.stats <- textureTriangleSummary(ssc, pch = 1, cex = 0.5, range.alpha = 50))

# simulated data
(ssc.stats.boot.d <- textureTriangleSummary(s.d, pch = 1, cex = 0.5, range.alpha = 50))

(ssc.stats.boot.n <- textureTriangleSummary(s.n, pch = 1, cex = 0.5, range.alpha = 50))

# marginal quantiles are pretty close
round(ssc.stats - ssc.stats.boot.d)
round(ssc.stats - ssc.stats.boot.n)

# sweet!
# you can add to one of these figures
textureTriangleSummary(s.d, pch = 1, cex = 0.5, range.alpha = 50, col = grey(0.5), legend = FALSE)
TT <- TT.geo.get()
TT.points(tri.data = ssc, geo = TT, col = 'royalblue', pch = 16, cex = 0.5, lwd = 1, tri.sum.tst = FALSE)



## try it with larger data set
# hmm..
data(sp5)

ssc <- trunc(sp5, 0, 10) %>%
  horizons(.) %>%
  select(
    .,
    SAND = sand,
    SILT = silt,
    CLAY = clay
  )


idx <- sample(1:nrow(ssc), size = 50)
ssc.subset <- ssc[idx, ]

s.d <- bootstrapSoilTexture(ssc.subset, n = nrow(ssc))$samples
s.n <- bootstrapSoilTexture(ssc.subset, n = nrow(ssc), method = 'normal')$samples


par(mfrow = c(1, 3))

stats.full <- textureTriangleSummary(ssc, pch = 1, cex = 0.5, range.alpha = 50, col = grey(0.5), legend = FALSE, main = 'Original')

# stats.sub <- textureTriangleSummary(ssc.subset, pch = 1, cex = 0.5, range.alpha = 50, col = grey(0.5), legend = FALSE, main = 'Subset')

stats.sim.d <- textureTriangleSummary(s.d, pch = 1, cex = 0.5, range.alpha = 50, col = grey(0.5), legend = FALSE, main = 'Dirichlet')

stats.sim.n <- textureTriangleSummary(s.n, pch = 1, cex = 0.5, range.alpha = 50, col = grey(0.5), legend = FALSE, main = 'Multivariate Normal')


dev.off()


## Much to think about / document / comb through some literature

stats.sim.d <- textureTriangleSummary(ssc, pch = 1, cex = 0.5, range.alpha = 50, col = grey(0.5), legend = FALSE, main = 'Hmmm')
TT <- TT.geo.get()
TT.points(tri.data = s.d, geo = TT, col = 'royalblue', pch = 16, cex = 0.5, lwd = 1, tri.sum.tst = FALSE)
TT.points(tri.data = s.n, geo = TT, col = 'firebrick', pch = 16, cex = 0.25, lwd = 1, tri.sum.tst = FALSE)


