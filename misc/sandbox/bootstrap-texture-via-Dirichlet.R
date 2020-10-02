library(aqp)
library(soilDB)
library(compositions)
library(soiltexture)
library(dplyr)

# sample data
data('sp6')
depths(sp6) <- id ~ top + bottom

# I still like this
ssc <- horizons(sp6)[grep('^Bt', sp6$name), c('sand', 'silt', 'clay')]
names(ssc) <- toupper(names(ssc))

# ok fine, I'll try dplyr
ssc <- horizons(sp6) %>%
  filter(grepl('^Bt', x = name)) %>%
  select(
    SAND = sand,
    SILT = silt,
    CLAY = clay
  )

# simulate 100 samples
s <- bootstrapSoilTexture(ssc, n = 100)
s <- s$samples

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
TT.points(tri.data = s, geo = TT, col='firebrick', pch = 3, cex = 0.5, lwd = 1, tri.sum.tst = FALSE)
TT.points(tri.data = ssc, geo = TT, bg='royalblue', pch = 22, cex = 1, lwd = 1, tri.sum.tst = FALSE)

legend('top', legend = c('Source', 'Simulated'), pch = c(22, 3), col = c('black', 'firebrick'), pt.bg = c('royalblue', NA), horiz = TRUE, bty = 'n')


# source data
(ssc.stats <- textureTriangleSummary(ssc, pch = 1, cex = 0.5, range.alpha = 50))

# simulated data
(ssc.stats.boot <- textureTriangleSummary(s, pch = 1, cex = 0.5, range.alpha = 50))

# marginal quantiles are pretty close
round(ssc.stats - ssc.stats.boot)


# sweet!
# you can add to one of these figures
textureTriangleSummary(s, pch = 1, cex = 0.5, range.alpha = 50, col = grey(0.5), legend = FALSE)
TT <- TT.geo.get()
TT.points(tri.data = ssc, geo = TT, col = 'royalblue', pch = 16, cex = 0.5, lwd = 1, tri.sum.tst = FALSE)





