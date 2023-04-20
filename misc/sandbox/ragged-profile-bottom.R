library(aqp)
library(soilDB)

x1 <- 0.25
x2 <- 0.75
y <- 25

p1 <- cbind(x1, y)
p2 <- cbind(x2, y)

.r <- aqp:::.raggedLines(x1, x2, y, n = 16)


plot(1, 1, xlim = c(0, 5), ylim = c(50, 0), type = 'n')
points(p1, pch = 16)
points(p2, pch = 16)
segments(x0 = .r[-nrow(.r), 1], x1 = .r[-1, 1], y0 = .r[-nrow(.r), 2], y1 = .r[-1, 2])




# some interesting soil series
s <- c('leon', 'musick', 'clarksville', 'pardee', 'lucy', 'pierre', 'drummer', 'zook', 'san joaquin')

# get basic morphology and extended data from SoilWeb cache
osds.full <- fetchOSD(s, extended = TRUE)

# save copy of SoilProfileCollection for later
osds <- osds.full$SPC


data("rowley2019")

# not so interesting, these all share the same texture
rowley2019$texture <- ssc_to_texcl(sand = rowley2019$Sand, clay = rowley2019$Clay)

## TODO: condition the use of \n on horizon thickness:
##       thin horizons use namne : texture : pH

# composite horizon ID
rowley2019$hz.label <- sprintf(
  "%s\n%s: %s", 
  rowley2019$name, 
  toupper(rowley2019$texture), 
  sprintf("%.1f", rowley2019$pH)
)

horizons(rowley2019)$hzd <- 2
horizons(rowley2019)$hzto <- 4


par(mar = c(0, 0, 0, 2))


plotSPC(
  rowley2019[1, ], 
  width = 0.25, 
  name.style = 'center-center', 
  name = 'hz.label', 
  cex.names = 0.85,
  n = 10
)

plotSPC(
  rowley2019[1:3, ], 
  width = 0.25, 
  name.style = 'center-center', 
  name = 'hz.label', 
  cex.names = 0.85
)


plotSPC(
  rowley2019, 
  width = 0.25, 
  name.style = 'center-center', 
  name = 'hz.label', 
  cex.names = 0.85
)

plotSPC(
  rowley2019, 
  width = 0.25, 
  name.style = 'center-center', 
  name = 'hz.label', 
  cex.names = 0.85,
  divide.hz = FALSE,
  color = 'Ca_exch'
)

plotSPC(
  rowley2019, 
  width = 0.25, 
  name.style = 'center-center', 
  name = 'hz.label', 
  cex.names = 0.85,
  color = 'Ca_exch'
)

plotSPC(
  rowley2019, 
  width = 0.25, 
  name.style = 'center-center', 
  name = 'hz.label', 
  cex.names = 0.85,
  color = 'Ca_exch', 
  hz.distinctness.offset = 'hzd', hz.topography.offset = 'hzto'
)


plotSPC(
  rowley2019, 
  width = 0.25, 
  name.style = 'center-center', 
  name = 'hz.label', 
  cex.names = 0.85,
  color = 'Ca_exch',
  max.depth = 50
)


site(rowley2019)$rb <- c(TRUE, FALSE) 

plotSPC(
  rowley2019, 
  width = 0.25, 
  name.style = 'center-center', 
  name = 'hz.label', 
  cex.names = 0.85,
  color = 'Ca_exch',
  max.depth = 50,
  raggedBottom = 'rb'
)




# use offset values instead of horizon designations to emphasize effect
par(mar = c(0, 0, 0, 1))
plotSPC(osds, cex.names = 0.66, name.style = 'center-center', width = 0.33, hz.distinctness.offset = 'hzd', max.depth = 150)

plotSPC(osds, cex.names = 0.66, name.style = 'center-center', width = 0.25, hz.distinctness.offset = 'hzd')

plotSPC(osds, cex.names = 0.66, name.style = 'center-center', width = 0.45, hz.distinctness.offset = 'hzd')


plotSPC(osds[, 1:2], cex.names = 0.66, name.style = 'center-center', width = 0.45, hz.distinctness.offset = 'hzd')



osds$hzto <- hzTopographyCodeToOffset(osds$topography)
osds$hzto.lty <- hzTopographyCodeToLineType(osds$topography)

plotSPC(osds, cex.names = 0.66, name.style = 'center-center', width = 0.33, hz.topography.offset = 'hzto', hz.boundary.lty = 'hzto.lty')

plotSPC(osds, cex.names = 0.66, name.style = 'center-center', width = 0.33, hz.topography.offset = 'hzto', hz.boundary.lty = 'hzto.lty', hz.distinctness.offset = 'hzd', max.depth = 150)




p <- perturb(osds[1, ], boundary.attr = 'hzd', n = 60)

site(p)$tr <- c(TRUE, FALSE)


plotSPC(p[1:2, ], cex.names = 0.66, name.style = 'center-center', width = 0.1, hz.distinctness.offset = 'hzd', raggedBottom = 'tr')

plotSPC(p[1:2, ], cex.names = 0.66, name.style = 'center-center', width = 0.25, hz.distinctness.offset = 'hzd', raggedBottom = 'tr', n = 3)


plotSPC(p[1:2, ], cex.names = 0.66, name.style = 'center-center', width = 0.25, hz.distinctness.offset = 'hzd', raggedBottom = 'tr', n = 3, divide.hz = FALSE)



plotSPC(p[1:10, , .FIRST], cex.names = 0.66, name.style = 'center-center', width = 0.25, hz.distinctness.offset = 'hzd', raggedBottom = 'tr')


plotSPC(p[1:10, , .FIRST], cex.names = 0.66, name.style = 'center-center', width = 0.25, hz.distinctness.offset = 'hzd', raggedBottom = 'tr', divide.hz = FALSE)

plotSPC(p[1:10, , .FIRST], cex.names = 0.66, name.style = 'center-center', width = 0.25, hz.distinctness.offset = 'hzd')


plotSPC(p[1, ], cex.names = 0.66, name.style = 'center-center', width = 0.1, hz.distinctness.offset = 'hzd')

plotSPC(p[1, ], cex.names = 0.66, name.style = 'center-center', width = 0.33, hz.distinctness.offset = 'hzd')

plotSPC(p[1:5, ], cex.names = 0.66, name.style = 'center-center', width = 0.33, hz.distinctness.offset = 'hzd')

plotSPC(p[1:10, ], cex.names = 0.66, name.style = 'center-center', width = 0.33, hz.distinctness.offset = 'hzd')



plotSPC(p[1:20, ], cex.names = 0.66, name.style = 'center-center', width = 0.33, hz.distinctness.offset = 'hzd')

plotSPC(p[1:40, ], cex.names = 0.66, name.style = 'center-center', width = 0.33, hz.distinctness.offset = 'hzd')

plotSPC(p[1:60, ], cex.names = 0.66, name.style = 'center-center', width = 0.33, hz.distinctness.offset = 'hzd')




