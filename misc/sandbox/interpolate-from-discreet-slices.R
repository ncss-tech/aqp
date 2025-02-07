library(aqp)
library(soilDB)


x1 <- list(
  id = 'A',
  depths = c(5, 18, 33, 66, 165),
  name = c('Ap', 'AB',  'Bw', 'Bt', 'Cr'),
  clay = c(8, 15,  16, 25, 5),
  soil_color = c('2.5Y 2/1', '10YR 3/3', '10YR 4/4', '10YR 4/6', '5G 6/2')
)

x2 <- list(
  id = 'B',
  depths = c(5, 25, 33, 95, 165),
  name = c('Ap', 'AB',  'Bw', 'Bt', 'Cr'),
  clay = c(8, 15,  16, 25, 5),
  soil_color = c('2.5Y 2/1', '10YR 3/3', '10YR 4/4', '10YR 4/6', '5G 6/2')
)


# x$clay <- scale(x$clay)

s1 <- quickSPC(x1)
s2 <- quickSPC(x2)

# plotSPC(s, name.style = 'center-center', cex.names = 1)

# plotSPC(s, name.style = 'center-center', cex.names = 1, color = 'clay')

d1 <- dice(s1, c(0, 5, 15, 30, 60, 100, 150) ~ .)
d2 <- dice(s2, c(0, 5, 15, 30, 60, 100, 150) ~ .)

profile_id(d1) <- "A'"
profile_id(d2) <- "B'"

# plotSPC(d, name = '', cex.names = 1, color = 'clay')


a1 <- approxfun(d1$top, d1$clay, yleft = d1$clay[1], yright = d1$clay[length(d1$clay)], method = 'linear')
a2 <- approxfun(d2$top, d2$clay, yleft = d2$clay[1], yright = d2$clay[length(d2$clay)], method = 'linear')

dd1 <- dice(s1)
dd1$clay <- a1(dd1$top)

dd2 <- dice(s2)
dd2$clay <- a2(dd2$top)

profile_id(dd1) <- "A''"
profile_id(dd2) <- "B''"

# plotSPC(d1, name = '', cex.names = 1, color = 'clay', divide.hz = FALSE)

diff <- dd1
diff$clay <- dd1$clay - dd2$clay
profile_id(diff) <- "A'' - B''"

g <- c(s1, s2, d1, d2, dd1, dd2, diff)

.cols <- c('grey', hcl.colors(10, 'spectral', rev = TRUE))
par(mar = c(0, 0, 3, 2.5))
plotSPC(g, name = '', cex.names = 1, color = 'clay', divide.hz = FALSE, width = 0.33, depth.axis = list(line = -3), max.depth = 155, col.palette = .cols, n.legend = 8)


slab(g, id ~ clay, slab.structure = c(0, 25), slab.fun = mean, na.rm = TRUE)




