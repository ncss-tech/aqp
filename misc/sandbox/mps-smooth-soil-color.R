library(aqp)
library(mpspline2)

# example soil profile with some wild colors
x <- list(
  id = 'P1',
  depths = c(5, 25, 33, 100, 150),
  name = c('A', 'Bt1', 'Bt2', 'BC', 'Cr'),
  p1 = c(5, 25, 35, 10, 8),
  color = c('10YR 2/1', '7.5YR 3/3', '2.5Y 8/2', '2.5YR 4/6', '5G 6/3'),
  hzd = c('clear', 'clear', 'abrupt', 'gradual', NA)
)

# init SPC
x <- quickSPC(x)
x$hzd <- hzDistinctnessCodeToOffset(x$hzd)

# convert Munsell -> sRGB in hex notation
x$col_source <- parseMunsell(x$color)

# convert Munsell -> CIELAB
.lab <- parseMunsell(x$color, convertColors = TRUE, returnLAB  = TRUE)

# shortcut to splice-in CIELAB color coordinates
replaceHorizons(x) <- cbind(horizons(x), .lab)

# check
plotSPC(x, color = 'L', hz.distinctness.offset = 'hzd')

# hack to smooth multiple variables
# future enhancement to spc2mpspline()
.lambda <- 0.1
.spcL <- spc2mpspline(x, var_name = 'L', lam = .lambda, method = 'est_1cm')
.spcA <- spc2mpspline(x, var_name = 'A', lam = .lambda, method = 'est_1cm')
.spcB <- spc2mpspline(x, var_name = 'B', lam = .lambda, method = 'est_1cm')

m <- .spcL
m$A_spline <- .spcA$A_spline
m$B_spline <- .spcB$B_spline


# check
# ... negative numbers truncated at 0
par(mar = c(0, 0, 3, 3))
plotSPC(m, color = 'L_spline', name = NA, lwd = 0, divide.hz = FALSE)
plotSPC(m, color = 'A_spline', name = NA, lwd = 0, divide.hz = FALSE)
plotSPC(m, color = 'B_spline', name = NA, lwd = 0, divide.hz = FALSE)

# back-transform to Munsell at this point
.lab <- horizons(m)[, c('L_spline', 'A_spline', 'B_spline')]
names(.lab) <- c('L', 'A', 'B')

# interesting...
.mun <- col2Munsell(.lab, space = 'CIELAB')
table(.mun$hue)
table(.mun$value)
table(.mun$chroma)

# convert smoothed CIELAB -> sRGB
.srgb <- convertColor(horizons(m)[, c('L_spline', 'A_spline', 'B_spline')], from = 'Lab', to = 'sRGB', from.ref.white = 'D65', to.ref.white = 'D65')

# sRGB -> hex notation
m$col_spline <- rgb(.srgb, maxColorValue = 1)

# ok
plotSPC(m, color = 'col_spline', name = NA, lwd = 0, divide.hz = FALSE)

# normalize names and combine SPCs
m$soil_color <- m$col_spline
x$soil_color <- x$col_source

profile_id(m) <- 'P1-EA Spline'

z <- combine(x, m)

# compare side by side
par(mar = c(0, 0, 0, 3))
plotSPC(z, color = 'soil_color', name = NA, lwd = 0, divide.hz = FALSE, cex.names = 1)

# green hues lost due to truncation of smoothed values at x>=0

