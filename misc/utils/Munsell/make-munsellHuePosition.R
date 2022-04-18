## Build Munsell hues position data for use in other functions / utils
## D.E. Beaudette
## 2021-019-27
##
## based on content / links found in national TN #2
## https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/ref/?cid=nrcs142p2_053569


# all 40 Munsell hues
hues <- c('5R', '7.5R', '10R',
          '2.5YR', '5YR', '7.5YR', '10YR',
          '2.5Y', '5Y', '7.5Y', '10Y',
          '2.5GY', '5GY', '7.5GY', '10GY',
          '2.5G', '5G', '7.5G', '10G',
          '2.5BG', '5BG', '7.5BG', '10BG',
          '2.5B', '5B', '7.5B', '10B',
          '2.5PB', '5PB', '7.5PB', '10PB',
          '2.5P', '5P', '7.5P', '10P',
          '2.5RP', '5RP', '7.5RP', '10RP',
          '2.5R')


# angular divisions: 40 / 360 = 9 degrees per hue
# working in radians because math is simpler
delta.theta <- ((2 * pi) / length(hues))

# angle for each hue
# starting from 5R (0 degrees) -> [x = 1, y = 0]
theta <- 0:(length(hues) - 1) * delta.theta 

# convert to rectangular coordinates for simpler plotting
# rotate origin 90 degrees CCW -> [x = 0, y = 1]
x <- -cos(theta + pi/2)
y <- sin(theta + pi/2)

# tiny numbers -> 0
x <- zapsmall(x)
y <- zapsmall(y)

# pack into DF
d <- data.frame(
  hues = hues,
  theta = theta,
  x = x,
  y = y
)

# neutral is at the center
d.N <- data.frame(
  hues = 'N',
  theta = 0,
  x = 0,
  y = 0
)

# add neutral
munsellHuePosition <- rbind(d, d.N)

# save
save(munsellHuePosition, file = '../../../data/munsellHuePosition.rda', compress = 'xz')




