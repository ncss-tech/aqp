library(aqp)
library(raster)

x <- lapply(1:100, random_profile, method = 'LPP', SPC = TRUE)
x <- combine(x)

## TODO: will sometimes generate z+1 slices
s <- dice(x, fm = 0:100 ~ .)

z <- 1:100
var <- 'p1'

m <- matrix(
  data = horizons(s[, z])[[var]], ncol = length(s), byrow = FALSE
)

# ok
m[1, ] == s[, 1][[var]]
m[, 1] == s[1, ][[var]]

par(mar = c(1, 1, 3, 1))
image(t(m), x = 1:length(s), y = z, ylim = c(100, 0), las = 1, ylab = '', xlab = '', axes = FALSE, col = viridis::viridis(12))

r <- raster(m)
plot(r, col = viridis::viridis(12))

# ok
r[1, ] == s[, 1][[var]]

## TODO
# convert vars -> raster brick of property grids (rows = depth slice, cols = profile ID)

