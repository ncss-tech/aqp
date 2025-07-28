
## TODO:
# * use matrix / array 


library(aqp)
library(terra)

x <- rp(100, method = 'LPP')

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

.cols <- hcl.colors(n = 12)

image(
  z = t(m), 
  x = 1:length(s), 
  y = z, 
  ylim = c(100, 0), 
  las = 1, 
  ylab = '', 
  xlab = '', 
  axes = FALSE, 
  col = .cols
)

r <- rast(m)
plot(r, col = .cols)

# ok
r[1, ] == s[, 1][[var]]

## TODO
# convert vars -> raster brick of property grids (rows = depth slice, cols = profile ID)

