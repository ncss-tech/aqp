# https://jakubnowosad.com/supercells/
library(supercells)
library(aqp)
library(terra)
library(sf)
library(farver)


## new intro
# https://jakubnowosad.com/supercells/articles/v2-intro.html


x <- rast('leaf-john-kelley.jpg', noflip = TRUE)

# assign fake CRS
crs(x) <- 'epsg:5070'


# 2026-02-26: must convert to / from CIELAB outside of supercells()
#             can provide any distance function

x.lab <- x

# simple trick to directly access / replace values of a SpatRast
values(x.lab) <- convert_colour(values(x), from = 'rgb', to = 'lab', white_from = 'D65')
names(x.lab) <- c('l', 'a', 'b')


# this returns NA compactness for many values of k...
(tn <- sc_tune_compactness(x.lab, k = 7, metric = 'local'))


# euclidean distances in LAB coordinates ~ approximates color perception

# use tuned compactness
s <- sc_slic(x.lab, compactness = tn$compactness, k = 7)

# 
# (tn <- sc_tune_compactness(x.lab, step = 25, metric = 'local'))
# s <- sc_slic(x.lab, step = 25, compactness = tn$compactness)

# manually specific compactness
s <- sc_slic(x.lab, compactness = 10, k = 7)

g <- sc_metrics_pixels(x.lab, s)
plot(g)




s <- vect(s)

plotRGB(x)
lines(s, col = 'yellow')



# # relatively fast
# # euclidean distances in LAB coordinates ~ approximates color perception
# s <- supercells(x.lab, k = 7, compactness = use_adaptive(), verbose = 2, avg_fun = median)


# # much slower
# # more accurate color contrast metric ~ CIE dE 2000
# df <- function(x, y) {
#   compare_colour(from = t(x), to = t(y), from_space = 'lab', to_space = 'lab', method = 'CIE2000', white_from = 'D65')
# }
# 
# s <- supercells(x.lab, k = 7, compactness = use_adaptive(), verbose = 2, avg_fun = median, dist_fun = df)
# 



plotRGB(x)
lines(s, col = 'yellow')

d <- as.data.frame(s)
cols <- convert_colour(d[, c('l','a', 'b')], from = 'lab', to = 'rgb', white_from = 'D65')

cols <- rgb(cols, maxColorValue = 255)

par(mfcol = c(1, 2))
plotRGB(x, mar = c(0, 0, 0, 0))
plot(s, col = cols, border = 'white', mar = c(0, 0, 0, 0), axes = FALSE)

m <- col2Munsell(d[, c('l','a', 'b')], space = 'CIELAB')
m

# question: does avg_fun work on transformed or original values?

# https://github.com/Nowosad/supercells/issues/22
