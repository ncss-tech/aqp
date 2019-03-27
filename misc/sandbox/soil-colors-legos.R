library(aqp)
library(farver)

# lego colors
# assuming these are sRGB / D65 colors
legos <- read.csv('https://github.com/ryantimpe/LEGOMosaics/raw/master/Colors/Lego_Colors.csv', stringsAsFactors = FALSE)

# convert
legos.sRGB <- legos[, c('R', 'G', 'B')] / 255
leogs.LAB <- convert_colour(legos.sRGB, from = 'rgb', to = 'lab', white_from = 'D65', white_to = 'D65')

# sort colors based on distances in CIE LAB spacec
previewColors(rgb(legos.sRGB))

# test conversion to Munsell
rgb2munsell(legos.sRGB)

# color to match, result is sRGB
x <- parseMunsell(c('10YR 3/4', '7.5YR 4/4', '7.5YR 5/6', '2.5Y 6/2'), return_triplets=TRUE)

# compare example color to all lego colors via CIE2000
d <- compare_colour(from=x, to=legos.sRGB, from_space='rgb', to_space = 'rgb', method='cie2000')

# sort lego colors based on CIE2000 distances from exmaple color
previewColors(rgb(legos.sRGB), col.order=order(d[1, ]), method='manual')
previewColors(rgb(legos.sRGB), col.order=order(d[2, ]), method='manual')
previewColors(rgb(legos.sRGB), col.order=order(d[3, ]), method='manual')
previewColors(rgb(legos.sRGB), col.order=order(d[4, ]), method='manual')

# check closest (perceptual) color
idx <- apply(d, 1, which.min)
legos[idx, ]

# back-transform to Munsell
rgb2munsell(legos.sRGB[idx, ])
