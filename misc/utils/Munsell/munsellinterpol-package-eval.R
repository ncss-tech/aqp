
# https://cran.r-project.org/web/packages/munsellinterpol/vignettes/munsellinterpol-guide.html

library(aqp)
library(munsellinterpol)
library(farver)

library(microbenchmark)

# input for Munsell -> sRGB test
data(munsell)
colors <- with(munsell, sprintf('%s %s/%s', hue, value, chroma))

# relatively fast, using 100 replications
b <- microbenchmark(
  aqp = munsell2rgb(munsell$hue, munsell$value, munsell$chroma),
  munsellinterpol = MunsellTosRGB(colors)
)

# input for the back transform test
# note quite the same thing
sRGB <- munsell2rgb(munsell$hue, munsell$value, munsell$chroma, return_triplets = TRUE)
xyY <- MunsellTosRGB(colors)$xyY

# # this takes a really long time
# b2 <- microbenchmark(
#   aqp = rgb2munsell(sRGB),
#   munsellinterpol = xyYtoMunsell(xyY),
#   times = 3 
# )
# 
# b2
# 

# this is cool
par( omi=c(0,0,0,0), mai=c(0.6,0.7,0.4,0.2) )
plotPatchesH( "10YR", back='#f7f7f7' )


ColorBlockFromMunsell('10YR 3/4')

# ok, how bad of an estimate does getClosestMunsellChip() return on some non-standard chips?

# good examples in here
data(sp6)
colors <- sp6$color


# nice
ColorBlockFromMunsell(colors)

par(bg = 'black', fg = 'white')
soilPalette(getClosestMunsellChip(colors[1:8]), lab = ColorBlockFromMunsell(colors[1:8])$Name, lab.cex = 0.5)



# aqp estimated colors, NN matching of hue, rounding value / chroma
aqp.cols <- getClosestMunsellChip(colors)

# munsellinterpol estimates
# real-deal, this is the best you can expect
y <- MunsellTosRGB(colors)
munsellinterpol.cols <- rgb(y$RGB, maxColorValue = 255)

# wow, very close
par(bg = 'black', fg = 'white')
colorspace::swatchplot(
  `aqp::getClosestMunsellChip` = aqp.cols, 
  `munsellinterpol::MunsellTosRGB` = munsellinterpol.cols
)


# how close?
# eval with dE00
aqp.sRGB <- getClosestMunsellChip(colors, return_triplets = TRUE)

d <- vector(mode = 'numeric', length = nrow(aqp.sRGB))

# note: farver::compare_color expects sRGB values in 0-255 range
for(i in 1:nrow(aqp.sRGB)) {
  d[i] <- compare_colour(from = aqp.sRGB[i, ] * 255, to = rbind(y$RGB[i, ]), from_space = 'rgb', to_space = 'rgb', method = 'CIE2000')
}

# very close!
summary(d)

contrastChart('10YR 3/3', hues = '10YR')
contrastChart('10YR 3/3', hues = c('10YR', '2.5Y', '7.5YR'), thresh = 4)


colorContrastPlot('10G 8/6', '10R 8/6')
