library(aqp)
library(munsellinterpol)


data(munsell)
colors <- with(munsell, sprintf('%s %s/%s', hue, value, chroma))

cb <- ColorBlockFromMunsell(colors)

par(bg = 'black', fg = 'white')
soilPalette(getClosestMunsellChip(colors[1:8]), lab = ColorBlockFromMunsell(colors[1:8])$Name, lab.cex = 0.5)



idx <- grep('^light brown$', cb$Name)

cb[idx, ]

col <- parseMunsell(colors[idx])

cq <- colorQuantiles(col)
plotColorQuantiles(cq)

soilPalette(colors = col, lab = colors[idx])
title('"light brown"', col.main = 'white', font.main = 4)



data(traditionalColorNames)

sort(table(traditionalColorNames$traditional_name))

idx <- which(traditionalColorNames$traditional_name == 'pale yellow')
col <- parseMunsell(traditionalColorNames$munsell[idx])

soilPalette(colors = col, lab = traditionalColorNames$munsell[idx])
title('"pale yellow"', col.main = 'white', font.main = 4)

cq <- colorQuantiles(col)
plotColorQuantiles(cq)
