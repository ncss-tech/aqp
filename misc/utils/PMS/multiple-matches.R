library(aqp)
library(lattice)

data("pms.munsell.lut")

# find 
# which(table(pms.munsell.lut$munsell) > 5)

colors <- pms.munsell.lut[pms.munsell.lut$munsell == '5YR 5/5', ]
colors <- colors[order(colors$dE00), ]

par(mar = c(0, 0, 2, 0), fg = 'white', bg = 'black')
soilPalette(colors$hex, lab = colors$code)
title('Pantone Colors Roughly Matching 5YR 5/5', col.main = 'white', line = 0)


## this doesn't work as expected due to multiple matches

colors <- expand.grid(hue = '7.5YR', value = c(3, 5, 7), chroma = c(1, 3, 6, 8))
colors$munsell <- with(colors, sprintf('%s %s/%s', hue, value, chroma))
colors$col <- parseMunsell(colors$munsell)

g <- merge(colors, pms.munsell.lut, by = 'munsell', all.x = TRUE, sort = FALSE)


xyplot(
  value ~ factor(chroma) | factor(hue),
  data = g,
  main="Common Soil Colors", layout=c(1,1), scales=list(alternating=1),
  strip=strip.custom(bg=grey(0.85)),
  as.table=TRUE, subscripts=TRUE, xlab='Chroma', ylab='Value',
  panel=function(x, y, subscripts, ...)
  {
    panel.xyplot(x, y, pch=15, cex=8, col=g$col[subscripts])
    panel.text(x, y, g$code, col=invertLabelColor(colors$col[subscripts]))
  }
)

