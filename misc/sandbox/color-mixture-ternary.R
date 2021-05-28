library(aqp)
library(sharpshootR)
library(Ternary)

# https://cran.r-project.org/web/packages/Ternary/vignettes/Ternary.html

x <- c('2.5R 6/10', '2.5B 6/10', '2.5G 6/10')
cols <- parseMunsell(x)

# colorMixtureVenn(x, mixingMethod = 'exact')


# test 0-weights
mixMunsell(x, w = c(100, 0, 0), mixingMethod = 'exact')


g <- expand.grid(sand = seq(0, 100, by = 10), clay = seq(0, 100, by = 10))
g$silt <- pmax(100 - (g$sand + g$clay), 0)

g <- g[which(g$sand + g$silt + g$clay == 100), ]

g$munsell <- NA

for(i in 1:nrow(g)) {
  g$munsell[i] <- mixMunsell(x, w = unlist(g[i, 1:3]), mixingMethod = 'exact')$munsell
}

g$col <- parseMunsell(g$munsell)

g$sRGB <- rgb(g$sand, g$silt, g$clay, maxColorValue = 100)

par(mfcol = c(1, 3))

colorMixtureVenn(x, mixingMethod = 'exact')

TernaryPlot(alab = 'clay', blab = 'silt', clab = 'sand',
            lab.col = 'black', clockwise = TRUE,
            point = 'up', lab.cex = 0.8, grid.minor.lines = 0,
            grid.lty = 'solid', col = 'white', grid.col = grey(0.8), 
            axis.col = 'black', ticks.col = 'black',
            axis.rotate = FALSE,
            padding = 0.08
)

TernaryPoints(g[, c('clay', 'silt', 'sand')], col = g$col, pch = 16, cex = 3.5)


TernaryPlot(alab = 'clay', blab = 'silt', clab = 'sand',
            lab.col = 'black', clockwise = TRUE,
            point = 'up', lab.cex = 0.8, grid.minor.lines = 0,
            grid.lty = 'solid', col = 'white', grid.col = grey(0.8), 
            axis.col = 'black', ticks.col = 'black',
            axis.rotate = FALSE,
            padding = 0.08
)

TernaryPoints(g[, c('clay', 'silt', 'sand')], col = g$sRGB, pch = 16, cex = 3.5)



##


x <- c('10YR 2/1', '2.5R 6/10', '2.5Y 8/2')
cols <- parseMunsell(x)

colorMixtureVenn(x, mixingMethod = 'exact')

g <- expand.grid(sand = seq(0, 100, by = 20), silt = seq(0, 100, by = 20))
g$clay <- pmax(100 - (g$sand + g$silt), 0)

g <- g[which(g$sand + g$silt + g$clay == 100), ]

g$munsell <- NA

for(i in 1:nrow(g)) {
  g$munsell[i] <- mixMunsell(x, w = unlist(g[i, 1:3]), mixingMethod = 'exact')$munsell
}

g$col <- parseMunsell(g$munsell)


par(mfcol = c(1, 2))

TernaryPlot(alab = '', blab = '', clab = '',
            lab.col = 'black', clockwise = TRUE,
            point = 'up', lab.cex = 0.8, grid.minor.lines = 0,
            grid.lty = 'solid', col = 'white', grid.col = grey(0.8), 
            axis.col = 'black', ticks.col = 'black',
            axis.rotate = FALSE,
            padding = 0.08
)

TernaryPoints(g[, c('clay', 'silt', 'sand')], col = g$col, pch = 16, cex = 5)


