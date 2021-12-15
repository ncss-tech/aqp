# https://www.stat.auckland.ac.nz/~paul/useR2015-grid/grid-slides.html#1


## patterns
# https://davenquinn.com/projects/geologic-patterns/
# https://github.com/davenquinn/geologic-patterns/tree/master/assets/svg

# https://github.com/coolbutuseless/svgparser
# https://coolbutuseless.github.io/package/svgparser/articles/tileable.html

# https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/definitions/definitions.html



library(aqp)
library(soilDB)
library(grid)

source('sketch-grid.R')

x <- fetchOSD(c('sierra'))
sketch(x)
sketch(x, depthAxis = TRUE)

x <- fetchOSD(c('amador', 'gillender', 'pentz'))
sketch(x)
sketch(x, depthAxis = TRUE)


x <- fetchOSD(c('amador', 'drummer', 'lucy', 'cecil', 'tristan'))
sketch(x)
sketch(x, depthAxis = TRUE)


x <- fetchOSD(c('amador', 'drummer', 'lucy', 'cecil', 'tristan', 'pierre', 'reddig', 'zook', 'ramona', 'peterz', 'pentz', 'dylan'))
sketch(x)

sketch(x, depthAxis = TRUE)


grid.ls()

current.vpTree()


horizons(x)$.sd <- 5
sketch(
  perturb(x[3, ], n = 25, thickness.attr = '.sd')
)

sketch(
  perturb(x[3, ], n = 25, thickness.attr = '.sd'), depthAxis = TRUE
)


# semi-graceful failures
x$top[c(4, 8, 16, 22)] <- NA
sketch(x)
