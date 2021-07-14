# https://www.stat.auckland.ac.nz/~paul/useR2015-grid/grid-slides.html#1


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

# semi-graceful failures
x$top[c(4, 8, 16, 22)] <- NA
sketch(x)

horizons(x)$.sd <- 5
sketch(
  perturb(x[3, ], n = 25, thickness.attr = '.sd')
)

sketch(
  perturb(x[3, ], n = 25, thickness.attr = '.sd'), depthAxis = TRUE
)


