# 
# x <- data.frame(
#   id = 'a',
#   top = c(0, 25),
#   bottom = c(25, 150),
#   soil_color = 'white'
# )
# 
# depths(x) <- id ~ top + bottom
# 
# x <- duplicate(x, times = 2)


library(aqp)
library(soilDB)
library(svgparser)
library(grid)


s <- c('amador', 'drummer', 'lucy', 'cecil', 'tristan', 'pierre', 'reddig', 'zook', 'ramona', 'peterz', 'pentz', 'dylan')
x <- fetchOSD(s)


## TODO: move to saved object
# load this from an SVG -> saved .rda
# load with absolute units (mm) for fixed size
# can't use stroke = 'white' -> tiling borders become visible
pat_grob <- svgparser::read_svg(
  svg_file = 'https://raw.githubusercontent.com/davenquinn/geologic-patterns/master/assets/svg/606.svg', 
  scale = 1, 
  style_default = list(fill = grey(0.25)), 
  default.units = 'mm'
)

pat_grob$vp <- viewport(width = unit(4, 'cm'), height = unit(4, 'cm'))

# pat_grob <- svgparser::read_svg('fragment-poly.svg', scale = 1, style_default = list(fill = grey(0.25)), default.units = 'mm')

## simple version for testing
# pat_grob <- circleGrob(r=unit(2, "mm"), gp=gpar(col=NA, fill="grey"))


## windows output devices don't work
## ragg output devices don't work
##
## have to use Cairo PNG

source('sketch-grid.R')

png(file = 'test.png', width = 1000, height = 650, type = 'cairo')

sketch(x, depthAxis = TRUE, cex.names = 0.85,  cex.depths = 0.85, cex.depthAxis = 0.85)

dev.off()


