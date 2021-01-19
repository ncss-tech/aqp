library(aqp)
library(data.table)
library(plyr)
library(microbenchmark)
library(daff)

load(system.file("data/munsell.rda", package="aqp")[1])

m.dt <- data.table(munsell)


x <- base::merge(munsell, munsell, by = c('hue','value','chroma'), sort = FALSE, all.x = TRUE)
y <- plyr::join(munsell, munsell, type = 'left', by = c('hue','value','chroma'))
z <- merge(m.dt, m.dt, by = c('hue','value','chroma'), sort = FALSE, all.x = TRUE)

all.equal(x, y)
d <- diff_data(x, y)
render_diff(d)



microbenchmark(
  'base::merge' = base::merge(munsell, munsell, by = c('hue','value','chroma'), sort = FALSE, all.x = TRUE),
  'plyr::join' = plyr::join(munsell, munsell, type = 'left', by = c('hue','value','chroma')),
  'data.table::merge' = merge(m.dt, m.dt, by = c('hue','value','chroma'), sort = FALSE, all.x = TRUE)
  )



