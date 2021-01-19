library(aqp)
library(data.table)
library(plyr)
library(microbenchmark)
# library(daff)

load(system.file("data/munsell.rda", package="aqp")[1])

# on-the-fly to/from DT
DTmerge <- function(x, y, ...) {
  x.dt <- data.table(x)
  j <- merge(x = x.dt, y = x.dt, by = c('hue','value','chroma'), sort = FALSE, all.x = TRUE)
  return(as.data.frame(j))
}

x <- base::merge(munsell, munsell, by = c('hue','value','chroma'), sort = FALSE, all.x = TRUE)
y <- plyr::join(munsell, munsell, type = 'left', by = c('hue','value','chroma'))
z <- DTmerge(m.dt, m.dt, by = c('hue','value','chroma'), sort = FALSE, all.x = TRUE)

# names have been made unique
head(x)

# names are not unique !
head(y)

# names have been made unique
head(z)


## this will cause RStudio preview window to hang... why!? 
# d <- diff_data(x, y)
# render_diff(d)

# plyr::join 2x faster than base::merge
# data.table::merge (with conversion to/from) 5x faster than base::merge

microbenchmark(
'base::merge' = base::merge(munsell, munsell, by = c('hue','value','chroma'), sort = FALSE, all.x = TRUE),
'plyr::join' = plyr::join(munsell, munsell, type = 'left', by = c('hue','value','chroma')),
'data.table::merge' = DTmerge(m.dt, m.dt, by = c('hue','value','chroma'), sort = FALSE, all.x = TRUE)
)




