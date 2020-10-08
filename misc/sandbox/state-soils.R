library(aqp)
library(soilDB)

data('us.state.soils')

x <- fetchOSD(us.state.soils$series)

us.state.soils$series <- toupper(us.state.soils$series)
names(us.state.soils) <- c('state', 'abbreviated', 'id')
site(x) <- us.state.soils

o <- order(x$abbreviated)

par(mar = c(0,0,0,0))
plotSPC(
  x, 
  plot.depth.axis = TRUE, 
  axis.line.offset = -5,
  name = NA, 
  label = 'abbreviated', 
  width = 0.35, 
  id.style = 'top', 
  plot.order = o
)

mtext('aqp::us.state.soils', side = 1, at = 0, font = 2, line = -2, adj = 0)
