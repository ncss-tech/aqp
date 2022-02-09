

# x: SPC
# sw: column containing surface water data
# sf: scaling factor for surface water depths
# sw.axis.line: horizontal positioning for surface water axis
# sw.axis.cex: text scaling for surface wate axis
addSurfaceWater <- function(x, sw = 'swaterdepth', sw.axis.line = -1.5, sw.axis.cex = 0.5, sw.axis.n = 8, sw.axis.title = 'Water Depth', sw.axis.title.line = -1.5, water.col = '#4169E180') {
  
  # last plot configuration
  lsp <- get('last_spc_plot', envir = aqp.env)
  
  # get site data
  s <- site(x)
  # get surface water depths
  swd <- s[[sw]]
  # re-order as needed
  swd <- swd[lsp$plot.order]
  
  # scaling factor from last plot
  sf <- lsp$scaling.factor
  
  # re-scale according to scaling factors
  # * surface water depth scaling factor
  swd.scaled <- (swd * sf)
  
  # water column left/right sides are set by plot width
  w.left <- 1:length(x) - lsp$width
  w.right <- 1:length(x) + lsp$width
  
  # bottom and top are related to y.offset and scaling factor of surface water depth
  w.bottom <- lsp$y.offset
  w.top <- (-1 * swd.scaled) + lsp$y.offset
  
  # add simple water columns
  rect(xleft = w.left, xright = w.right, ybottom = w.bottom, ytop = w.top, col = water.col)
  
  # axis labels, original scale
  swd.labs <- pretty(c(0, max(swd, na.rm = TRUE)), n = sw.axis.n)
  swd.labs.txt <- sprintf("%s %s", swd.labs, depth_units(x))
  
  ## TODO: adapt to use vector of y offsets
  # axis ticks, plot scale
  swd.ticks <- (swd.labs * sf)
  
  # surface water depth axis
  axis(side = 2, at = swd.ticks, labels = swd.labs.txt, line = sw.axis.line, las=1, cex.axis=sw.axis.cex)
  
  # annotate
  mtext(side = 3, at = 0, text = sw.axis.title, line = sw.axis.title.line, adj = 0, cex = 0.75)
  
}

