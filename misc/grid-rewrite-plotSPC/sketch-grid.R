sketch <- function(x, cex.ids = 1, cex.names = 0.66, cex.depths = 0.66, cex.depthAxis = 0.5, widthFactor = 1, depthAxis = FALSE, depthAxis.interval = 10, fill.pattern = FALSE) {
  
  # number of profiles, possibly set as an argument
  n <- length(x)
  
  hztb <- horizonDepths(x)
  
  # calculation for profile widths, NPC
  profileWidth <- widthFactor * (100 / n) / 2 / 100
  
  
  ## viewport stuff isn't working as expected
  # https://stat.ethz.ch/R-manual/R-devel/library/grid/doc/viewports.pdf
  
  if(depthAxis) {
    ww <- unit(c(1, 1, 4), c('lines', 'null', 'lines'))
  } else {
    ww <- unit(c(1, 1, 2), c('lines', 'null', 'lines'))
  }
  
  lay <- grid.layout(
    nrow = 1, 
    ncol = 3, 
    widths = ww,
    heights = unit(c(1, 1), c('npc'))
  )
  
  ## this is not right
  # top level VP
  vp <- viewport(layout = lay)
  
  # main VP
  main.vp <- viewport(x = unit(0.5, 'npc'), y = unit(0.5, 'npc'), 
                      just = c("center", "center"),
                      width = unit(0.95, 'npc'), height = unit(0.95, 'npc'),
                      xscale = c(0, 1), yscale = c(max(x)+10, -10), 
                      name = 'main',
                      layout.pos.row = 1, layout.pos.col = 2
  )
  
  
  # depth axis VP
  depthAxis.vp <- viewport(x = unit(0.5, 'npc'), y = unit(0.5, 'npc'),
                           just = c("center", "center"),
                           width = unit(0.95, 'npc'), height = unit(0.95, 'npc'),
                           xscale = c(0, 1), yscale = c(max(x)+10, -10),
                           name = 'depthAxis',
                           layout.pos.row = 1, layout.pos.col = 3
  )
  
  # blank page
  grid.newpage()
  
  splot <- vpTree(vp, vpList(main.vp, depthAxis.vp))
  pushViewport(splot)
  
  # check viewport geom
  # showViewport(splot)
  
  # depth axis
  if(depthAxis) {
    seekViewport('depthAxis')
    .addDepthAxis(maxDepth = max(x), interval = depthAxis.interval, cex.da = cex.depthAxis)
  }
  
  # activate main VP
  seekViewport('main')
  
  
  
  # accommodation for a single profile
  if(n == 1) {
    pos <- 0.5
  } else {
    pos <- aqp:::.rescaleRange(1:n, x0 = 0.05, x1 = 0.95)
  }
  
  
  # iterate over profiles
  for(i in 1:length(x)) {
    
    # current profile
    z <- x[i, ]
    
    # current rectangle style
    gp <- gpar(fill = z$soil_color, col = 'black')
    
    # current horizon depths and derived
    tops <- z[[hztb[1]]]
    bottoms <- z[[hztb[2]]]
    thicks <- bottoms - tops 
    mids <- (tops + bottoms) / 2
    
    # current horizon and profile labels
    nm <- hzDesgn(z)
    id <- profile_id(z)
    
    # current x-position, NPC
    x.pos <- pos[i]
    
    ## TODO: patterns !
    
    
    # profiles as rectangles
    grid.rect(
      x = unit(x.pos, 'npc'), 
      y = unit(tops, 'native'), 
      just = c('center', 'bottom'), 
      width = unit(profileWidth, 'npc'), 
      height = unit(thicks, 'native'), 
      gp = gp, 
      name = sprintf('%s.shape', id)
    )
    
    
    ## TODO: is this vectorized?
    # optional pattern overlay
    if(fill.pattern) {
      
      for(hz.i in seq_along(tops)) {
        
        ## TODO: can we add some randomness to the fill between horizons / profiles?
        
        ## TODO: pattern should be selected based on rules / interpretation of hz data
        
        pat <- pattern(
          # x = runif(1, min = 0.2, max = 0.8),
          # y = runif(1, min = 0.4, max = 0.6),
          grob = pat_grob, 
          extend = 'repeat',
          height = unit(4, 'cm'),
          width = unit(4, 'cm')
        )
        
        # pattern encoded into grid graphics parameters
        gp.pattern <- gpar(fill = pat, col = NA)
        
        # apply pattern
        grid.rect(
          x = unit(x.pos, 'npc'), 
          y = unit(tops[hz.i], 'native'), 
          just = c('center', 'bottom'), 
          width = unit(profileWidth, 'npc'), 
          height = unit(thicks[hz.i], 'native'), 
          gp = gp.pattern, 
          name = sprintf('%s.shape.pattern', id)
        )
      }
    }
    
    
    
    # IDs or profile labels
    grid.text(
      label = id, 
      x = unit(x.pos, 'npc'), 
      y = unit(1, 'npc'), 
      just = c('center', 'top'), 
      gp = gpar(font = 2, cex = cex.ids),
      name = sprintf('%s.id', id)
    )
    
    # horizon designations or horizon labels
    grid.text(
      label = nm, 
      x = unit(x.pos, 'npc'), 
      y = unit(mids, 'native'), 
      just = c('center', 'center'), 
      gp = gpar(col = invertLabelColor(z$soil_color), font = 3, cex = cex.names),
      name = sprintf('%s.hzlabel', id)
    )
    
    # depths
    grid.text(
      label = tops, 
      x = unit(x.pos + (profileWidth/2) + 0.002, 'npc'), 
      y = unit(tops, 'native'), 
      just = c('left', 'center'), 
      gp = gpar(cex = cex.depths),
      name = sprintf('%s.hzdepth', id)
    )
  }
  
  # upViewport(0)
  
}


.addDepthAxis <- function(maxDepth, interval = 10, col = c('white', grey(0.3)), cex.da = 0.5) {
  
  center.npc <- 0.5
  
  # alternating bands
  zmin <- 0
  zmax <- round(maxDepth, -1)
  s <- seq(from = zmin, to = zmax, by = interval)
  
  # alternating colors
  cols <- rep(col, times = length(s))
  gp <- gpar(fill = cols, col = 'black')
  
  # largest text
  # not plotted
  tg <- textGrob(
    label = max(s), 
    x = unit(center.npc, 'npc'), 
    y = unit(s, 'native'), 
    just = c('center', 'top'), 
    gp = gpar(cex = cex.da, col = invertLabelColor(cols)),
    name = 'depthAxisTextMaxWidth'
  )
  
  w <- grobWidth(tg) + unit(0.1, 'npc')
  
  # depth tape
  grid.rect(
    x = unit(center.npc, 'npc'), 
    y = unit(s, 'native'), 
    just = c('center', 'bottom'), 
    width = w, 
    height = unit(interval, 'native'),
    gp = gp, 
    name = 'depthAxis'
  ) 
  
  # depth annotation
  grid.text(
    label = s, 
    x = unit(center.npc, 'npc'), 
    y = unit(s, 'native') - unit(0.0025, 'npc'), 
    just = c('center', 'top'), 
    gp = gpar(cex = cex.da, col = invertLabelColor(cols)),
    name = 'depthAxisText'
  )
}


