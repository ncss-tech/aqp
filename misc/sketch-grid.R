sketch <- function(x, widthFactor = 1, depthAxis = FALSE) {
  
  # number of profiles, possibly set as an argument
  n <- length(x)
  
  # calculation for profile widths, NPC
  profileWidth <- widthFactor * (100 / n) / 2 / 100

  if(depthAxis) {
    lay <- grid.layout(nrow = 1, ncol = 2, widths = c(0.95, 0.05))
  } else {
    lay <- grid.layout(nrow = 1, ncol = 1, widths = 1)
  }
  
  
  
  # main VIP
  main.vp <- viewport(x = unit(0.5, 'npc'), y = unit(0.5, 'npc'), 
                 just = c("center", "center"),
                 width = unit(0.95, 'npc'), height = unit(0.95, 'npc'),
                 xscale = c(0, 1), yscale = c(max(x)+10, -10), 
                 layout.pos.row = 1, layout.pos.col = 1
  )
  

  # depth axis VP
  depthAxis.vp <- viewport(x = unit(0.5, 'npc'), y = unit(0.5, 'npc'),
                      just = c("center", "center"),
                      width = unit(0.95, 'npc'), height = unit(0.95, 'npc'),
                      xscale = c(0, 1), yscale = c(max(x)+10, -10),
                      layout.pos.row = 1, layout.pos.col = 2
  )

  
  # blank page
  grid.newpage()
  pushViewport(viewport(layout = lay))
  
  ## check viewport geom
  # showViewport(main.vp)
  
  # depth axis
  if(depthAxis) {
    
    pushViewport(depthAxis.vp)
    .addDepthAxis(maxDepth = max(x), interval = 10)
    popViewport()
  }
  
  # activate main VP
  pushViewport(main.vp)
  
  
  
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
    hztb <- horizonDepths(z)
    
    tops <- z[[hztb[1]]]
    bottoms <- z[[hztb[2]]]
    thicks <- bottoms - tops 
    mids <- (tops + bottoms) / 2
    
    # current horizon and profile labels
    nm <- hzDesgn(z)
    id <- profile_id(z)
    
    # current x-position, NPC
    x.pos <- pos[i]
    
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
    
    # IDs or profile labels
    grid.text(
      label = id, 
      x = unit(x.pos, 'npc'), 
      y = unit(1, 'npc'), 
      just = c('center', 'top'), 
      gp = gpar(font = 2),
      name = sprintf('%s.id', id)
    )
    
    # horizon designations or horizon labels
    grid.text(
      label = nm, 
      x = unit(x.pos, 'npc'), 
      y = unit(mids, 'native'), 
      just = c('center', 'center'), 
      gp = gpar(col = invertLabelColor(z$soil_color), font = 3, cex = 0.66),
      name = sprintf('%s.hzlabel', id)
    )
    
    # depths
    grid.text(
      label = tops, 
      x = unit(x.pos + (profileWidth/2) + 0.002, 'npc'), 
      y = unit(tops, 'native'), 
      just = c('left', 'center'), 
      gp = gpar(cex = 0.66),
      name = sprintf('%s.hzdepth', id)
    )
  }
  
}


.addDepthAxis <- function(maxDepth, interval = 10, col = c('white', grey(0.3))) {
  
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
    gp = gpar(cex = 0.5, col = invertLabelColor(cols)),
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
    y = unit(s, 'native'), 
    just = c('center', 'top'), 
    gp = gpar(cex = 0.5, col = invertLabelColor(cols)),
    name = 'depthAxisText'
  )
}


