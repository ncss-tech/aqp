sketch <- function(x, widthFactor = 1) {
  n <- length(x)
  
  profileWidth <- widthFactor * (100 / n) / 2 / 100

  vp <- viewport(x = unit(0.5, 'npc'), y = unit(0.5, 'npc'), 
                 just = c("center", "center"),
                 width = unit(0.95, 'npc'), height = unit(0.95, 'npc'),
                 xscale = c(0, 1), yscale = c(max(x)+10, -10)
  )
  
  # grid.show.viewport(vp)
  
  
  grid.newpage()
  pushViewport(vp)
  
  # s <- pretty(seq(0, max(x)))
  # grid.text(label = s, x = 1, y = s, default.units = 'native')
  
  if(n == 1) {
    pos <- 0.5
  } else {
    pos <- aqp:::.rescaleRange(1:n, x0 = 0.05, x1 = 0.95)
  }
  
  
  for(i in 1:length(x)) {
    
    # current profie
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
      gp = gp
    )
    
    # IDs or profile labels
    grid.text(
      label = id, 
      x = unit(x.pos, 'npc'), 
      y = unit(1, 'npc'), 
      just = c('center', 'top'), 
      gp = gpar(font = 2)
    )
    
    # horizon designations or horizon labels
    grid.text(
      label = nm, 
      x = unit(x.pos, 'npc'), 
      y = unit(mids, 'native'), 
      just = c('center', 'center'), 
      gp = gpar(col = invertLabelColor(z$soil_color), font = 3, cex = 0.66)
    )
    
    # depths
    grid.text(
      label = tops, 
      x = unit(x.pos + (profileWidth/2) + 0.002, 'npc'), 
      y = unit(tops, 'native'), 
      just = c('left', 'center'), 
      gp = gpar(cex = 0.66)
    )
  }
  
}
