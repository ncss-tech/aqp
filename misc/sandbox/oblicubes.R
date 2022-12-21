# https://github.com/trevorld/oblicubes

library(isocubes)
library(grid)
library(aqp)
library(soilDB)
library(purrr)

cubit <- function(i, max.depth = 150, n.sim = 40, n.fuzz = 10, ...) {
  
  x <- fetchOSD(i, colorState = 'moist')
  x <- trunc(x, 0, max.depth)
  
  # convert horizon distinctness codes into reasonable depth offsets
  x$hd <- hzDistinctnessCodeToOffset(
    x$distinctness, 
    codes = c('very abrupt', 'abrupt', 'clear', 'gradual', 'diffuse')
  )
  
  # number of new IDs sets the number of realizations
  s <- perturb(
    x, 
    id = sprintf("Sim. %02d", 1:n.sim),
    boundary.attr = 'hd', 
    min.thickness = 5
  )
  
  s <- dice(s)
  
  p <- profileApply(s, function(d) {
    
    .id <- match(profile_id(d), profile_id(s))
    
    coords <- data.frame(
      x = .id, 
      y = (max.depth - d$bottom) / 3, 
      z = sample(1:n.fuzz, size = nrow(d), replace = TRUE)
    )
    
    fill <- d$soil_color
    
    return(list(coords = coords, fill = fill))
  }, simplify = FALSE
  )
  
  
  coords <- do.call('rbind',
                    lapply(p, '[[', 'coords')
  )
  
  fill <- do.call('c',
                  lapply(p, '[[', 'fill')
  )
  
  
  
  return(
    list(coords = coords, fill = fill)
  )
  
}

o <- c('leon', 'musick', 'fresno', 'zook', 'amador', 'sierra', 'lucy', 'pierre', 'miami', 'drummer')

z <- lapply(o, cubit, n.fuzz = 5, n.sim = 50)
z <- transpose(z)

for(i in seq_along(z$coords)) {
  .inc <- (i-1) * 5
  z$coords[[i]]$z <- z$coords[[i]]$z - .inc
}


z.c <- do.call('rbind', z$coords)
z.f <- do.call('c', z$fill)

cubes <- isocubesGrob(
  coords = z.c,
  fill = z.f,
  max_y = 100,
  occlusion_depth = 4,
  col = NA, 
  xo = 0.2,
  yo = 0.2,
  verbose = TRUE
)

grid.newpage()
grid.draw(cubes)


