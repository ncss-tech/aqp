## Simulation gradual erosion / deposition of an SPC
## currently very slow
##


library(aqp)
library(soilDB)

# simulate the addition / removal of material from the top of an SPC
evolve <- function(x, loss.limits = c(0, 10), gain.limits = c(0, 20), iter = 10) {
  
  hztb <- horizonDepths(x)
  
  
  # previous states: loss / gain is cumulative 
  loss.prev <- 0
  gain.prev <- 0
  
  # original IDs
  ids <- profile_id(x)
  
  # store iterations in a list
  x.new <- vector(mode = 'list', length = iter)
  
  # first iteration is the original
  x.1 <- x
  newID <- sprintf("T%03d_%s", 1, ids)
  profile_id(x.1) <- newID
  x.new[[1]] <- x.1
  
  # subsequent iterations
  for(i in 2:iter) {
    # adjust ID
    newID <- sprintf("T%03d_%s", i, ids)
    
    # flip a coin: loss / gain
    coin.flip <- sample(0:1, size = 1, prob = c(0.7, 0.3))
    
    # loss
    amount <- runif(1, min = loss.limits[1], max = loss.limits[2])
    amount <- round(amount + loss.prev)
    
    # gain, applied to previous iteration
    x.last <- x.new[[i-1]]
    
    # gain material
    gain.amount <- runif(1, min = gain.limits[1] , max = gain.limits[2])
    # convert to absolute depth
    x.last.top <- min(x.last[[hztb[1]]])
    gain.z <- x.last.top - gain.amount
    
    if(coin.flip < 1) {
      
      # erode via truncation at top
      x.mod <- trunc(x, z1 = amount, z2 = max(x))
      
    } else {
      
      # 
      x.mod <- deposite(x.last, z = gain.z)
    }
    
    
    # set new ID
    profile_id(x.mod) <- newID
    x.new[[i]] <- x.mod
    
    # maintain state
    loss.prev <- amount
  }
  
  # list -> SPC
  x.new <- combine(x.new)
  
  hzdesgnname(x.new)
  
  return(x.new)
}

# add "new" material to the top of an SPC
deposite <- function(x, z, depName = '2C', depColor = parseMunsell('5Y 6/2')) {
  
  x.new <- fillHzGaps(x, to_top = z, flag = TRUE)
  hzd <- hzdesgnname(x.new)
  
  idx <- which(x.new$.filledGap)
  
  x.new[[hzd]][idx] <- depName
  x.new$soil_color[idx] <- depColor
  
  ## TODO: need mergeHorizons() to aggregate over deposition events
  
  return(x.new)

}


# works with multiple starting profiles
# o <- fetchOSD(c('drummer', 'cecil'))

# single profile simplest to think about
o <- fetchOSD(c('musick'))

# limits define range of runif()
e <- evolve(o, iter = 25, loss.limits = c(1, 10))

par(mar = c(0, 0, 0, 0))
plotSPC(e, plot.depth.axis = FALSE, name.style = 'center-center', print.id = FALSE, width = 0.45)
abline(h = 0)



plotSPC(z, plot.depth.axis = FALSE, name.style = 'center-center', print.id = FALSE, width = 0.45)
abline(h = 0)





