library(aqp)






s <- lapply(1:20, random_profile, SPC = TRUE)
s <- combine(s)
getLastHorizonID(s)

s <- lapply(sample(letters, size = 26), random_profile, SPC = TRUE)
s <- combine(s)
getLastHorizonID(s)






data(sp4)
depths(sp4) <- id ~ top + bottom

par(mar = c(0, 0, 0, 0))
plotSPC(sp4, width = 0.3)

# copy to break
x <- sp4

## valid (deepest) horizons to repair:
# introduce NA
x$bottom[4] <- NA
# top == bottom
x$bottom[6] <- x$top[6]

## invalid horizons to repair
x$bottom[12] <- NA

# marked as invalid
checkHzDepthLogic(x)

# ugh, creates some sketch artifacts
# https://github.com/ncss-tech/aqp/issues/189
plotSPC(x, width = 0.3)


z <- repairMissingHzDepths(x)
z$.repaired <- factor(z$.repaired)


plotSPC(
  x, 
  width = 0.3,
  plot.depth.axis = FALSE
)

plotSPC(
  z, 
  width = 0.3,
  color = '.repaired',
  plot.depth.axis = FALSE
)

