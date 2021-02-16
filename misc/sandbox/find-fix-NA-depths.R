library(aqp)


## reasonable tests



s <- lapply(1:20, random_profile, SPC = TRUE)
s <- combine(s)
getLastHorizonID(s)
plotSPC(s, name = hzidname(s))

s <- lapply(sample(letters, size = 26), random_profile, SPC = TRUE)
s <- combine(s)
getLastHorizonID(s)
plotSPC(s, name = hzidname(s))





data(sp4)
depths(sp4) <- id ~ top + bottom

par(mar = c(0, 0, 0, 0))
plotSPC(sp4, width = 0.3)

# copy to break
x <- sp4

## legal (deepest) horizons to repair:
# introduce NA
x$bottom[4] <- NA
# top == bottom
x$bottom[6] <- x$top[6]

## not legal horizons to repair
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

# extra bad case -- where two top depths are NA cannot know which is the "bottom most"
data(sp4)
x2 <- sp4
x2$top[3:4] <- NA
x2$bottom[3:4] <- x2$bottom[4:3]

# we get a warning on build -- top depths with NA are extra bad news
depths(x2) <- id ~ top + bottom

# this is actually technically possible, tho unlikely as bottom depth is ignored by SPC sort

# invalid depth logic in profile 1 (colusa)
checkHzDepthLogic(x2)

# plotSPC 
#  NB: shows the horizons even though they have no top depth
plotSPC(x2)

# inspect
horizons(x2[1,])

# do the repair
z2 <- repairMissingHzDepths(x2)
plot(z2)

# result is unaltered -- is this what we expect? / can we _do_ anything w/ NA top depths?
# in the absence of top depth, order cannot be guaranteed
# the most common sources of NA top depths will also have bottom be NA -- e.g. empty NASIS record
horizons(z2[1,])
