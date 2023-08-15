##
## sp1[1, ] has a bottom horizon with top == bottom 
## this causes problems when plotSPC(..., hz.depths = TRUE)
## 
## https://github.com/ncss-tech/aqp/issues/290
##
##

## TODO: weekly update on this and related data issues


library(aqp)

options(.aqp.plotSPC.args = list(depth.axis = FALSE))

data(sp1)
depths(sp1) <- id ~ top + bottom
hzdesgnname(sp1) <- 'name'

# sp1[1, ] has problems
checkHzDepthLogic(sp1)

# 
par(mar = c(0, 0, 0, 2))

plotSPC(
  sp1,
  cex.names = 0.66,
  hz.depths = FALSE,
  depth.axis = TRUE,
  name.style = 'center-center'
)

plotSPC(
  sp1,
  cex.names = 0.66,
  hz.depths = TRUE,
  name.style = 'center-center'
)

x <- repairMissingHzDepths(sp1)

plotSPC(
  x,
  cex.names = 0.66,
  hz.depths = TRUE,
  name.style = 'center-center'
)


repairMissingHzDepths(sp1[5, ])
