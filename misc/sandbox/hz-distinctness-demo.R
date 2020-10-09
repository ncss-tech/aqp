library(aqp)
library(soilDB)

x <- fetchOSD('musick')

# encode horizon distinctness via vertical offset
x$hd <- hzDistinctnessCodeToOffset(
  x$distinctness, 
  codes=c('very abrupt', 'abrupt', 'clear', 'gradual', 'diffuse')
  )

x$code <- 'OSD'

# safely duplicate these data 6 times
# --> new IDs generated, old IDs saved into .oldID
s <- duplicate(x, times = 6)

# ok
plotSPC(s) 

# storage for intermediate SPC objs
l <- list()

# iterate over horizon distinctness depths
hzdo <- c(0, 0.5, 2, 5, 15, 20)/2

for(i in seq_along(hzdo)) {
  # current profile
  ss <- s[i, ]
  # set for all horizons
  ss$hd <- hzdo[i]
  l[[i]] <- ss
}

# new version of aqp::union()
s <- combine(l)
# horizon distinctness codes
s$code <- c('none', 'very abrupt', 'abrupt', 'clear', 'gradual', 'diffuse')


# combine OSD with horizon distinctness demo profiles
z <- combine(x, s)

par(mar=c(0,0,1,0))
plotSPC(z, width=0.33, name.style='center-center', print.id=TRUE, hz.distinctness.offset = 'hd', label='code', hz.depths=TRUE, cex.names=0.8, plot.depth.axis=FALSE)
title('Horizon Boundary Distinctness', line = 0)

