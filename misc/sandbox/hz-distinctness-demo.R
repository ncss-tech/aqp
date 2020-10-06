library(aqp)
library(soilDB)

x <- fetchOSD('musick')

x$hd <- hzDistinctnessCodeToOffset(x$distinctness, codes=c('very abrupt', 'abrupt', 'clear', 'gradual', 'diffuse'))
x$code <- 'OSD'

# hack: simulation with sd=0 -> replication of original data
s <- sim(x, n = 6, hz.sd = 0)

# https://github.com/ncss-tech/aqp/issues/161
# remove original ID column, this causes problems for union -> rbindfill -> merge.data.frame
s$id <- NULL

## Error in aqp::union(list(x, s)) : non-unique profile IDs detected
# alternative approach with permute_profile
horizons(x)$bdy <- 0
s <- permute_profile(x, n=6, boundary.attr = 'bdy')
plotSPC(s)
# 

s$hd <- rep(NA, times=nrow(s))
l <- list()

hzdo <- c(0, 0.5, 2, 5, 15, 20)/2
for(i in seq_along(hzdo)) {
  ss <- s[i, ]
  ss$hd <- hzdo[i]
  l[[i]] <- ss
}

s <- aqp::union(l)
s$code <- c('none', 'very abrupt', 'abrupt', 'clear', 'gradual', 'diffuse')

z <- aqp::union(list(x, s))

par(mar=c(0,0,1,0))
plotSPC(z, width=0.33, name.style='center-center', print.id=TRUE, hz.distinctness.offset = 'hd', label='code', hz.depths=TRUE, cex.names=0.8, plot.depth.axis=FALSE)
title('Horizon Boundary Distinctness', line=-1)

