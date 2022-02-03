library(aqp)
library(soilDB)

s <- c('leon', 'musick', 'clarksville', 'sierra', 'pardee', 'amador', 'lucy', 'dylan', 'tristan', 'pierre', 'drummer', 'zook')
osds <- fetchOSD(s)

# encode horizon boundarydistinctness via vertical offset
osds$hd <- hzDistinctnessCodeToOffset(osds$distinctness)

# encode horizon boundary topography via vertical offset
osds$hzto <- hzTopographyCodeToOffset(osds$topography)

# also encode horizon boundary topography las line type
osds$hzto.lty <- hzTopographyCodeToLineType(osds$topography)

site(osds)$code <- 'OSD'

osds$bnd.code <- sprintf(
  "%s%s",
  substr(osds$distinctness, 1, 1),
  substr(osds$topography, 1, 1)
)

# remove missing (NA) labels
osds$bnd.code <- gsub('NANA', '', osds$bnd.code)

# ok
par(mar = c(0, 0, 0, 1), bg = 'black', fg = 'white')
plotSPC(osds, width = 0.3, hz.distinctness.offset = 'hd', hz.topography.offset = 'hzto', cex.id = 0.66, cex.names = 0.66) 

plotSPC(osds, width = 0.3, hz.distinctness.offset = 'hd', hz.topography.offset = 'hzto', cex.id = 0.66, cex.names = 0.66, name = 'bnd.code') 


plotSPC(osds, width = 0.3, hz.distinctness.offset = 'hd', hz.topography.offset = 'hzto', cex.id = 0.66, cex.names = 0.66, name = 'bnd.code', hz.boundary.lty = 'hzto.lty') 

legend('bottomleft', horiz = TRUE, legend = c('Smooth', 'Wavy', 'Irregular', 'Broken'), lty = 1:4, inset = 0.05, bty = 'n', cex = 0.85)


plotSPC(osds, width = 0.3, hz.distinctness.offset = 'hd', hz.topography.offset = 'hzto', cex.id = 0.66, cex.names = 0.66, name = 'name', hz.boundary.lty = 'hzto.lty') 

legend('bottomleft', horiz = TRUE, legend = c('Smooth', 'Wavy', 'Irregular', 'Broken'), lty = 1:4, inset = 0.05, bty = 'n', cex = 0.85)


# single horizon test: 
# https://github.com/ncss-tech/aqp/issues/189
par(mar = c(0, 0, 0, 1), bg = 'black', fg = 'white')
plotSPC(osds[, 1], width = 0.3, hz.distinctness.offset = 'hd', hz.topography.offset = 'hzto', cex.id = 0.66, cex.names = 0.66, name = 'name', hz.boundary.lty = 'hzto.lty') 




# keep a single OSD for the demo
x <- subset(osds, id == 'LEON')

# safely duplicate these data 6 times
# --> new IDs generated, old IDs saved into .oldID
s <- duplicate(x, times = 6)

# ok
par(mar = c(0, 0, 0, 1))
plotSPC(s, width = 0.3, hz.distinctness.offset = 'hd', hz.topography.offset = 'hzto') 

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

par(mar=c(0,0,1,1))
plotSPC(z, width=0.33, print.id=TRUE, hz.distinctness.offset = 'hd', label='code', cex.names=0.8)
title('Horizon Boundary Distinctness', line = 0, col.main = 'white')

# # not so very useful legend
# par(xpd = NA)
# hzdo.vals <- c(0.5, 2, 5, 15, 20)/2
# rect(xleft = 0, ybottom = 20, xright = 0.5, ytop = 0)
# segments(x0 = 0, y0 = 10 + hzdo.vals, x1 = 0.5, y1 = 10 - hzdo.vals)


# safely duplicate these data 4 times
# --> new IDs generated, old IDs saved into .oldID
s <- duplicate(x, times = 4)

# storage for intermediate SPC objs
l <- list()

# iterate over horizon topography offsets
# theses aren't the default values but maybe should be
hzto <- c(0, 5, 10, 25)

for(i in seq_along(hzto)) {
  # current profile
  ss <- s[i, ]
  # set for all horizons
  ss$hzto <- hzto[i]
  l[[i]] <- ss
}

# new version of aqp::union()
s <- combine(l)
# horizon distinctness codes
s$code <- c('smooth', 'wavy', 'irregular', 'broken')


# combine OSD with horizon distinctness demo profiles
z <- combine(x, s)

par(mar=c(0,0,1,1))
plotSPC(z, width=0.33, print.id=TRUE, hz.topography.offset = 'hzto', label='code', cex.names=0.8)
title('Horizon Boundary Distinctness', line = 0, col.main = 'white')




## combinations

x <- subset(osds, id == 'DRUMMER')

# full set
g <- expand.grid(
  distinctness = c('V', 'A', 'C', 'G', 'D'),
  topography = c('S', 'W', 'I', 'B')
)

# simplified set
g <- expand.grid(
  distinctness = c('A', 'C', 'G', 'D'),
  topography = c('S', 'W', 'I')
)



l <- list()
s <- duplicate(x, times = nrow(g))
for(i in 1:nrow(g)) {
  # current profile
  ss <- s[i, ]
  # set for all horizons
  horizons(ss)$hd <- hzDistinctnessCodeToOffset(g$distinctness[i])
  horizons(ss)$ht <- hzTopographyCodeToOffset(g$topography[i])
  horizons(ss)$ht.lty <- hzTopographyCodeToLineType(g$topography[i]) 
  l[[i]] <- ss  
}

s <- combine(l)
s$code <- sprintf("%s%s", g$distinctness, g$topography)

par(mar=c(0,0,1,1), bg = 'black', fg = 'white')
plotSPC(s, width=0.33, print.id=TRUE, hz.distinctness.offset = 'hd', hz.topography.offset = 'ht', label='code', cex.names=0.8, name = NA, color = NA, default.color = 'black', hz.boundary.lty = 'ht.lty')
title('Horizon Boundary Types', line = 0, col.main = 'white')

p.seq <- rep(1:length(s), each = nrow(s[1, ]))
points(x = p.seq, y = s$bottom, pch = 15, col = 'green', cex = 0.5)

legend('bottom', horiz = TRUE, legend = c('Smooth', 'Wavy', 'Irregular', 'Broken'), lty = 1:4, inset = 0.1)


