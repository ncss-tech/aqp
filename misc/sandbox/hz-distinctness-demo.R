library(aqp)
library(soilDB)


osds <- fetchOSD(c('leon', 'musick', 'clarksville', 'sierra', 'pardee', 'amador', 'lucy', 'dylan', 'tristan', 'pierre', 'drummer', 'zook'))

# encode horizon boundarydistinctness via vertical offset
osds$hd <- hzDistinctnessCodeToOffset(
  osds$distinctness, 
  codes=c('very abrupt', 'abrupt', 'clear', 'gradual', 'diffuse')
  )

# encode horizon boundary topography via vertical offset
osds$hzto <- hzTopographyCodeToOffset(
  osds$topography, 
  codes = c('smooth', 'wavy', 'irregular', 'broken')
)

site(osds)$code <- 'OSD'

osds$bnd.code <- sprintf(
  "%s/%s",
  substr(osds$distinctness, 1, 1),
  substr(osds$topography, 1, 1)
)

osds$bnd.code <- gsub('NA/NA', '', osds$bnd.code)

# ok
par(mar = c(0, 0, 0, 1), bg = 'black', fg = 'white')
plotSPC(osds, width = 0.3, hz.distinctness.offset = 'hd', hz.topography.offset = 'hzto', cex.names = 0.55) 

plotSPC(osds, width = 0.3, hz.distinctness.offset = 'hd', hz.topography.offset = 'hzto', cex.names = 0.66, name = 'bnd.code') 

# keep a single OSD for the demo
x <- filter(osds, id == 'LEON')

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
title('Horizon Boundary Distinctness', line = 0)



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
title('Horizon Boundary Distinctness', line = 0)




## combinations

x <- filter(osds, id == 'ZOOK')

g <- expand.grid(
  topography = c('S', 'W', 'I', 'B'),
  distinctness = c('V', 'A', 'C', 'G', 'D')
  
)

l <- list()
s <- duplicate(x, times = nrow(g))
for(i in 1:nrow(g)) {
  # current profile
  ss <- s[i, ]
  # set for all horizons
  horizons(ss)$hd <- hzDistinctnessCodeToOffset(g$distinctness[i])
  horizons(ss)$ht <- hzTopographyCodeToOffset(g$topography[i])
  l[[i]] <- ss  
}

s <- combine(l)
s$code <- sprintf("%s/%s", g$distinctness, g$topography)

par(mar=c(0,0,1,1), bg = 'black', fg = 'white')
plotSPC(s, width=0.33, print.id=TRUE, hz.distinctness.offset = 'hd', hz.topography.offset = 'ht', label='code', cex.names=0.8, name = NA, color = NA, default.color = 'black')
title('Horizon Boundary Types', line = 0, col.main = 'white')
abline(h = x$top, col = 'green', lty = 3)
