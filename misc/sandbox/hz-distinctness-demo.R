library(aqp)
library(soilDB)


x <- fetchOSD(c('leon', 'musick', 'clarksville', 'sierra', 'pardee', 'amador', 'lucy', 'dylan', 'tristan', 'pierre', 'drummer', 'zook'))

# encode horizon boundarydistinctness via vertical offset
x$hd <- hzDistinctnessCodeToOffset(
  x$distinctness, 
  codes=c('very abrupt', 'abrupt', 'clear', 'gradual', 'diffuse')
  )

# encode horizon boundary topography via vertical offset
x$hzto <- hzTopographyCodeToOffset(
  x$topography, 
  codes = c('smooth', 'wavy', 'irregular', 'broken')
)

site(x)$code <- 'OSD'


# ok
par(mar = c(0, 0, 0, 1), bg = 'black', fg = 'white')
plotSPC(x, width = 0.3, hz.distinctness.offset = 'hd', hz.topography.offset = 'hzto', cex.names = 0.55) 


# keep a single OSD for the demo
x <- filter(x, id == 'LEON')

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



