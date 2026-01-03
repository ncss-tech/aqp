library(aqp)


## idea: information content of soil profile vs. many times shuffled variants
##        * smaller deviations => less information
##        * larger deviations => more information




data('osd', package = 'aqp')
o <- osd

# apply shuffling by-profile
o.d <- shuffle(o, mode = 'data')

# appl
o.h <- shuffle(o, mode = 'horizon')

# sampling with replacement
o.h2 <- shuffle(o, mode = 'horizon', replace = TRUE)

# add method to IDs
profile_id(o.d) <- sprintf("%s\ndata", profile_id(o.d))
profile_id(o.h) <- sprintf("%s\nhz", profile_id(o.h))
profile_id(o.h2) <- sprintf("%s\nhz R", profile_id(o.h2))

# combine into single SPC
g <- combine(o, o.d, o.h, o.h2)

# graphical comparison
op <- par(mar = c(0, 0, 0.5, 2.5))
plotSPC(g, name.style = 'center-center', cex.names = 0.66, width = 0.3, cex.id = 0.75)

par(op)


## interpret...
profileInformationIndex(o, vars = 'hzname')
profileInformationIndex(o.d, vars = 'hzname')
profileInformationIndex(o.h, vars = 'hzname')


d <- duplicate(o, times = 10)

d <- shuffle(d, mode = 'horizon')

d <- combine(o, d)

plotSPC(d, name.style = 'center-center', cex.names = 0.66, width = 0.3)
profileInformationIndex(d, vars = 'hzname')


## 
dd <- soilColorSignature(d, color = 'soil_color', method = 'depthSlices', perceptualDistMat = TRUE)

plotProfileDendrogram(d, cluster::diana(dd))

## NCSP
library(soilDB)
library(cluster)
library(vegan)


o <- fetchOSD(c('hanford', 'cecil', 'menfro', 'drummer'))
o <- trunc(o, 0, 150)

d <- duplicate(o, times = 25)

# plotSPC(d)

d <- aqp::shuffle(d, mode = 'horizon')


plotSPC(d, color = 'texture_class', show.legend = FALSE, print.id = FALSE, name = NA, divide.hz = FALSE)

o$pii <- profileInformationIndex(o, vars = c('texture_class'))
d$pii <- profileInformationIndex(d, vars = c('texture_class'))


site(o)[, c('id', 'pii')]
tapply(d$pii, d$.oldID, mean)
tapply(d$pii, d$.oldID, sd)

tapply(d$pii, d$.oldID, mean) / tapply(d$pii, d$.oldID, var)


dd <- NCSP(d, vars = 'texture_class')

b <- betadisper(dd, group = d$.oldID, bias.adjust = TRUE, sqrt.dist = FALSE, type = 'median')

b






## simulate some data
s <- rp(25, method = 'LPP', lpp.a = 5, lpp.b = 10, lpp.d = 5, lpp.e = 5, lpp.u = 25)

par(mar = c(1, 0, 3, 2))
plotSPC(s, name.style = 'center-center', color = 'p1', width = 0.33)

# shuffle 100 times
x <- replicate(100, {
  ss <- combine(
    profileApply(s, FUN = shuffle)
  )
  profileInformationIndex(ss, vars = c('p1', 'p2', 'p3'))
})

# original
x.o <- profileInformationIndex(s, vars = c('p1', 'p2', 'p3'))

# rows: profile index
# cols: simulation index
str(x)

# PII variance by profile, across all simulations 
v.1 <- apply(x, 1, var)

# PII variance by simulations, across all profiles
v.2 <- apply(x, 2, var)


hist(v.1)
hist(v.2)

hist(v.2 / var(x.o), breaks = 15)

d <- sweep(x, MARGIN = 1, STATS = x.o, FUN = '-')

hist(d)

# what does this mean?

par(mar = c(1, 0, 3, 2))
plotSPC(s, name.style = 'center-center', color = 'p1', width = 0.33, plot.order = order(v.1))

sort(v.1)

