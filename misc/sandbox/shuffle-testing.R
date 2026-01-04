library(aqp)
library(corrplot)
library(soilDB)
library(cluster)
library(vegan)



## idea: information content of soil profile vs. many times shuffled variants
##        * smaller deviations => less information
##        * larger deviations => more information




data('osd', package = 'aqp')
o <- osd

# apply shuffling by-profile
o.d <- shuffle(o, mode = 'data')

# apply shuffling by physical horizon
o.h <- shuffle(o, mode = 'horizon')

# sampling with replacement
o.h2 <- shuffle(o, mode = 'horizon', replace = TRUE)


plotSPC(o[1, ])

# generate unique permutations of a unique sequence
# s: sequence
# nrow(perms(s)) == s!
# inefficient for n > 8
perms <- function(s) {
  
  # sequence length
  .n <- length(s)
  
  # all possible combinations, includes duplicates
  .g <- do.call('expand.grid', rep(list(s), times = .n))
  .g <- as.matrix(.g)
  
  # keep permutations without duplicates
  .idx <- which(apply(.g, 1, function(i) {length(unique(i)) == .n}))
  .g <- .g[.idx, ]
  
  # rows are permutations
  # columns are sequence elements
  return(.g)
}

perms(1:4)



# add method to IDs
profile_id(o.d) <- sprintf("%s\ndata", profile_id(o.d))
profile_id(o.h) <- sprintf("%s\nhz", profile_id(o.h))
profile_id(o.h2) <- sprintf("%s\nhz R", profile_id(o.h2))

# combine into single SPC
g <- combine(o, o.d, o.h, o.h2)

# graphical comparison
par(mar = c(0, 0, 0.5, 2.5))
plotSPC(g, name.style = 'center-center', cex.names = 0.66, width = 0.3, cex.id = 0.75)



## interpret...
profileInformationIndex(o, vars = 'hzname')
profileInformationIndex(o.d, vars = 'hzname')
profileInformationIndex(o.h, vars = 'hzname')


d <- duplicate(o, times = 10)

d <- aqp::shuffle(d, mode = 'horizon')

d <- combine(o, d)

plotSPC(d, name.style = 'center-center', cex.names = 0.66, width = 0.3)
profileInformationIndex(d, vars = 'hzname')


## 
dd <- soilColorSignature(d, color = 'soil_color', method = 'depthSlices', perceptualDistMat = TRUE)

plotProfileDendrogram(d, cluster::diana(dd))

## NCSP

# idea:
#  permute horizons of S many times (not feasible to perform all permutations)
)
#  


o <- fetchOSD(c('hanford', 'cecil', 'menfro', 'drummer', 'mexico'))
o <- trunc(o, 0, 150)

par(mar = c(0, 0, 0, 2))
plotSPC(o, name.style = 'center-center', cex.names = 0.9)


g <- combine(
  o,
  aqp::shuffle(duplicate(o, times = 25), mode = 'horizon')
)

# ensure .oldID is available for original data
idx <- which(is.na(g$.oldID))
g$.oldID[idx] <- profile_id(g)[idx]

par(mar = c(0, 0, 0, 1))
plotSPC(g)

plotSPC(g, color = 'texture_class', show.legend = FALSE, print.id = FALSE, name = NA, divide.hz = FALSE)

o$pii <- profileInformationIndex(o, vars = c('texture_class'))
g$pii <- profileInformationIndex(g, vars = c('texture_class'))


site(o)[, c('id', 'pii')]
tapply(g$pii, g$.oldID, mean)
tapply(g$pii, g$.oldID, sd)
tapply(g$pii, g$.oldID, mean) / tapply(g$pii, g$.oldID, var)


d <- NCSP(g, vars = c('texture_class', 'value', 'chroma'))

b <- betadisper(d, group = g$.oldID, bias.adjust = TRUE, sqrt.dist = FALSE, type = 'median')

b

plot(b)

par(mar = c(0, 0, 0, 0))
plotProfileDendrogram(g, cluster::diana(d), scaling.factor = 0.8)


# expand dist object to full matrix form of the pair-wise distances 
m <- as.matrix(d)
# copy short IDs from Soil Profile Collection to full distance matrix
dimnames(m) <- list(g$.oldID, g$.oldID)

# invert device foreground / background colors for an artistic effect
# use colors from The Life Aquatic
par(bg = 'black', fg = 'white')
corrplot(
  m, 
  col = hcl.colors(n = 25, palette = 'zissou1'), 
  is.corr = FALSE, 
  col.lim = c(0, 100), 
  method = "color", 
  order = "original",
  type = "upper", 
  # tl.pos = "n",
  # cl.pos = "n",
  mar = c(0.1, 0, 0, 0.8), tl.cex = 0.45
) 


.p <- profile_id(o)

D <- lapply(.p, function(i) {
  
  idx <- grep(i, profile_id(g), fixed = TRUE)
  
  # dimnames(m[idx, idx])
  
  m.i <- m[idx, idx]
  v.i <- m.i[upper.tri(m.i, diag = FALSE)]
  return(v.i)
})

names(D) <- .p

boxplot(D, las = 1)




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

