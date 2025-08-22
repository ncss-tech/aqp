library(aqp)
data("osd")

o <- osd


# shuffle horizon data, excluding id, top, bottom
shuffle <- function(i) {
  .hzd <- horizonDepths(i)
  .id <- idname(i)
  .h <- horizons(i)
  .nm <- horizonNames(i)
  
  # static horizon data
  .hs <- .h[, c(.id, .hzd)]
  
  # dynamic horizon data -> these will be shuffled
  .hd <- .h[, which(! .nm %in% c(.hzd, .id))]
  
  # shuffle
  .idx <- sample(1:nrow(.hd))
  .hd <- .hd[.idx, ]
  
  # combine static + shuffled
  .hnew <- cbind(.hs, .hd)
  
  # replace and return
  replaceHorizons(i) <- .hnew
  
  return(i)
}


# apply shuffling by-profile
oo <- combine(
  profileApply(o, FUN = shuffle)
)

# TODO: implement via data.table


# graphical comparison
plotSPC(o, name.style = 'center-center', cex.names = 0.66, width = 0.2, x.idx.offset = 0.25)
plotSPC(oo, name.style = 'center-center', cex.names = 0.66, width = 0.2, add = TRUE, x.idx.offset = -0.25, print.id = FALSE, depth.axis = FALSE)


profileInformationIndex(o, vars = 'hzname')
profileInformationIndex(oo, vars = 'hzname')


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

