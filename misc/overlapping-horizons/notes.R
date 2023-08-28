library(aqp)

## keeping track of progress here:
# https://github.com/ncss-tech/aqp/issues/296



## example data from Sharon

# x <- read.csv('e:/temp/2020GRR014SPCdata.csv')
# 
# depths(x) <- upedonid ~ hzdept + hzdepb
# hzdesgnname(x) <- 'hzname'
# site(x) <- ~ NasisSiteName + taxonname + dspplotid + earthcovkind1 + earthcovkind2
# 
# x <- x[c(1, 2, 3, 8), ]
# 
# saveRDS(x, file = 'misc/overlapping-horizons/example-DSP-data.rds')


# example DSP data
x <- readRDS('misc/overlapping-horizons/example-DSP-data.rds')

# all 4 profiles have overlap
checkHzDepthLogic(x)

## TODO: there should be an overlap error reported here:
checkHzDepthLogic(x, byhz = TRUE)

## consider a new function flagOverlap()

# crude prototype, single profile at a time
flagOverlap <- function(x) {
  
  .fo <- function(i) {
    .x <- i[, , .TOP]
    .o <- overlapMetrics(.x, thresh = 0.1)
    
    .res <- rep(FALSE, times = length(.x))
    .res[.o$idx] <- TRUE
    
    return(.res)
  }
  
   profileApply(x, .fo, simplify = TRUE)
}

x$.overlapFlag <- flagOverlap(x)

par(mar = c(2, 0, 3, 2))
options(.aqp.plotSPC.args = list(name.style = 'center-center', color = 'hzID', hz.depths = TRUE, depth.axis = FALSE, cex.names = 0.85))

plotSPC(x, color = '.overlapFlag')

plotSPC(x, color = 'hzID', show.legend = FALSE)
plotSPC(x, color = 'claytotest')


## TODO: fillHzGaps() adding bogus horizons
xx <- fillHzGaps(x[1, ])
plotSPC(xx)



## test dice()

# ok
(d <- dice(x, fm = 30 ~ claytotest + .overlapFlag, SPC = FALSE))

# ok: no error thrown
(d <- dice(x, fm = 30 ~ claytotest + .overlapFlag, SPC = FALSE, strict = TRUE))

# not right: extra NA slices...?
d <- dice(x, fm = 25:45 ~ claytotest + .overlapFlag)
plotSPC(d, color = 'claytotest', cex.names = 0.5, hz.depths = FALSE, depth.axis = TRUE)
horizons(d[1, ])

d <- dice(x, fm = 28:40 ~ claytotest + .overlapFlag)
plotSPC(d, color = 'claytotest', cex.names = 0.5, hz.depths = FALSE, depth.axis = TRUE)
horizons(d)




## test slab()
# wt.mean is correct
(a <- slab(x, upedonid ~ claytotest, slab.structure = c(0, 50), slab.fun = mean, na.rm = TRUE))

profileApply(x, function(i) {
  i <- trunc(i, 0, 50)
  .v <- i$claytotest
  .w <- i$hzdepb - i$hzdept
  idx <- which(!is.na(.v) & !is.na(.w))
  weighted.mean(.v[idx], w = .w[idx])
})

