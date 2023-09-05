library(aqp)

## keeping track of progress here:
# https://github.com/ncss-tech/aqp/issues/296

data("SPC.with.overlap")

x <- SPC.with.overlap

# all 4 profiles have overlap
checkHzDepthLogic(x)

# correctly identifies overlap
hzDepthTests(top = x[1, , .TOP], bottom = x[1, , .BOTTOM])


## TODO: there should be an overlap error reported here, right?
checkHzDepthLogic(x, byhz = TRUE)

## this can't possibly detect overlap, working a horizon at a time..
hzDepthTests(top = x[1, 1, .TOP], bottom = x[1, 1, .BOTTOM])



## 


x$.overlapFlag <- flagOverlappingHz(x)

par(mar = c(2, 0, 3, 2))
options(.aqp.plotSPC.args = list(name.style = 'center-center', color = 'hzID', hz.depths = TRUE, depth.axis = FALSE, cex.names = 0.85))

plotSPC(x, color = '.overlapFlag')

plotSPC(x, color = 'hzID', show.legend = FALSE)
plotSPC(x, color = 'claytotest')


## two overlapping horizons
z <- data.frame(
  id = 'SPC',
  top = c(0, 25, 25, 50, 75, 100, 100),
  bottom = c(25, 50, 50, 75, 100, 125, 125)
)

depths(z) <- id ~ top + bottom
z$.overlapFlag <- flagOverlappingHz(z)
plotSPC(z, color = '.overlapFlag')

# no longer returns bogus horizons
plotSPC(fillHzGaps(z), color = '.overlapFlag')


# create a real gap
z <- data.frame(
  id = 'SPC',
  top = c(0, 15, 25, 25, 50, 75, 100, 100),
  bottom = c(10, 25, 50, 50, 75, 100, 125, 125)
)

depths(z) <- id ~ top + bottom
z$.overlapFlag <- flagOverlappingHz(z)
plotSPC(z, color = '.overlapFlag', default.color = 'yellow')

# gap has been filled
# overlapping horizons have not been interpreted as gaps
plotSPC(fillHzGaps(z), color = '.overlapFlag', default.color = 'yellow')



## no longer returns bogus horizons
xx <- fillHzGaps(x[1, ])
plotSPC(xx)



## test dice()

# ok
(d <- dice(x, fm = 30 ~ claytotest + .overlapFlag, SPC = FALSE))

# ok: no error thrown
(d <- dice(x, fm = 30 ~ claytotest + .overlapFlag, SPC = FALSE, strict = TRUE))

# no longer creates extra NA slices
d <- dice(x, fm = 25:45 ~ claytotest + .overlapFlag)
plotSPC(d, color = 'claytotest', cex.names = 0.5, hz.depths = FALSE, depth.axis = TRUE)
horizons(d[1, ])

d <- dice(x, fm = 28:40 ~ claytotest + .overlapFlag)
plotSPC(d, color = 'claytotest', cex.names = 0.5, hz.depths = FALSE, depth.axis = TRUE)
horizons(d[1, ])


# no bogus horizons
d <- dice(x)
plotSPC(d, color = 'claytotest', cex.names = 0.5, hz.depths = FALSE, depth.axis = TRUE)

# as expected
horizons(d[1, 25:35])


## test slab()
# wt.mean is correct
# contributing fraction is correct
(a <- slab(x, upedonid ~ claytotest, slab.structure = c(0, 50), slab.fun = mean, na.rm = TRUE))

profileApply(x, function(i) {
  i <- trunc(i, 0, 50)
  .v <- i$claytotest
  .w <- i$hzdepb - i$hzdept
  idx <- which(!is.na(.v) & !is.na(.w))
  weighted.mean(.v[idx], w = .w[idx])
})

