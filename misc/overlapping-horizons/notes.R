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
# x <- x[c(1, 2, 3, 4, 8), ]
# 
# saveRDS(x, file = 'misc/overlapping-horizons/example-DSP-data.rds')


# example DSP data
x <- readRDS('misc/overlapping-horizons/example-DSP-data.rds')

# all 4 profiles have overlap
checkHzDepthLogic(x)

# correctly identifies overlap
hzDepthTests(top = x[1, , .TOP], bottom = x[1, , .BOTTOM])


## TODO: there should be an overlap error reported here, right?
checkHzDepthLogic(x, byhz = TRUE)

## this can't possibly detect overlap, working a horizon at a time..
hzDepthTests(top = x[1, 1, .TOP], bottom = x[1, 1, .BOTTOM])



## 
#' @title Flag perfectly overlapping horizons within a SoilProfileCollection
#'
#' @param x a `SoilProfileCollection` object
#'
#' @return logical vector with length (and order) matching the horizons of `x` 
#' @export
#'
#' @examples
#' 
#' # two overlapping horizons
#' z <- data.frame(
#'   id = 'SPC',
#'   top = c(0, 25, 25, 50, 75, 100, 100),
#'   bottom = c(25, 50, 50, 75, 100, 125, 125)
#' )
#' 
#' depths(z) <- id ~ top + bottom
#' z$.overlapFlag <- flagOverlappingHz(z)
#' plotSPC(z, color = '.overlapFlag', hz.depths = TRUE, depth.axis = FALSE, cex.names = 0.85)
#' 
flagOverlappingHz <- function(x) {
  
  # crude prototype, single profile at a time
  .fo <- function(i) {
    
    # tops / bottoms
    # NA not currently handled
    .tops <- i[, , .TOP]
    .bottoms <- i[, , .BOTTOM]
    
    # find perfect overlap
    .rt <- rle(.tops)
    .rb <- rle(.bottoms)
    
    # id affected horizons
    .ot <- .rt$values[which(.rt$lengths > 1)]
    .ob <- .rb$values[which(.rb$lengths > 1)]
    
    ## TODO: tests required
    # index affected horizons
    .m <- outer(.ot, .tops, '==')
    idx <- as.vector(apply(.m, 1, which))
    
    # generate flag vector along sequence of horizons 
    .res <- rep(FALSE, times = length(.tops))
    .res[idx] <- TRUE
    
    return(.res)
  }
  
  # TODO: can probably be made faster
  #  * only hz data required
  #  * split (profile ID) / apply (.fo()) / combine via DT (returns vector)
  profileApply(x, .fo, simplify = TRUE)
}

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


# no bogus horizons
d <- dice(x)
plotSPC(d, color = 'claytotest', cex.names = 0.5, hz.depths = FALSE, depth.axis = TRUE)

# extra records are returned
horizons(d[1, 25:35])


# 
d <- dice(x, strict = TRUE)
d@metadata



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

