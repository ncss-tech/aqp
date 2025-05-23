library(aqp)
library(compositions)

library(soiltexture)
library(scales)

library(DBI)
library(RSQLite)

library(ragg)

library(purrr)

## load soil texture grid points
data("soiltexture", package = 'aqp')


## get all soil texture measurements from latest KSSL snapshot

# connect
db <- dbConnect(RSQLite::SQLite(), 'E:/NASIS-KSSL-LDM/ncss_labdata-2023.sqlite')

qq <- "
SELECT
sand_total AS sand, silt_total AS silt, clay_total AS clay
FROM lab_layer AS l
JOIN lab_physical_properties AS p ON l.labsampnum = p.labsampnum
WHERE sand IS NOT NULL
AND silt IS NOT NULL
AND clay IS NOT NULL
;"

# run query
x <- dbGetQuery(db, qq)

# close connection
dbDisconnect(db)


# adjust names for plotting with TT.plot()
# names must be SAND, SILT, CLAY
names(x) <- toupper(names(x))

## filtering bogus data

# test of bogus data
x$sum <- rowSums(x[, c('SAND', 'SILT', 'CLAY')])
# > 5% deviation from 100%
idx <- which(abs(x$sum - 100) > 5)

# check errors: 467 ~ 0.1%
(length(idx) / nrow(x)) * 100

# remove errors
x <- x[-idx, ]

# find negative proportions: 37 cases
idx <- which(
  x$SAND < 0 | x$SILT < 0 | x$CLAY < 0
)

x <- x[-idx, ]


## 368,546 records
nrow(x)


# check entire dataset
ragg::agg_png(filename = 'KSSL-full-texture-distribution.png', width = 1000, height = 1000, scaling = 1.5)

TT <- TT.plot(
  class.sys= "USDA-NCSS.TT",    # use "our" texture triangle
  main= "KSSL Soil Texture Database",          # title
  tri.sum.tst=FALSE,            # do not test for exact sum(sand, silt, clay) == 100
  cex.lab=1,                 # scaling of label text
  cex.axis=0.75,                # scaling of axis
  frame.bg.col='white',         # background color
  class.lab.col='black',        # color for texture class labels
  lwd.axis=1.5,                    # line thickness for axis
  arrows.show=TRUE
)

# super slow via RStudio graphics device
TT.points(tri.data = x, geo = TT, col=alpha('royalblue', 0.01), cex = 0.125, tri.sum.tst = FALSE)

dev.off()


# apply texture class rules
x$class <- ssc_to_texcl(sand = x$SAND, clay = x$CLAY, as.is = TRUE)
sort(round(prop.table(table(x$class)), 2))

# split by class for processing
cl <- split(x, x$class)


# extract parameters required to properly simulate SSC samples via texture class label
prepareCompositionalSummary <- function(i, var = 'class') {
  
  # keep track of the texture class
  this.class <- as.character(i[[var]][1])
  
  # convert to a closed / proportional composition object
  z <- acomp(i[, 1:3], total = 100)
  
  # fit 3-term alpha parameters of Dirichlet distribution
  D <- fitDirichlet(z)
 
  # safely compute compositional mean / variance-covariance
  mean.comp <- meanCol(z)
  var.comp <- compositions::var(z, robust = FALSE, method = 'pearson')
  
  # package results
  res <- list(
    texture = this.class,
    mean = mean.comp,
    var = var.comp,
    D.alpha = D$alpha,
    n = nrow(i)
  )
  
  return(res)
}


## TODO: use rnorm.acomp()


##
##
##
sampleComposition <- function(i, var = 'class', n = 100) {
  # simulate from Dirichlet
  s <- rDirichlet.acomp(n, i$D.alpha)
  
  # convert back to format that is suitable for plotting on the TT
  s <- as.data.frame(unclass(s) * 100)
  names(s) <- names(i$mean)
  
  # apply texture class rules
  s[[var]] <- ssc_to_texcl(sand = s$SAND, clay = s$CLAY, as.is = TRUE)
  
  # maybe useful: number of samples in the source texture class
  hit.rate <- prop.table(table(factor(s[[var]] == i$texture)))
  
  # truncate samples to source texture class
  s.trunc <- s[as.character(s[[var]]) == i$texture, ]
  
  # package results into a list
  res <- list(
    samples.truncated = s.trunc,
    samples.full = s,
    mean = i$mean,
    var = i$var,
    hit.rate = hit.rate,
    D.alpha = i$D.alpha,
    texture.class.varname = var
  )
  
  return(res)
}



# processCompData <- function(i, var = 'class', n = 100) {
#   
#   # current texture class
#   # may be a factor, and levels aren't likely the same later on
#   # treat as a text label
#   this.class <- as.character(i[[var]][1])
#   
#   # convert to a closed / proportional composition object
#   z <- acomp(i[, 1:3], total = 100)
#   
#   ## simulate from Dirichlet distribution
#   ## step 1: fit 3-term alpha parameters 
#   D <- fitDirichlet(z)
#   # simulate from Dirichlet
#   s <- rDirichlet.acomp(n, D$alpha)
#   
#   # convert back to format that is suitable for plotting on the TT
#   s <- as.data.frame(unclass(s) * 100)
#   names(s) <- names(i)[1:3]
#   
#   # safely compute compositional mean / variance-covariance
#   mean.comp <- meanCol(z)
#   var.comp <- compositions::var(z, robust = FALSE, method = 'pearson')
# 
#   ##
#   ## these values are often WAY outside of the source class
#   ##
#   # # generate normally distributed samples on the simplex
#   # # these may extend beyond the original class limits
#   # # results are in range [0,1]
#   # s <- rnorm.acomp(n = n, mean = mean.comp, var = var.comp)
#   # 
#   # # convert back to original range [0,100]
#   # s <- as.data.frame(unclass(s) * 100)
#   
#   # apply texture class rules
#   s[[var]] <- ssc_to_texcl(sand = s$SAND, clay = s$CLAY, as.is = TRUE)
#   
#   # maybe useful: number of samples in the source texture class
#   hit.rate <- prop.table(table(factor(s[[var]] == this.class)))
#   
#   # truncate samples to source texture class
#   s.trunc <- s[as.character(s[[var]]) == this.class, ]
#   
#   # package results into a list
#   res <- list(
#     samples.truncated = s.trunc,
#     samples.full = s,
#     mean = mean.comp,
#     var = var.comp,
#     hit.rate = hit.rate,
#     D.alpha = D$alpha,
#     texture.class.varname = var
#   )
#   
#   return(res)
# }


plotSummary <- function(ssc, alpha = c(0.25, 0.5)) {
  
  ## hackish, sample data subject to change
  
  # subset grid of SSC values from aqp::soiltexture$values object
  tclnm <- ssc$texture.class.varname
  idx <- which(soiltexture$values$texcl == ssc$samples.truncated[[tclnm]][1])
  
  zz <- soiltexture$values[idx, ]
  names(zz) <- c('SAND', 'SILT', 'CLAY')
  
  
  TT <- TT.plot(
    class.sys = "USDA-NCSS.TT",    # use "our" texture triangle
    main = "Soil Textures",          # title
    tri.sum.tst = FALSE,            # do not test for exact sum(sand, silt, clay) == 100
    cex.lab = 0.75,                 # scaling of label text
    cex.axis = 0.75,                # scaling of axis
    frame.bg.col = 'white',         # background color
    class.lab.col = 'black',        # color for texture class labels
    lwd.axis = 1.5,                    # line thickness for axis
    arrows.show = TRUE
  )
  
  # annotate grid samples
  TT.points(tri.data = zz, geo = TT, col='grey', pch = '.', cex = 0.01)
  
  # full samples, these will extend beyond original class limits
  TT.points(tri.data = ssc$samples.full, geo = TT, col=alpha('royalblue', alpha[1]), cex = 0.5)
  
  # samples truncated to class boundaries
  TT.points(tri.data = ssc$samples.truncated, geo = TT, col=alpha('firebrick', alpha[2]), pch = 15, cex = 0.5)
  
  # compositional mean
  TT.points(tri.data = data.frame(t(ssc$mean)), geo = TT, bg='firebrick', pch = 22, cex = 1, lwd = 1)
  
  # uniform / grid mean
  idx <- which(soiltexture$averages$texcl == ssc$samples.truncated[[tclnm]][1])
  m <- soiltexture$averages[idx, ]
  names(m) <- toupper(names(m))
  
  TT.points(tri.data = m, geo = TT, bg='darkgreen', pch = 22, cex = 1, lwd = 1)
  
  # uniform / grid samples
  # same number as simulated
  ss <- texcl_to_ssc(ssc$samples.truncated[[tclnm]], sample = TRUE)
  names(ss) <- toupper(names(ss))
  
  TT.points(tri.data = ss, geo = TT, col=alpha('darkgreen', alpha[2]), pch = 15, cex = 0.5)
  
  legend('topright', legend = c('Uniform / Grid', 'Dirichlet'), bty = 'n', pch = 15, col = c('darkgreen', 'firebrick'), horiz = TRUE, pt.cex = 1.5)
  
  txt <- sprintf("hit rate: %s%%", round(ssc$hit.rate[2] * 100))
  mtext(txt, side = 3, at = 0, line = -4)
}



texture.class.parameters <- map(cl, .f = prepareCompositionalSummary, .progress = TRUE)

z <- map(texture.class.parameters, .f = sampleComposition, n = 250, .progress = TRUE)



plotSummary(z[['c']], alpha = c(0.215, 0.33))
plotSummary(z[['cl']], alpha = c(0.215, 0.33))
plotSummary(z[['sl']], alpha = c(0.215, 0.33))
plotSummary(z[['sil']], alpha = c(0.215, 0.33))
plotSummary(z[['sl']], alpha = c(0.215, 0.33))
plotSummary(z[['scl']], alpha = c(0.215, 0.33))
plotSummary(z[['sc']], alpha = c(0.215, 0.33))
plotSummary(z[['s']], alpha = c(0.215, 0.33))
plotSummary(z[['sicl']], alpha = c(0.215, 0.33))
plotSummary(z[['l']], alpha = c(0.215, 0.33))
plotSummary(z[['si']], alpha = c(0.215, 0.33))


z[['l']]

lapply(z, '[[', 'hit.rate')

texture.class.parameters[['c']]

## TODO: save texture.class.parameters -> aqp data object for use in soil texture functions


