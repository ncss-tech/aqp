# Based on Koop et al., 2020
# https://www.sciencedirect.com/science/article/pii/S0016706119304069


# Outline:
# 
# * within each profile of an SPC
#   - mark each horizon as target (illuvial in paper), reference (elluvial) via regex(hzdesgn)
#   - compute thickness-wt mean + SD of associated properties
#   - compute scores for all horizons
#   - compute HDI for each horizon (assumptions about theoretical min/max)
#   - optionally compute PDI (thickness-wt mean over all horizons)
# 
# Considerations / Assumptions:
# 
# * Each index relies on different properties and scoring rules
#   - soil color: CIELAB L and A coordinates
#   - clay films: scoring rule for clay film distinctness
#   - pedogenic carbonates: scoring rules for 'k' and 'kk'
#   - particle size distribution: particle diameters
# * REGEX patterns for matching target and reference horizons
#   - check for overlapping patterns
# * missing data?
# * single horizon target or reference ==> wt SD is 0 (!!)



library(aqp)

data(osd)
x <- osd


# soil color example

par(mar = c(0, 0, 0, 2.5))
plotSPC(x, name.style = 'center-center', cex.names = 1)

# required soil property data
.lab <- munsell2rgb(x$hue, x$value, x$chroma, returnLAB = TRUE)
horizons(x) <- data.frame(hzID = x$hzID, .lab)

# mark horizons
.target <- grepl(pattern = 'A', x[[hzdesgnname(x)]])
.reference <- grepl(pattern = 'E|B|C', x[[hzdesgnname(x)]])

# test for overlap
.target & .reference

# NA signifies no match, likely irrelevant horizons
.mark <- rep(NA_character_, times = length(.target))
.mark[which(.target)] <- 'target'
.mark[which(.reference)] <- 'reference'

# graphical check
x$.mark <- .mark
plotSPC(x, name.style = 'center-center', cex.names = 1, color = '.mark')


.hd <- horizonDepths(x)
.hdn <- hzidname(x)

# working with a single member of SPC
h <- horizons(x[1, ])
h$.thick <- h[[.hd[2]]] - h[[.hd[1]]]

# subset required properties
h <- h[, c(.hdn, '.mark', '.thick', 'L', 'A')]

# remove all NA
h <- na.omit(h)

# check for 0-row possibility

# check for single-row possibility
# --> wt. SD is 0

# split into target / reference for simplicity
z <- split(h, h$.mark)


# weighted means, NA have been removed
wm <- lapply(z, function(i) {
  
  Lbar <- weighted.mean(x = i$L, w = i$.thick)
  Abar <- weighted.mean(x = i$A, w = i$.thick)
  
  return(c(Lbar = Lbar, Abar = Abar))
})


# modified weighted SD, NA have been removed
# TODO: this may need to be applied to all records
#       --> how to interpret h bar x? which mean is this?

ws <- lapply(z, function(i) {
  
  .n <- nrow(i)
  
  # S tilda L
  .top <- .n * sum(i$.thick * (i$L - mean(i$L))^2)
  .bottom <- (.n - 1) * sum(i$.thick)
  
  StL <- sqrt(.top / .bottom)
  
  # S tilda A
  .top <- .n * sum(i$.thick * (i$A - mean(i$A))^2)
  .bottom <- (.n - 1) * sum(i$.thick)
  
  StA <- sqrt(.top / .bottom)
  
  # TODO: consider setting St* to 1 when .n < 2
  
  return(c(StL = StL, StA = StA))
})


# scores

# HDI









