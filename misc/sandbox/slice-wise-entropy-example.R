library(aqp)
library(latticeExtra)
library(plyr)
library(Hmisc)
library(reshape)


data(sp3)
depths(sp3) <- id ~ top + bottom

# how can we compute entropy from continuous variables?
# http://en.wikipedia.org/wiki/Differential_entropy
# http://cran.r-project.org/web/packages/entropy/

# calculation for continous random vars based on binning / counts
# http://cran.r-project.org/web/packages/entropy/entropy.pdf

## this isn't correct, and barfs when there is < 90% data available
f.entropy <- function(v) {
  # compute density, will use to map values -> probabilities
  # density has constraints on non-NA sample size
  d <- density(v, na.rm = TRUE, bw=1)
  # map values -> p via density estimation
  f <- splinefun(d)
  p <- f(v)
  # ... this doesn't sum to 1, density() is only approximate
  
  # shannon entropy is based on probabilities... which sum to 1
  h <- -sum(p * log(p, base=length(p)))
  
  # return fake lower / upper
  res <- c(value=h, lower=NA, upper=NA)
  return(res)
}

f.sig.to.noise <- function(v) {
  res <- mean(v, na.rm=TRUE) / sd(v, na.rm=TRUE)
    
  # return fake lower / upper
  res <- c(value=res, lower=NA, upper=NA)
  return(res)
}

# http://en.wikipedia.org/wiki/Quartile_coefficient_of_dispersion
# http://stats.stackexchange.com/questions/38635/ratio-of-range-to-iqr-vs-coefficient-of-variation-which-is-the-more-useful-r
f.qcd <- function(v) {
  res <- IQR(v, na.rm=TRUE) / median(v, na.rm=TRUE)
  
  # return fake lower / upper
  res <- c(value=res, lower=NA, upper=NA)
  return(res)
}

## this isn't possible as slab functions can only "see" the current slab
# QCD of z-scores ?

mean.and.sd <- function(values) {
  m <- mean(values, na.rm=TRUE)
  s <- sd(values, na.rm=TRUE)
  upper <- m + s
  lower <- m - s
  res <- c(value=m, lower=lower, upper=upper)
  return(res)
}



## does this make sense?
# simulate a no-information, baseline QCD, from all original, sliced values
no.information.qcd <- function(i, n.slices=101){
  # "i" is a chunk of data.frame
  x <- i$value
  
  # remove NA
  not.NA.idx <- which(!is.na(x))
  x <- x[not.NA.idx]
  
  # remove O's
  non.zero.idx <- which(x != 0)
  x <- x[non.zero.idx]
  
  # this is the mean QCD over then entire set of property 'x'
  qcd <- IQR(x) / median(x)
  
  # this is the sum of mean QCD over the number of depth slices
  qcd.sum <- qcd * n.slices
  
  d <- data.frame(mean.ni.qcd=qcd, sum.ni.qcd=qcd.sum)
  return(d)
}



## weighted mean QCD, weighted by contributing fraction
## must remove 0's and NA before computing weighted mean QCD
## when n=1, IQR = 0 => QCD = 0
wtd.mean.qcd <- function(i){
  # "i" is a chunk of data.frame
  x <- i$value
  w <- i$contributing_fraction
  
  # remove NA
  not.NA.idx <- which(!is.na(x))
  x <- x[not.NA.idx]
  w <- w[not.NA.idx]
  
  # remove O's
  non.zero.idx <- which(x != 0)
  x <- x[non.zero.idx]
  w <- w[non.zero.idx]
  res <- wtd.mean(x, weights=w, na.rm=TRUE)
  
  #   ## this doesn't make much sense...
  #   # normalize to some kind of baseline
  #   n <- length(x)
  #   r <- sample(x, size=n * 100, prob = w, replace = TRUE)
  #   base.qcd <- IQR(r) / median(r)
  
  d <- data.frame(wt.mean.qcd=res)
  return(d)
}

## weighted sum QCD
## must remove 0's and NA before computing weighted mean QCD
## when n=1, IQR = 0 => QCD = 0
wtd.sum.qcd <- function(i){
  # "i" is a chunk of data.frame
  x <- i$value
  w <- i$contributing_fraction
  
  # remove NA
  not.NA.idx <- which(!is.na(x))
  x <- x[not.NA.idx]
  w <- w[not.NA.idx]
  
  # remove O's
  non.zero.idx <- which(x != 0)
  x <- x[non.zero.idx]
  w <- w[non.zero.idx]
  
  # compute weighted sum
  res <- sum(x * w)
  
  #   ## this doesn't make much sense...
  #   # normalize to some kind of baseline,
  #   # re-sampled sum(QCD) from 10x original number of slices
  #   n <- length(x)
  #   s <- replicate(n, sample(x, size=n*10, prob = w, replace = TRUE))
  #   base.qcd <- sum(apply(s, 2, function(i) IQR(i) / median(i)))
  
  d <- data.frame(wt.sum.qcd=res)
  return(d)
}


# compute some "information" metrics
a <- slab(sp3,  ~ clay + A + cec + ph, slab.fun=mean.and.sd, slab.structure=0:100)
a.1 <- slab(sp3,  ~ clay + A + cec + ph, slab.fun=f.entropy, slab.structure=0:100)
a.2 <- slab(sp3, ~ clay + A + cec + ph, slab.fun=f.sig.to.noise, slab.structure=0:100)
a.3 <- slab(sp3, ~ clay + A + cec + ph, slab.fun=f.qcd, slab.structure=0:100)

# combine
g <- make.groups(summary=a, entropy=a.1, sig.to.noise=a.2, qcd=a.3)
g$which <- factor(g$which, labels=c('Mean +/- 1SD', 'psuedo-Entropy', 'Signal : Noise', 'QCD'))

p <- xyplot(
  top ~ value | which + variable, data=g,
  lower=g$lower, upper=g$upper, sync.colors=TRUE, alpha=0.5,
  cf=g$contributing_fraction,
  ylab='Depth (cm)',
  xlab='',
  ylim=c(100,-5), layout=c(5,3), scales=list(x=list(relation='free')),
  par.settings=list(superpose.line=list(lwd=2, col=c('RoyalBlue', 'Orange2'))),
  panel=panel.depth_function, 
  prepanel=prepanel.depth_function,
  auto.key=list(columns=2, lines=TRUE, points=FALSE)
)

## not quite right, as x-axis shouldn't float...
useOuterStrips(p, strip=strip.custom(bg=grey(0.85)), strip.left = strip.custom(horizontal=FALSE, bg=grey(0.85)))


## a "no-information QCD" must be computed from the raw data, by depth-slice
s <- slice(sp3, 0:100 ~ clay + A + cec + ph, just.the.data = TRUE)
s.long <- melt(s, id.vars = c('top', 'bottom'), measure.vars = c('clay', 'A', 'cec', 'ph'))
qcd.ni <- ddply(s.long, c('variable'), no.information.qcd)


## compute weighted mean and weighted sum QCD by variable
## note that these must be standardized by slice-wise "no-information" QCD
wm.qcd <- ddply(a.3, 'variable', .fun=wtd.mean.qcd)
ws.qcd <- ddply(a.3, 'variable', .fun=wtd.sum.qcd)

### does this make sense?
# join QCD summaries to "no-information" baseline
ss <- join(join(wm.qcd, ws.qcd), qcd.ni)
transform(ss, mean.qcd=wt.mean.qcd / mean.ni.qcd, sum.qcd=wt.sum.qcd / sum.ni.qcd)



