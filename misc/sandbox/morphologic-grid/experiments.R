library(raster)
library(rasterVis)
library(aqp)
library(RColorBrewer)
library(latticeExtra)
library(reshape2)

# site data
x.site <- read.csv('examples/hv4-site.csv', stringsAsFactors = FALSE)

# soil morphologic units (SMU)
x.smu <- read.csv('examples/hv4-smu.csv', stringsAsFactors = FALSE)


# rasterized from vector linework, scanned from field notes drawn to scale
# c/o Nic Jelinski <jeli0026@umn.edu>
x <- raster('examples/hv4.tif')

# check RAT via .vat.dbf
# OK
foreign::read.dbf('examples/hv4.tif.vat.dbf')

# check RAT
# same as DBF, good
(rat <- levels(x)[[1]])

# raster values are?
# 0-255
x

# it would appear that values 1-9 are data
# 10 is NODATA
# ID column doesn't match Value column
# re-set here, but that isn't permanent... why is this?
rat$ID <- rat$Value
levels(x) <- rat

# try setting NODATA, seems to work
NAvalue(x) <- 10

# levels of the horizon designation are not in depth-wise order and include NA
# ordering is semi-arbitrary but a depth-wise sorting is ideal
levels(rat$Horizon)

## TODO: consider establishing an ideal vertical ordering in the original raster and RAT 

## TODO: set levels later, using some kind of depth-wise eval

# remove "NA" from levels
# ideal ordering for soil morphologic units (SMU)
SMU.depthwise <- c('Oi', 'Oe', 'Cg', 'Cg/Oajj', 'Oa/Cgjj', 'Wf/2Cg1', 'Wf', 'Oaf', 'Wf/2Cg2')
rat$Horizon <- factor(rat$Horizon, levels = SMU.depthwise)
# re-pack
levels(x) <- rat

# removing NA from the RAT
rat <- na.omit(rat)
levels(x) <- rat

# check dimentions and resolution:
# extent seems to make sense but the pixels are smaller than 1x1 cm
x

# template 1x1 cm raster
x.simple <- raster(nrows=100, ncols=100, resolution=1, xmn=0, xmx=100, ymn=-100, ymx=0)

## ack: this drops the RAT
# try converting to 1x1 cm raster via NN
x.1x1 <- resample(x, x.simple, method='ngb')
levels(x.1x1) <- rat

# looks OK
pal <- colorRampPalette(brewer.pal(9, 'Set1'))
n <- nrow(rat)

# looks OK
plot(x, col=pal(n))
plot(x.1x1, col=pal(n))


## rasterVis will perform thematic mapping with a nice legend
## it will not respect levels of 'att'
levelplot(x, att='Horizon', margin=FALSE, par.settings=rasterTheme(region=pal(n)), scales=list(y=list(tick.number=12)))

(plot.1 <- levelplot(x.1x1, att='Horizon', margin=FALSE, par.settings=rasterTheme(region=pal(n)), scales=list(y=list(tick.number=12))))






# double check cell counts in RAT
# not quite the same
# leave out NA
hz.counts <- freq(x, useNA='no')
cbind(rat, reCount=hz.counts[, 2])

# convert into proportions / area weights
data.frame(hz=rat$Horizon, prop=round(hz.counts[, 2] / sum(hz.counts[, 2]), 3))

# try 1x1 cm raster
# very close: good
hz.counts.1x1 <- freq(x.1x1, useNA='no')
data.frame(hz=rat$Horizon, prop=round(hz.counts.1x1[, 2] / sum(hz.counts.1x1[, 2]), 3))


##
## likely simpler to do things by depth slice as matrix
## note: using 1x1 cm resampled raster so that depths are correct
## 

# convert cell values to matrix
m <- as.matrix(x.1x1)

# Pr(Hz | depth) via cell counts
# note: equal depths are expanded as columns
p <- apply(m, 1, function(i) {
  i <- factor(i, levels=rat$Value, labels=rat$Horizon)
  tab <- table(i)
  res <- tab / sum(tab)
  return(res)
})

# translate columns -> rows, mirrors depth logic
p <- t(p)

# viz of Pr(Hz | depth)
# about right
matplot(p, type = 'l', lty=1, col=pal(n), las=1, lwd=2)

# Shannon Entropy as an index of horizon confusion vs. depth
xyplot(1:100 ~ apply(p, 1, shannonEntropy), type=c('l', 'g'), lwd=2, col='RoyalBlue', asp=1.5, ylim=c(110, -10), ylab='Depth (cm)', xlab='Shannon Entropy')


# convert to data.drame, add depths
p <- as.data.frame(p)
p$top <- 0:(nrow(p)-1)
p$bottom <- p$top + 1

# reshape to long format for plotting
p.long <- melt(p, id.vars=c('top', 'bottom'))

# remove very small Pr(Hz)
# p.long$value[p.long$value == 0] <- NA 

# interesting but hard to read
(plot.2 <- xyplot(top ~ value, data=p.long, groups=variable, ylim=c(110, -10), type='l', par.settings=list(superpose.line=list(lwd=2, col=pal(n))), asp=1, auto.key=list(columns=3, lines=TRUE, points=FALSE, cex=0.7), ylab='Depth (cm)', xlab='Pr(Hz | depth)'))


## 
## determing rough depth ranges
##

# iteration likely simplest, save to a list
l <- list()

## TODO: this is kind of messy
## cell value should == factor level ordering for SMU

# iterate over raster CELL codes for SMU designation, not the same as levels
# these are stored in the matrix form

for(i in rat$ID) {
  # rows are the collection of SMU IDs by depth
  l[[i]] <- row(m)[which(m == i)]
}

# apply SMU designations / names
names(l) <- rat$Horizon

# re-order according to depth-wise SMU names
# note that we have to specify the LUT first in match()
l <- l[match(levels(rat$Horizon), names(l))]

# that looks about right
boxplot(l, las=1, ylab='Depth (cm)', ylim=c(100, 0))

lapply(l, range)
lapply(l, quantile, probs=c(0.25, 0.75))
sort(sapply(l, median))


## notes for next time
# 1. cell values, SMU names, and integer representation of SMU are easily mixed-up!
# 2. consider storing SMU levels in matrix form




