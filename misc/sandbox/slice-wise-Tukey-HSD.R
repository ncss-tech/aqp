# load libraries
library(aqp)
library(soilDB)
library(lattice)
library(latticeExtra)
library(viridis)
library(grid)

source('HSD-functions.R')

# define plotting style
tps <- list(superpose.line=list(col=c('RoyalBlue', 'DarkRed', 'DarkGreen'), lwd=2))

# get multiple series' worth of data
# TODO: add code to deal with those series that return 0 records
s <- c('holland', 'pierre')
g <- fetchKSSL(s)

# check taxonname for variability
# letter case etc.
table(g$taxonname)

# this usually works to normalize taxonnames
# unless!! one of the names is found within another
# a better approach would pick the shortest match
for(i in s) {
  g$taxonname[grep(pattern = i, x = g$taxonname, ignore.case = TRUE)] <- i
}

# check normalization: OK
table(g$taxonname)

# convert to factor here so that levels are enforced later
g$taxonname <- factor(g$taxonname)


# compute weighted-mean particle diameter
g$wmpd <- with(horizons(g), ((vcs * 1.5) + (cs * 0.75) + (ms * 0.375) + (fs * 0.175) + (vfs *0.075) + (silt * 0.026) + (clay * 0.0015)) / (vcs + cs + ms + fs + vfs + silt + clay))


# plot the grouped object, with profiles arranged by taxonname
par(mar=c(0,1,5,1))
groupedProfilePlot(g, groups = 'taxonname', color='sand', group.name.offset = -10, print.id=FALSE)
groupedProfilePlot(g, groups = 'taxonname', color='wmpd', group.name.offset = -10, print.id=FALSE)
groupedProfilePlot(g, groups = 'taxonname', color='estimated_ph_h2o', group.name.offset = -10, print.id=FALSE)
groupedProfilePlot(g, groups = 'taxonname', color='estimated_oc', group.name.offset = -10, print.id=FALSE)


##
## move the following into a specialized function 
##


## aqp TODO: 
# * hzdesgn / horizon texture class are not preserved over slice (should it be?)
# [, i] expects it to be (should it?)
#

# don't enforce strict horizon depth checking
# use results with caution
s <- slice(g, 0:100 ~ hzn_desgn + lab_texture_class + estimated_oc, strict = FALSE)

# aggregate by taxoname to check depth patterns
a <- slab(g, fm = taxonname ~ estimated_oc, slab.structure = 0:100)




# iterate over slice-index and perform Tukey's HSD
# this likely violates the "multiple comparison" criteria... but I am not a statistician
HSD <- lapply(1:100, slicedHSD, var = 'estimated_oc', group = 'taxonname', conf = 0.95)

# list -> DF
HSD <- do.call('rbind', HSD)

# check: not too bad
head(HSD)

## TODO: there must be something better...
# color segments according to p.adj
col.at <- c(0, 0.05, 0.1, 0.5, 1)

ck <- list(
  at = col.at,
  labels = list(
    at = col.at,
    labels = col.at, 3,
    rot = 45
  ),
  space = 'bottom',
  height = 0.5, width = 1.5
)


p.1 <- segplot(
  top ~ lwr + upr, centers = diff, level = p.adj, data = HSD, 
  col.regions = viridis, ylim = c(105, -5), 
  at = col.at,
  colorkey = ck,
  panel = function(...) {
    panel.abline(v=0, lty = 3)
    panel.segplot(...)
  }
)

# plot grouped, aggregate data
p.2 <- xyplot(top ~ p.q50 | variable, groups=taxonname, data=a, ylab='Depth',
       xlab='median bounded by 25th and 75th percentiles',
       lower=a$p.q25, upper=a$p.q75, ylim=c(105, -5),
       panel=panel.depth_function, alpha=0.25, sync.colors=TRUE,
       prepanel=prepanel.depth_function,
       cf=a$contributing_fraction,
       par.strip.text=list(cex=0.8),
       strip=strip.custom(bg=grey(0.85)),
       scales=list(x=list(alternating=1, relation='free'), y=list(alternating=3)),
       par.settings=tps,
       auto.key=list(columns=2, lines=TRUE, points=FALSE)
)

pp <- c(p.1, p.2, y.same = FALSE)
row.names(pp) <- c('HSD', 'Var')
pp <- update(pp, scales = list(y = list(rot = 0)), ylab.right = 'Depth (cm)')
pp <- resizePanels(pp, w = c(0.5, 1))

pp



## entirely different approach, likely better!
## consider adding to panel.depth.function

r <- seq(0, 100, by = 1)
cols <- c('grey', 'royalblue')
idx <- as.numeric(HSD$p.adj <= 0.05) + 1

p.2 + layer(
  grid.rect(
    x = unit(0.025, 'npc'), 
    y = unit(r[-length(r)], 'native'), 
    width = unit(0.025, 'npc'), 
    height = unit(-1, 'native'), 
    vjust = 1,
    gp = gpar(
      col = NA,
      fill = cols[idx]
    ))
)




