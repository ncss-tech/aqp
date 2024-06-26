# load libraries
library(aqp)
library(soilDB)
library(lattice)
library(latticeExtra)
library(tactile)
library(grid)

# define plotting style
tps <- tactile.theme(superpose.line=list(col=c('RoyalBlue', 'DarkRed', 'DarkGreen'), lwd=2))

# get multiple series' worth of data
# TODO: add code to deal with those series that return 0 records
s <- c('holland', 'chaix')
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


# perform slice-wise Tukey HSD
# results contain a list of HSD + output from slab
# also, metadata related to depth intervals and confidence
HSD <- slicedHSD(g, fm = 0:100 ~ bs82 | taxonname, conf = 0.9)

# check: looks good
lapply(HSD, head)


## attempt to vizualize this stuff

# color segments according to p.adj
col.at <- c(0, 0.05, 0.1, 0.5, 1)

# manually created color key
ck <- list(
  at = col.at,
  labels = list(
    at = col.at,
    labels = col.at, 3,
    rot = 0
  ),
  space = 'right',
  height = 0.75, width = 1.5
)


# standard output from slab()
p.1 <- xyplot(top ~ p.q50 | variable, groups=taxonname, data=HSD$agg, ylab='Depth (cm)',
              xlab='median bounded by 25th and 75th percentiles',
              lower=HSD$agg$p.q25, upper=HSD$agg$p.q75, ylim=c(105, -5),
              panel=panel.depth_function, alpha=0.25, sync.colors=TRUE,
              prepanel=prepanel.depth_function,
              cf=HSD$agg$contributing_fraction,
              par.strip.text=list(cex=0.8),
              strip=strip.custom(bg=grey(0.85)),
              scales=list(x=list(alternating=1, relation='free'), y=list(alternating=3)),
              par.settings=tps,
              auto.key=list(columns=2, lines=TRUE, points=FALSE)
)



## TODO:
# consider filled / open symbols via: pch = HSD$HSD$p.adj < 0.05,
# automate figures via helper function

# experimental HSD viz
p.2 <- segplot(
  hzn_top ~ lwr + upr, centers = diff, level = p.adj, data = HSD$HSD, 
  col.regions = hcl.colors, ylim = c(105, -5), 
  xlab = 'HSD',
  at = col.at,
  colorkey = ck,
  panel = function(...) {
    panel.grid(h = -1, v = -1, lty = 3, col = 1)
    panel.abline(v=0, lwd = 2)
    panel.segplot(...)
  }
)


## entirely different approach, likely better!
## consider adding to panel.depth.function

r <- seq(HSD$min.depth, HSD$max.depth, by = 1)
cols <- c(grey(0.85), 'darkgreen')
idx <- as.numeric(HSD$HSD$p.adj <= 0.05) + 1

p.3 <- p.1 + layer(
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

# manual intervention
row.names(p.3) <- 'Variable of Interest (units)'

## merge panels
pp <- c(p.3, p.2, x.same = FALSE, y.same = TRUE, merge.legends = TRUE)
pp <- update(pp, scales = list(y = list(rot = 0)), ylab = 'Depth (cm)', ylim = c(105, -5))
pp <- resizePanels(pp, w = c(1, 0.5))

# manually fix panel names
row.names(pp) <- c('Base Saturation ph 8.2 (%)', 'HSD')

# wow, this is sometimes required to "fix" the HSD panel
# https://stackoverflow.com/questions/34645201/change-x-axis-limits-on-stratigraphic-plots-ie-multi-panel-plotshttps://stackoverflow.com/questions/34645201/change-x-axis-limits-on-stratigraphic-plots-ie-multi-panel-plots
pp$x.limits[[2]] <- c(min(HSD$HSD$lwr, na.rm = TRUE), max(HSD$HSD$upr, na.rm = TRUE))

# y-axis is perfectly aligned
pp


## inspect pieces
p.1

p.2

p.3


# ## alternative approach: not all that much better
# # however, y-axis isn't perfectly alligned
# print(p.1, more = TRUE, position = c(0, 0, 0.66, 1))
# print(p.2, more = FALSE, position = c(0.66, 0, 1, 0.99))
# 
# 


## what about LME?
library(nlme)
library(rms)

s <- dice(g, 0:25 ~ .)
horizons(s)$group <- denormalize(s, 'taxonname')
s$mid <- (s$hzn_top + s$hzn_bot) / 2

z <- horizons(s)

vars <- c('sliceID', 'wmpd', 'group', 'mid')
idx <- complete.cases(z[, vars])
z <- z[idx, vars]

dd <- datadist(z)
options(datadist="dd")

# GLS: I've used this before to parametrize correlation structure
(m.gls <- Gls(wmpd ~ rcs(mid) * group, data = z, correlation = corAR1(form = ~ mid | sliceID)))

plot(Predict(m.gls))

anova(m.gls)

## I don't really know what I am doing here...
# how to specify the correct random effect structure?

# LME: not sure how to parameterize correlation structure
# https://bbolker.github.io/mixedmodels-misc/notes/corr_braindump.html
# this gives a reasonable phi estimate, but clearly something is wrong
(m.lme <- lme(wmpd ~ group, random = ~ 1 | mid , data = z, correlation = corAR1(form = ~ 1 | mid )))


## ??
anova(m.lme)

intervals(m.lme, which = 'fixed')

