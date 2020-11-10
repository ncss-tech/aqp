library(aqp)
library(soilDB)
library(reshape2)
library(latticeExtra)
library(viridis)
library(cluster)
library(sharpshootR)
library(Gmedian)

x <- fetchSDA(WHERE = "areasymbol = 'CA113'")

x <- filter(x, !is.na(taxgrtgroup))

table(x$taxgrtgroup)

a <- slab(x, taxgrtgroup ~ sandtotal_r + silttotal_r + claytotal_r + ec_r + om_r + dbthirdbar_r)

levels(a$variable) <- c('Sand (%)', 'Silt (%)', 'Clay (%)', 'EC', 'Organic Matter (%)', 'Db 1/3 Bar (g/cc)')

a <- a[a$bottom <= 150, ]

# define plotting style
tps <- list(superpose.line=list(col=c('RoyalBlue', 'DarkRed', 'DarkGreen'), lwd=2))

s <- a[a$taxgrtgroup == 'Haploxeralfs', ]

# plot grouped, aggregate data
xyplot(top ~ p.q50 | variable, data=s, ylab='Depth',
       xlab='median bounded by 25th and 75th percentiles',
       lower=s$p.q25, upper=s$p.q75, ylim=c(155,-5),
       panel=panel.depth_function, alpha=0.25, sync.colors=TRUE,
       prepanel=prepanel.depth_function,
       cf=s$contributing_fraction,
       par.strip.text=list(cex=0.8),
       strip=strip.custom(bg=grey(0.85)),
       layout=c(6,1), scales=list(x=list(alternating=1, relation='free'), y=list(alternating=3)),
       par.settings=tps
)

s <- a[a$taxgrtgroup == 'Fluvaquents', ]

# plot grouped, aggregate data
xyplot(top ~ p.q50 | variable, data=s, ylab='Depth',
       xlab='median bounded by 25th and 75th percentiles',
       lower=s$p.q25, upper=s$p.q75, ylim=c(155,-5),
       panel=panel.depth_function, alpha=0.25, sync.colors=TRUE,
       prepanel=prepanel.depth_function,
       cf=s$contributing_fraction,
       par.strip.text=list(cex=0.8),
       strip=strip.custom(bg=grey(0.85)),
       layout=c(6,1), scales=list(x=list(alternating=1, relation='free'), y=list(alternating=3)),
       par.settings=tps
)


s <- a[a$variable == 'Sand (%)', ]

# plot grouped, aggregate data
xyplot(top ~ p.q50 | taxgrtgroup, data=s, ylab='Depth',
       xlab='median bounded by 25th and 75th percentiles',
       lower=s$p.q25, upper=s$p.q75, ylim=c(155,-5),
       panel=panel.depth_function, alpha=0.25, sync.colors=TRUE,
       prepanel=prepanel.depth_function,
       cf=s$contributing_fraction,
       par.strip.text=list(cex=0.8),
       strip=strip.custom(bg=grey(0.85)),
       layout=c(5, 4), as.table = TRUE,
       scales=list(x=list(alternating=1), y=list(alternating=3)),
       par.settings=tps
)




plotSPC(filter(x, taxgrtgroup == 'Fluvaquents'), color = 'sandtotal_r', width = 0.3)
plotSPC(filter(x, taxgrtgroup == 'Fluvaquents'), color = 'om_r', width = 0.3)


s <- a[a$taxgrtgroup %in% c('Haploxeralfs', 'Endoaquerts', 'Argixerolls'), ]
s$taxgrtgroup <- factor(s$taxgrtgroup)

# plot grouped, aggregate data
xyplot(top ~ p.q50 | variable, data=s, groups = taxgrtgroup,
       ylab='Depth',
       xlab='median bounded by 25th and 75th percentiles',
       lower=s$p.q25, upper=s$p.q75, ylim=c(155,-5),
       panel=panel.depth_function, alpha=0.25, sync.colors=TRUE,
       prepanel=prepanel.depth_function,
       cf=s$contributing_fraction,
       par.strip.text=list(cex=0.8),
       strip=strip.custom(bg=grey(0.85)),
       layout=c(6,1), scales=list(x=list(alternating=1, relation='free'), y=list(alternating=3)),
       auto.key=list(columns=length(levels(s$taxgrtgroup)), lines=TRUE, points=FALSE),
       par.settings=tps
)





z <- dcast(a, taxgrtgroup + top + bottom ~ variable, value.var = 'p.q50')
depths(z) <- taxgrtgroup ~ top + bottom

h <- horizons(z)
h <- na.omit(h)

v <- c("Sand (%)", "Silt (%)", "Clay (%)", "EC", "Organic Matter (%)", "Db 1/3 Bar (g/cc)")

pp <- prcomp(h[, c("Sand (%)", "Silt (%)", "Clay (%)", "EC", "Organic Matter (%)", "Db 1/3 Bar (g/cc)")], center = TRUE, scale. = TRUE)

pr123 <- predict(pp)[, 1:3]
pr123 <- scales::rescale(pr123, to = c(0, 1))
cols <- rgb(pr123[, 1], pr123[, 2], pr123[, 3])

horizons(z) <- data.frame(hzID = h$hzID, pr123, cols = cols, stringsAsFactors = FALSE)


plotSPC(z, color = 'Sand (%)', divide.hz = FALSE, name = NA, width = 0.3)
plotSPC(z, color = 'Clay (%)', divide.hz = FALSE, name = NA, width = 0.3)

plotSPC(z, color = 'PC1', divide.hz = FALSE, name = NA, width = 0.3)
plotSPC(z, color = 'cols', divide.hz = FALSE, name = NA, width = 0.3)

z$ssc_total <- z$`Sand (%)` + z$`Silt (%)` + z$`Clay (%)`
plotSPC(z, color = 'ssc_total', divide.hz = FALSE, name = NA, width = 0.3)

d <- profile_compare(z, vars = v, max_d = 100, k = 0)

hc <- as.hclust(diana(d))

par(mar = c(0, 0, 0, 1), bg = 'black', fg = 'white')

plotProfileDendrogram(z, clust = hc, y.offset = 3, scaling.factor = 0.6, color = 'PC1', divide.hz = FALSE, name = NA, width = 0.3)


plotProfileDendrogram(z, clust = hc, y.offset = 3, scaling.factor = 0.6, color = 'PC2', divide.hz = FALSE, name = NA, width = 0.3)

plotProfileDendrogram(z, clust = hc, y.offset = 3, scaling.factor = 0.6, color = 'cols', divide.hz = FALSE, name = NA, width = 0.3, cex.names = 0.8)



### L1 median

# re-format for slice-wise L1m median
s <- slice(x, 0:150 ~ sandtotal_r + silttotal_r + claytotal_r + ec_r + om_r + dbthirdbar_r, strict = FALSE)

# work on de-normalized data as a DF
h <- as(s, 'data.frame')
# vars of iterest
vars <- c("sandtotal_r", "silttotal_r", "claytotal_r", "ec_r", "om_r", "dbthirdbar_r")

# iterate over groups
s.list <- split(h, list(h$taxgrtgroup))
.G <- lapply(s.list, FUN=function(i) {
  
  # iterate over depth slices
  ll <- split(i, i$hzdept_r)
  res <- lapply(ll, function(j) {
    
    # NA not allowed, explicitly filter
    no.na <- na.omit(j[, vars])
    
    # catch conditions where there are no data
    # TODO: what is the min number of required records?
    if(nrow(no.na) < 1)
      return(NULL)
    
    # L1 median
    G <- data.frame(Gmedian(no.na))
    
    # original names
    names(G) <- vars
    
    # package group + depth slice + L1 data
    data.frame(taxgrtgroup = j$taxgrtgroup[1], top = j$hzdept_r[1], bottom = j$hzdepb_r[1], G)
  })
  
  # list -> DF
  res <- do.call('rbind', res)
  
  return(res)
})

# list -> DF
.G <- do.call('rbind', .G)

# init SPC
depths(.G) <- taxgrtgroup ~ top + bottom

# check that sand + silt + clay sum to 100
.G$ssc_total <- .G$sandtotal_r + .G$silttotal_r + .G$claytotal_r

# nice, they do!
plotSPC(.G, color = 'ssc_total', divide.hz = FALSE, name = NA, width = 0.3)

# other properties
plotSPC(.G, color = 'sandtotal_r', divide.hz = FALSE, name = NA, width = 0.3)
plotSPC(.G, color = 'om_r', divide.hz = FALSE, name = NA, width = 0.3)
plotSPC(.G, color = 'ec_r', divide.hz = FALSE, name = NA, width = 0.3)

# subset complete cases of horizon data
h <- horizons(.G)
h <- na.omit(h)

# principal components with centering / scaling
pp <- prcomp(h[, vars], center = TRUE, scale. = TRUE)

# extract PC[1,2,3]
pr123 <- predict(pp)[, 1:3]
# rescale to [0,1]
pr123 <- scales::rescale(pr123, to = c(0, 1))
# sRGB composite from PC[1,2,3]
cols <- rgb(pr123[, 1], pr123[, 2], pr123[, 3])

# safely merge via left-join
horizons(.G) <- data.frame(hzID = h$hzID, pr123, cols = cols, stringsAsFactors = FALSE)


# pair-wise distances
d <- profile_compare(.G, vars = vars, max_d = 100, k = 0)

# clustering
hc <- as.hclust(diana(d))

# viz
par(mar = c(0, 0, 0, 1), bg = 'black', fg = 'white')

plotProfileDendrogram(.G, clust = hc, y.offset = 3, scaling.factor = 0.6, color = 'sandtotal_r', divide.hz = FALSE, name = NA, width = 0.3)

plotProfileDendrogram(.G, clust = hc, y.offset = 3, scaling.factor = 0.6, color = 'ec_r', divide.hz = FALSE, name = NA, width = 0.3)

plotProfileDendrogram(.G, clust = hc, y.offset = 3, scaling.factor = 0.6, color = 'PC1', divide.hz = FALSE, name = NA, width = 0.3)

plotProfileDendrogram(.G, clust = hc, y.offset = 3, scaling.factor = 0.6, color = 'cols', divide.hz = FALSE, name = NA, width = 0.3, cex.names = 0.8)


par(mfrow = c(2,1))






### slice-wise HSD



y <- filter(x, taxgrtgroup %in% c('Haploxeralfs', 'Argixerolls'))
y$taxgrtgroup <- factor(y$taxgrtgroup)
HSD <- slicedHSD(y, fm = 0:150 ~ sandtotal_r | taxgrtgroup)





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
p.1 <- xyplot(top ~ p.q50 | variable, groups=taxgrtgroup, data=HSD$agg, ylab='Depth',
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
  hzdept_r  ~ lwr + upr, centers = diff, level = p.adj, data = HSD$HSD, 
  col.regions = viridis, ylim = c(105, -5), 
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
row.names(pp) <- c('Total Sand (%)', 'HSD')

# wow, this is sometimes required to "fix" the HSD panel
# https://stackoverflow.com/questions/34645201/change-x-axis-limits-on-stratigraphic-plots-ie-multi-panel-plotshttps://stackoverflow.com/questions/34645201/change-x-axis-limits-on-stratigraphic-plots-ie-multi-panel-plots
pp$x.limits[[2]] <- c(min(HSD$HSD$lwr, na.rm = TRUE), max(HSD$HSD$upr, na.rm = TRUE))

# y-axis is perfectly aligned
pp


## inspect pieces
p.1

p.2

p.3




