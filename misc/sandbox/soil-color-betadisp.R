library(aqp)
library(soilDB)
library(vegan)
library(cluster)
library(sharpshootR)
library(RColorBrewer)

s.list <- c('amador', 'redding', 'pentz', 'willows', 'pardee', 'yolo', 'hanford', 'cecil', 'sycamore', 'KLAMATH', 'MOGLIA', 'vleck', 'drummer', 'CANEYHEAD', 'musick', 'sierra', 'HAYNER', 'zook', 'argonaut', 'PALAU')

# get these soil series
s <- fetchOSD(s.list)

# manually convert Munsell -> sRGB
rgb.data <- munsell2rgb(s$hue, s$value, s$chroma, return_triplets = TRUE)
s$r <- rgb.data$r
s$g <- rgb.data$g
s$b <- rgb.data$b

k <- 4
pig <- soilColorSignature(s, RescaleLightnessBy = 5, method = 'pam', pam.k = k)

# iterate over clusters, result is a distance matrix (delta-E00)
delta.E00 <- lapply(1:k, function(i) {
  # LAB coordinates are named by cluster 1:k
  v.names <- paste(c('L', 'A', 'B'), i, sep = '.')
  # delta-E00
  d.i <- farver::compare_colour(pig[, v.names], from_space='lab', white_from = 'D65', method='cie2000')
  # copy over SPC ids
  dimnames(d.i) <- list(pig[, 1], pig[, 1])
  # convert to dist object
  d.i <- as.dist(t(d.i))
  return(d.i)
})

# sum distance matrices
d <- Reduce('+', delta.E00)
# divisive clustering
dd <- diana(d)


# interesting groups of soils
s$color.group <- factor(cutree(dd, 3))


## map distance matrix to 2D space via principal coordinates
d.betadisper <- betadisper(d, group=s$color.group, bias.adjust = TRUE, sqrt.dist = FALSE, type='median')

## fancy plot
# define some nice colors
cols <- brewer.pal(9, 'Set1') 
# remove light colors
cols <- cols[c(1:5,7,9)]


par(mar=c(0,0,1,1))

plotProfileDendrogram(s, dd, dend.y.scale = max(d) * 2, scaling.factor = 0.45, y.offset = 6, width=0.3, cex.names=0.45, name.style='left-center')

ape::tiplabels(s$color.group, bg = NA, frame = 'none', offset = 2, font=2, cex=0.85)


par(mar=c(1,1,3,1))

p <- plot(
  d.betadisper, hull=FALSE, ellipse=TRUE, conf=0.9,
  col=cols, main='Soil Color Groups\n90% Probability Ellipse', sub='MLRA 15, 18, 22A, 22B'
)
ordilabel(p, labels = site(s)$id, cex=0.75)




## better example, using surface soil color for several soil series


x <- fetchKSSL(series=c('holland', 'drummer'), returnMorphologicData = TRUE, simplifyColors = TRUE)

s <- x$SPC
s$taxonname <- toupper(s$taxonname)
table(s$taxonname)

spc_in_sync(s)


# glomApply(s, function(p) c(0, 10))
#
# z <- trunc(s, 0, 15)

z <- slice(s, 0:15 ~ ., strict = FALSE)

# z <- filter(z, ! is.na(moist_soil_color) )

groupedProfilePlot(z, groups = 'taxonname', color='moist_soil_color', name=NA, print.id=FALSE)

h <- as(z, 'data.frame')

d <- farver::compare_colour(h[, c('m_r', 'm_g', 'm_b')], from_space='rgb', white_from = 'D65', method='cie2000')

# copy over SPC ids
dimnames(d) <- list(h$pedon_key, h$pedon_key)
# convert to dist object
d <- as.dist(t(d))


## map distance matrix to 2D space via principal coordinates
d.betadisper <- betadisper(d, group=h$taxonname, bias.adjust = TRUE, sqrt.dist = FALSE, type='median')



p <- plot(
  d.betadisper, hull=FALSE, ellipse=TRUE, conf=0.9,
  col=cols, main='Soil Color Groups\n90% Probability Ellipse', sub='MLRA 15, 18, 22A, 22B'
)
# ordilabel(p, labels = h$taxonname, cex=0.6)


##
q.col <- '10YR 3/4'
cc <- contrastChart(q.col, hues=c('10YR'), style = 'CC', returnData = TRUE)
print(cc$fig)

cc <- contrastChart(q.col, hues=c('7.5YR', '10YR'), style = 'hue', thresh = 16, returnData = TRUE)
print(cc$fig)

cc <- contrastChart(q.col, hues=huePosition(x=NULL, returnHues = TRUE), style = 'hue', thresh = 8, returnData = TRUE)
print(cc$fig)

cc <- contrastChart(q.col, hues=c('10YR', '5BG'), style = 'hue', returnData = TRUE)
print(cc$fig)

cc <- contrastChart(q.col, hues=c('10YR'), style = 'hue', returnData = TRUE)
print(cc$fig)

## can we model dE00 contours via gam?
library(mgcv)
m <- gam(dE00 ~ s(as.integer(chroma), value, bs='tp'), data=cc$data)
plot(m)

nd <- expand.grid(chroma=seq(1, 8, by=0.1), value=seq(2, 8, by=0.1))
nd$p <- predict(m, nd)

# interesting
levelplot(p ~ chroma * value, data=nd, col.regions=viridis::viridis, xlim = c(0.5, 8.5), ylim = c(1.5, 8.5))


d <- farver::compare_colour(cc$data[, c('L', 'A', 'B')], from_space='lab', white_from = 'D65', method='cie2000')

# convert to dist object
d <- as.dist(t(d))


## map distance matrix to 2D space via principal coordinates
d.betadisper <- betadisper(d, group=cc$data$cc, bias.adjust = TRUE, sqrt.dist = FALSE, type='median')

p <- plot(
  d.betadisper, hull=FALSE, ellipse=TRUE, conf=0.5,
  col=cols, main='Color Contrast\n50% Probability Ellipse', sub=q.col
)

ordisurf(p, cc$data$dE00, add=TRUE, col='black')

# ordilabel(p, labels = cc$data$munsell, cex=0.5)



##


q.col <- '10YR 3/4'
cc <- contrastChart(q.col, hues=c('2.5YR', '5YR', '7.5YR', '10YR', '2.5Y'), style = 'hue', returnData = TRUE)

d <- farver::compare_colour(cc$data[, c('L', 'A', 'B')], from_space='lab', white_from = 'D65', method='cie2000')


# convert to dist object
d <- as.dist(t(d))


mds <- MASS::isoMDS(d)

p <- ordiplot(mds, type='none', axes=FALSE, xlab='', ylab='')
points(p, 'sites', col=cc$data$color, pch=15)
ordisurf(p, cc$data$dE00, add=TRUE, col='black')

## how can you highlight the orignial color


