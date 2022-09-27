library(aqp)
library(soilDB)
library(vegan)
library(sharpshootR)
library(cluster)


s <- c('drummer', 'musick', 'pierre', 'lucy', 'cecil', 'miami')


osds <- fetchOSD(s)

x <- fetchKSSL(series = s)
x$taxonname <- factor(tolower(x$taxonname))

# truncate to 120 cm
x <- trunc(x, z1 = 0, z2 = 120)

# flag those soils with complete data in clay and CEC columns
x$.flag <- profileApply(x, function(i) {
  all(!is.na(i$clay) & !is.na(i$cec7))
})

# subset collection
x <- subset(x, .flag == TRUE)

# distance matrix
d <- profile_compare(x, vars=c('clay', 'estimated_ph_h2o', 'cec7', 'estimated_om'), k=0, max_d=120, rescale.result=FALSE)

# divisive hierarchical clustering
dd <- diana(d)

par(mar=c(0,0,3,1))
plotProfileDendrogram(x, dd, scaling.factor = 1, label='taxonname', name='hzn_desgn', color='estimated_ph_h2o', y.offset = 1, width=0.25, name.style = 'center-center')


# cross-tab 6 groups -- taxonname
cl <- cutree(as.hclust(dd), k = 6)
tx <- x$taxonname[match(names(cl), profile_id(x))]

addmargins(
  table(
    tx, 
    cl
  )
)


text(x = 1:length(dd$order), y = 0, cl[dd$order], cex = 0.66, font = 2)



## map distance matrix to 2D space via principal coordinates
d.betadisper <- betadisper(d, group=x$taxonname, bias.adjust = TRUE, sqrt.dist = FALSE, type='median')

## fancy plot
par(mar=c(3,3,3,1), mfcol=c(1,3))

boxplot(d.betadisper, varwidth=TRUE, las=1)

plot(
  d.betadisper, hull=FALSE, ellipse=TRUE, conf=0.5, las=1,
  col=c('Royalblue', 'Orange', 'Darkgreen', 'Firebrick'), 
  main='Ordination of Between-Profile Distances\n50% Probability Ellipse',
  xlab='', ylab=''
)


SoilTaxonomyDendrogram(osds, width=0.3, name.style = 'center-center', cex.taxon.labels = 1)







par(mar=c(3,8,3,1), mfcol=c(1,1))
plot(TukeyHSD(d.betadisper, conf.level = 0.85), las=1)


