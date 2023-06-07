
devtools::load_all()
library(sharpshootR)


data("sierraTransect")

# local copy
x <- sierraTransect

.a <- list(color = 'Fe_o_to_Fe_d', name.style = 'center-center', plot.depth.axis = FALSE, hz.depths = TRUE, width = 0.3, cex.names = 0.75, cex.id = 0.66)
options(.aqp.plotSPC.args = .a)

par(mar = c(0, 0, 0, 0))
plotSPC(x)

groupedProfilePlot(x, groups = 'transect' , group.name.offset = -15)


plot(site(x)[, c('MAP_800', 'MAAT_800')])

plotSPC(x, plot.order = order(x$effective.ppt_800))


d1 <- NCSP(x, vars = 'Fe_o_to_Fe_d', rescaleResult = TRUE)

# result is within [0,1]
d2 <- compareSites(x, vars = c('MAP_800', 'MAAT_800'))


w1 <- 1
w2 <- 1

## idea: following symbolic representation used in SEM ... I think
d <- (w1*d1 + w2*d2) / sum(c(w1, w2))


plotProfileDendrogram(x, clust = cluster::diana(d1), max.depth = 180, width = 0.3, scaling.factor = 0.008)

plotProfileDendrogram(x, clust = cluster::diana(d2), max.depth = 180, width = 0.3, scaling.factor = 0.008)



plotProfileDendrogram(x, clust = cluster::diana(d), max.depth = 180, width = 0.3, scaling.factor = 0.006)
