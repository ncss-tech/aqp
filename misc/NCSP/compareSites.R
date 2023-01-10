
devtools::load_all()
library(sharpshootR)


data("sierraTransect")

# TODO: rebuild this, contains old-style @sp and other issues
x <- sierraTransect
x <- rebuildSPC(x)

hzdesgnname(x) <- 'name'

par(mar = c(0, 0, 0, 1))
plotSPC(x, color = 'Fe_o_to_Fe_d', name.style = 'center-center')

groupedProfilePlot(x, groups = 'transect', color = 'Fe_o_to_Fe_d', name.style = 'center-center', group.name.offset = -15)


d1 <- NCSP(x, vars = 'Fe_o_to_Fe_d', rescaleResult = TRUE)

par(mar = c(0, 0, 0, 1))
plotProfileDendrogram(x, clust = cluster::diana(d1), color = 'Fe_o_to_Fe_d', width = 0.3, scaling.factor = 0.008, name.style = 'center-center')

d2 <- compareSites(x, vars = c('MAP', 'MAAT'))

par(mar = c(0, 0, 0, 1))
plotProfileDendrogram(x, clust = cluster::diana(d2), color = 'Fe_o_to_Fe_d', width = 0.3, name.style = 'center-center')

w1 <- 2
w2 <- 1

## idea: following symbolic representation used in SEM ... I think
d <- (w1*d1 + w2*d2) / sum(c(w1, w2))

par(mar = c(0, 0, 0, 1))
plotProfileDendrogram(x, clust = cluster::diana(d), color = 'Fe_o_to_Fe_d', width = 0.3, name.style = 'center-center')
