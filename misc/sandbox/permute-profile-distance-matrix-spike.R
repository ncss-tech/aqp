library(aqp)
library(cluster)
library(sharpshootR)

data(sp4)
depths(sp4) <- id ~ top + bottom

# profile to general realizations of
p.idx <- 1

# spike profile
spike.idx <- 6

# simulate some data
horizons(sp4)$bdy <- 4
p <- perturb(sp4[p.idx, ], n=10, boundary.attr = 'bdy', min.thickness = 2)

# remove from @site
site(p)$id <- NULL

# this is right
z.2 <- combine(list(p, sp4[c(p.idx, spike.idx), ]))

par(mar=c(0,0,3,0))
plotSPC(z.2, color = 'K')


## check out the version that worked  

dev.off()

d <- profile_compare(z.2, vars = c('K', 'sand', 'CEC_7'), max_d = 40, k = 0)
dd <- diana(d)

plotProfileDendrogram(z.2, dd, scaling.factor = 1.5, y.offset = 5, width = 0.3, color = 'K')




