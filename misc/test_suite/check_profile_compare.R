library(aqp)
library(sharpshootR)
library(cluster)


# this has been a problem for years, alpha-sorting changes ordering
set.seed(1010101)
s <- union(lapply(letters[1:10], random_profile, n_prop=3, method='LPP', SPC=TRUE))

pr <- princomp(horizons(s)[, c('p1', 'p2', 'p3')], cor = TRUE)
s$pc.1 <- pr$scores[, 1]


d <- profile_compare(s, vars=c('p1','p2', 'p3'), max_d=100, k=0, rescale.result=TRUE)
hc <- diana(d)

(plotProfileDendrogram(s, hc, color='pc.1', debug = TRUE, width=0.25))

