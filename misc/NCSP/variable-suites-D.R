devtools::load_all()

library(soilDB)
library(sharpshootR)
library(cluster)
library(ape)
library(dendextend)
library(bench)


# start fresh
data(sp4)
depths(sp4) <- id ~ top + bottom
hzdesgnname(sp4) <- 'name'

# site(sp4)$group <- factor(rep(c('A', 'B'), each = 5), ordered = TRUE)
site(sp4)$group <- factor(rep(c('A', 'B'), each = 5), ordered = FALSE)

d <- NCSP(sp4, vars = c('Ca', 'CEC_7'), k = 0, maxDepth = 40, rescaleResult = TRUE)
d.s <- compareSites(sp4, vars = 'group')

str(d)
str(d.s)

(d + d.s) / 2




