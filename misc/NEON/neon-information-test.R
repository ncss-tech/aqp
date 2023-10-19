library(aqp)
library(lattice)
library(tactile)
library(hexbin)

#### profileInformationIndex, NEON Data  ####


soildata.url <- "https://github.com/swsalley/NEONsoil/blob/master/DistributedAnalysis/SSSA-J_Variability/NEON_Lab_clean_20230127.csv?raw=true"

x <- read.csv(soildata.url)

depths(x) <- plotID ~ horizonTopDepth + horizonBottomDepth  

hzdesgnname(x) <- 'horizonName'

sdc <- getSoilDepthClass(x)

site(x) <- sdc

table(x$depth.class)
hist(x$depth)


i.sum.t <- profileInformationIndex(x, vars = c('clayTotal'), method = 'sum', baseline = TRUE)
i.mean.t <- profileInformationIndex(x, vars = c('clayTotal'), method = 'mean', baseline = TRUE)
i.sum.f <- profileInformationIndex(x, vars = c('clayTotal'), method = 'sum', baseline = FALSE)
i.mean.f <- profileInformationIndex(x, vars = c('clayTotal'), method = 'mean', baseline = FALSE)

neon.info <- data.frame(
  plotID = site(x)[[idname(x)]],
  depth = x$depth,
  depth.class = x$depth.class,
  i.sum.t, 
  i.mean.t, 
  i.sum.f, 
  i.mean.f
)

neon.info$site <- substr(neon.info$plotID , start = 1, stop = 4)
rownames(neon.info) <- NULL
head(neon.info)

# write.csv(neon.info, "E:/Publications/NEON/R/Neon_info_index.csv")
# this file is at: https://github.com/swsalley/NEONsoil/blob/master/NCSP/Neon_info_index.csv

v <- c('depth', 'i.sum.t', 'i.mean.t', 'i.sum.f', 'i.mean.f')
round(cor(neon.info[, v]), 2)

nm <- names(sort(tapply(neon.info$i.mean.t, neon.info$site, median)))
neon.info$g <- factor(neon.info$site, levels = nm)

bwplot(g ~ i.mean.t, data = neon.info, par.settings = tactile.theme(), panel = function(...) {
  panel.grid(-1, -1)
  panel.bwplot(...)
})

bwplot(g ~ i.mean.f, data = neon.info, par.settings = tactile.theme(), panel = function(...) {
  panel.grid(-1, -1)
  panel.bwplot(...)
})


bwplot(depth.class ~ i.sum.t, data = neon.info, par.settings = tactile.theme(), panel = function(...) {
  panel.grid(-1, -1)
  panel.bwplot(...)
})

bwplot(depth.class ~ i.sum.f, data = neon.info, par.settings = tactile.theme(), panel = function(...) {
  panel.grid(-1, -1)
  panel.bwplot(...)
})



hsd <- TukeyHSD(aov(i.sum.f ~ depth.class, data = neon.info))

par(mar = c(4, 8, 3, 1))
plot(hsd, cex.axis = 0.66, las = 1)

hexplom(neon.info[, v], par.settings = tactile.theme(), colramp = viridisLite::viridis, nbins = 30)


hexplom(neon.info[, c('depth', 'i.sum.t', 'i.sum.f')], par.settings = tactile.theme(), colramp = viridisLite::viridis, nbins = 30)



