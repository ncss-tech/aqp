library(aqp)
library(soilDB)
library(data.table)
library(lattice)
library(tactile)
library(hexbin)
library(viridisLite)

## TODO:
# * what is the baseline? 
# * compressed vs. raw
# * compressed values vs. compressed (mean or constant level factors)
# * Shannon H and differential entropy

a <- rnorm(n = 100)
b <- rep(mean(a), times = length(a))
a <- format(a, digits = 4)
b <- format(b, digits = 4)

length(memCompress(a, type = 'gzip')) / length(memCompress(b, type = 'gzip'))






s <- c('holland', 'sierra', 'musick', 'hanford', 'grangeville', 'delhi', 'amador', 'cecil', 'leon', 'lucy', 'clarksville', 'zook', 'clear lake', 'yolo', 'calhi', 'corralitos', 'sacramento', 'dodgeland')
x <- fetchOSD(s)

vars <- c('hue', 'value', 'chroma', 'texture_class', 'cf_class', 'pH', 'pH_class', 'distinctness', 'topography')

x$pi <- profileInformationIndex(x, vars = vars, baseline = FALSE, method = 'median')

par(mar = c(3, 0, 1, 1))
plotSPC(x, width = 0.3, name.style = 'center-center', plot.order = order(x$pi), cex.names = 0.75, shrink = TRUE)
axis(side = 1, at = 1:length(x), labels = format(x$pi, digits = 3)[order(x$pi)], cex.axis = 0.66, las = 1)
title('baseline = FALSE, method = median')



x$pi <- profileInformationIndex(x, vars = vars, baseline = TRUE, method = 'median')

par(mar = c(3, 0, 1, 1))
plotSPC(x, width = 0.3, name.style = 'center-center', plot.order = order(x$pi), cex.names = 0.75, shrink = TRUE)
axis(side = 1, at = 1:length(x), labels = format(x$pi, digits = 3)[order(x$pi)], cex.axis = 0.66, las = 1)
title('baseline = TRUE, method = median')


x$pi <- profileInformationIndex(x, vars = vars, baseline = FALSE, method = 'sum')

par(mar = c(3, 0, 1, 1))
plotSPC(x, width = 0.3, name.style = 'center-center', plot.order = order(x$pi), cex.names = 0.75, shrink = TRUE)
axis(side = 1, at = 1:length(x), labels = format(x$pi, digits = 3)[order(x$pi)], cex.axis = 0.66, las = 1)
title('baseline = FALSE, method = sum')


x$pi <- profileInformationIndex(x, vars = vars, baseline = TRUE, method = 'sum')

par(mar = c(3, 0, 1, 1))
plotSPC(x, width = 0.3, name.style = 'center-center', plot.order = order(x$pi), cex.names = 0.75, shrink = TRUE)
axis(side = 1, at = 1:length(x), labels = format(x$pi, digits = 3)[order(x$pi)], cex.axis = 0.66, las = 1)
title('baseline = TRUE, method = sum')


z <- data.frame(
  baseline.sum = profileInformationIndex(x, vars = vars, baseline = TRUE, method = 'sum'),
  sum = profileInformationIndex(x, vars = vars, baseline = FALSE, method = 'sum'),
  baseline.median = profileInformationIndex(x, vars = vars, baseline = TRUE, method = 'median'),
  median = profileInformationIndex(x, vars = vars, baseline = FALSE, method = 'median')
)

cor(z)
splom(z, par.settings = tactile.theme())


sc <- data.table::fread('https://github.com/ncss-tech/SoilWeb-data/raw/main/files/SC-database.csv.gz')
sc <- as.data.frame(sc)

sc.sub <- subset(sc, subset = taxgrtgroup %in% c('haploxeralfs', 'haploxerepts', 'palexeralfs', 'xerorthents', 'haploxererts'))

table(sc.sub$taxgrtgroup)

s <- sc.sub$soilseriesname
s <- split(s, makeChunks(s, size = 20))

x <- lapply(
  s,
  FUN = function(i) {
    fetchOSD(i)
  })

x <- combine(x)

vars <- c('hzname', 'hue', 'value', 'chroma', 'texture_class', 'cf_class', 'pH_class', 'distinctness', 'topography')


z <- data.frame(
  baseline.sum = profileInformationIndex(x, vars = vars, baseline = TRUE, method = 'sum'),
  baseline.median = profileInformationIndex(x, vars = vars, baseline = TRUE, method = 'median'),
  sum = profileInformationIndex(x, vars = vars, baseline = FALSE, method = 'sum'),
  median = profileInformationIndex(x, vars = vars, baseline = FALSE, method = 'median')
)

cor(z)

hexplom(z, par.settings = tactile.theme(axis.text = list(cex = 0.66)), trans = log, inv = exp, xbins = 30, colramp = mako, colorkey = FALSE, varname.cex = 0.75, varname.font = 2)



x$pi <- profileInformationIndex(x, vars = vars)
x$nhz <- profileApply(x, FUN = nrow, simplify = TRUE)

x$greatgroup <- factor(x$greatgroup, levels = c('palexeralfs', 'haploxeralfs', 'haploxerepts', 'xerorthents', 'haploxererts'))


hist(x$pi)

par(mar = c(0, 0, 0, 0))
plotSPC(x[x$pi > 0.8, ], width = 0.3, name.style = 'center-center', cex.names = 0.75, shrink = TRUE)




bwplot(greatgroup ~ pi, data = site(x), par.settings = tactile.theme(axis.text = list(cex = 1)), varwidth = TRUE, notch = TRUE, xlab = 'Profile Information Index')

bwplot(greatgroup ~ nhz, data = site(x), par.settings = tactile.theme(), varwidth = TRUE, notch = TRUE, xlab = 'Number of Horizons')

bwplot(pi ~ factor(nhz) | greatgroup, data = site(x), par.settings = tactile.theme(), ylab = 'Profile Information Index', xlab = 'Number of Horizons')


hexbinplot(pi ~ nhz | greatgroup, data = site(x), par.settings = tactile.theme(), ylab = 'Profile Information Index', xlab = 'Number of Horizons', trans = log, inv = exp, xbins = 10, colramp = viridis, colorkey = FALSE)



cor(x$nhz, x$pi)



library(ggdist)
library(ggplot2)

ggplot(site(x), aes(x = pi, y = greatgroup)) +
  stat_interval(inherit.aes = TRUE, orientation = 'horizontal', size = 6) + 
  theme_minimal() +
  theme(legend.position = c(1, 1), legend.justification ='right', legend.direction	
        = 'horizontal', legend.background = element_rect(fill = 'white', color = NA), axis.text.y = element_text(face = 'bold')) + 
  stat_summary(geom = 'point', fun = median, shape = 21, fill = 'black', col = 'white', cex = 3) +
  scale_color_brewer() + 
  scale_x_continuous(n.breaks = 16, limits = c(0, 1)) +
  xlab('Profile Information Index') + ylab('') +
  labs(title = 'Profile Information Index for Select Greatgroup Taxa', color = 'Interval')


##### 



z1 <- lapply(letters[1:10], random_profile, n = 5, exact = TRUE, n_prop = 1, SPC = TRUE, method = 'LPP', lpp.a=5, lpp.b=10, lpp.d=5, lpp.e=5, lpp.u=25)
z1 <- combine(z1)

z2 <- lapply(letters[11:20], random_profile, n = 5, exact = TRUE, n_prop = 1, SPC = TRUE, method = 'LPP', lpp.a=5, lpp.b=2, lpp.d=1, lpp.e=1, lpp.u=25)
z2 <- combine(z2)

site(z1)$g <- 'high'
site(z2)$g <- 'low'

z <- combine(z1, z2)

z$pi <- profileApply(z, FUN = profileInformationIndex, simplify = TRUE, vars = c('p1'), method = 'sum', baseline = FALSE)
z$nhz <- profileApply(z, FUN = nrow, simplify = TRUE)

z$pi
z$nhz


par(mar = c(0, 0, 3, 0))
groupedProfilePlot(z, groups = 'g', color = 'p1')


bwplot(g ~ pi, data = site(z))



z1 <- lapply(1:50, random_profile, n = 5, exact = TRUE, n_prop = 1, SPC = TRUE, method = 'LPP', lpp.a=5, lpp.b=10, lpp.d=5, lpp.e=5, lpp.u=25, min_thick = 2, max_thick = 50)
z1 <- combine(z1)

z1$pi <- profileApply(z1, FUN = profileInformationIndex, simplify = TRUE, vars = c('p1'), method = 'median', baseline = TRUE)

par(mar = c(3, 0, 0, 0))
plotSPC(z1, color = 'p1', plot.order = order(z1$pi), print.id = FALSE, width = 0.35, divide.hz = FALSE)
axis(side = 1, at = 1:length(z1), labels = format(z1$pi[order(z1$pi)], digits = 3), cex.axis = 0.66)


# simulate three profiles of increasing complexity
p1 <- data.frame(id = 1, top = 0, bottom = 100, p = 5)
p2 <- data.frame(id = 2, top = c(0, 10, 20, 30, 40, 50), bottom = c(10, 20, 30, 40, 50, 100), p = rep(5, times = 6))
p3 <- data.frame(id = 3, top = c(0, 10, 20, 30, 40, 50), bottom = c(10, 20, 30, 40, 50, 100), p = c(1, 5, 10, 3, 6, 2))

# combine and upgrade to SPC
z <- rbind(p1, p2, p3)
depths(z) <- id ~ top + bottom

# visual check
plotSPC(z, color = 'p')

# compute information index several ways
profileInformationIndex(z, vars = c('p'), method = 'sum')
profileInformationIndex(z, vars = c('p'), method = 'mean')

profileInformationIndex(z, vars = c('p'), method = 'mean', baseline = FALSE)
profileInformationIndex(z, vars = c('p'), method = 'sum', baseline = FALSE)

