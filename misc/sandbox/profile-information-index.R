
library(soilDB)
library(data.table)
library(lattice)
library(tactile)
library(hexbin)
library(purrr)


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



aqp:::.prepareVector(1:10, d = 4)
aqp:::.prepareVariable(1:10, numericDigits = 4, removeNA = FALSE)
aqp:::.prepareVariable(c('A', 'A', 'A', 'B', 'C'), numericDigits = 4, removeNA = FALSE)




aqp:::.compressedLength(1:10)
aqp:::.compressedLength(1)
aqp:::.compressedLength('AAA')
aqp:::.compressedLength(FALSE)

aqp:::.compressedLength(letters)
aqp:::.compressedLength('A')
aqp:::.compressedLength(factor('A'))


aqp:::.compressedLength(0)
aqp:::.compressedLength(1000)

aqp:::.compressedLength(c(1, 5, 10))
aqp:::.compressedLength(c(100, 500, 1000))

# single variable complexity vs. joint complexity
aqp:::.compressedLength(1:10) + aqp:::.compressedLength(20:30)
aqp:::.compressedLength(c(1:10, 20:30))



x <- c(1:5, 10:5)

d <- data.frame(
  source = aqp:::.compressedLength(x),
  rep.mean = aqp:::.compressedLength(rep(mean(x), times = length(x))),
  runif = aqp:::.compressedLength(runif(n = length(x), min = 0, max = 10))
)

knitr::kable(
  d, 
  caption = 'Length of gzip compressed objects (bytes).', 
  col.names = c('x: c(1:5, 10:5)', 'rep(mean(x, length(x)))', 'runif(length(x), min = 0, max = 10)')
)










### simple cases

# single horizon, constant value
p1 <- data.frame(id = 1, top = 0, bottom = 100, p = 5, name = 'H')

# multiple horizons, constant value
p2 <- data.frame(
  id = 2, top = c(0, 10, 20, 30, 40, 50),
  bottom = c(10, 20, 30, 40, 50, 100),
  p = rep(5, times = 6),
  name = c('A1', 'A2', 'Bw', 'Bt1', 'Bt2', 'C')
)

# multiple horizons, random values
p3 <- data.frame(
  id = 3, top = c(0, 10, 20, 30, 40, 50),
  bottom = c(10, 20, 30, 40, 50, 100),
  p = c(1, 5, 10, 35, 6, 2),
  name = c('A1', 'A2', 'Bw', 'Bt1', 'Bt2', 'C')
)

# multiple horizons, mostly NA
p4 <- data.frame(
  id = 4, top = c(0, 10, 20, 30, 40, 50),
  bottom = c(10, 20, 30, 40, 50, 100),
  p = c(1, NA, NA, NA, NA, NA),
  name = c('A1', 'A2', 'Bw', 'Bt1', 'Bt2', 'C')
)

# shallower version of p1
p5 <- data.frame(id = 5, top = 0, bottom = 50, p = 5, name = 'H')

# combine and upgrade to SPC
z <- rbind(p1, p2, p3, p4, p5)
depths(z) <- id ~ top + bottom
hzdesgnname(z) <- 'name'

# z <- fillHzGaps(z)

# visual check
par(mar = c(1, 0, 3, 3))
plotSPC(z, color = 'p', name.style = 'center-center', cex.names = 0.8, max.depth = 110)

# factor version of horizon name
z$fname <- factor(z$name)

vars <- c('p', 'name')
# result is total bytes
pi <- profileInformationIndex(z, vars = vars, baseline = FALSE, padNA = FALSE, removeNA = TRUE)

text(x = 1:5, y = 105, labels = pi, cex = 0.85)
mtext('Profile Information Index (bytes)', side = 1, line = -1)


# effect of aggregation function
profileInformationIndex(z, vars = vars, method = 'i', baseline = TRUE)
profileInformationIndex(z, vars = vars, method = 'j', baseline = TRUE)

# effect of baseline
profileInformationIndex(z, vars = vars, method = 'j', baseline = TRUE)
profileInformationIndex(z, vars = vars, method = 'j', baseline = FALSE)

# effect of removing NA
profileInformationIndex(z, vars = vars, method = 'j', baseline = FALSE, removeNA = TRUE, padNA = FALSE)
profileInformationIndex(z, vars = vars, method = 'j', baseline = FALSE, removeNA = FALSE, padNA = FALSE)


profileInformationIndex(z, vars = vars, method = 'j', baseline = TRUE, padNA = TRUE, removeNA = FALSE)
profileInformationIndex(z, vars = vars, method = 'j', baseline = TRUE, padNA = FALSE, removeNA = FALSE)

profileInformationIndex(z, vars = vars, method = 'j', baseline = TRUE, numericDigits = 1)
profileInformationIndex(z, vars = vars, method = 'j', baseline = TRUE, numericDigits = 10)





## for later, distance matrices

d <- NCSP(z, vars = c('p', 'fname'), maxDepth = 100)
d <- as.matrix(d)
d

aqp:::.compressedLength(d)










## truncate





# s <- c('holland', 'sierra', 'musick', 'hanford', 'grangeville', 'delhi', 'amador', 'cecil', 'leon', 'lucy', 'clarksville', 'zook', 'clear lake', 'yolo', 'calhi', 'corralitos', 'sacramento', 'dodgeland')
# x <- fetchOSD(s)
# 
# x <- trunc(x, 0, 100)
# 
# vars <- c('hue', 'value', 'chroma', 'texture_class', 'cf_class', 'pH', 'pH_class', 'distinctness', 'topography')
# 
# x$pi <- profileInformationIndex(x, vars = vars, baseline = FALSE, method = 'median')
# 
# par(mar = c(3, 0, 1, 2))
# plotSPC(x, width = 0.3, name.style = 'center-center', plot.order = order(x$pi), cex.names = 0.66, shrink = TRUE)
# axis(side = 1, at = 1:length(x), labels = format(x$pi, digits = 3)[order(x$pi)], cex.axis = 0.75, las = 1)
# title('baseline = FALSE, method = median')
# 






s <- c('holland', 'sierra', 'musick', 'hanford', 'grangeville', 'delhi', 'amador', 'cecil', 'leon', 'lucy', 'clarksville', 'zook', 'clear lake', 'yolo', 'calhi', 'corralitos', 'sacramento', 'dodgeland')
x <- fetchOSD(s)

# vars <- c('hue', 'value', 'chroma', 'texture_class', 'cf_class', 'pH', 'pH_class', 'distinctness', 'topography')

par(mar = c(3, 0, 1, 2), mfrow = c(2, 1))

vars <- c('hue', 'value', 'chroma', 'hzname')

x$pi <- profileInformationIndex(x, vars = vars, method = 'j', baseline = TRUE, padNA = TRUE, removeNA = FALSE)

plotSPC(x, width = 0.3, name.style = 'center-center', plot.order = order(x$pi), cex.names = 0.66, shrink = TRUE, max.depth = 200)
axis(side = 1, at = 1:length(x), labels = format(x$pi, digits = 3)[order(x$pi)], cex.axis = 0.75, las = 1)
title('baseline = TRUE, removeNA = FALSE, padNA = TRUE')


x$pi <- profileInformationIndex(x, vars = vars, method = 'j', baseline = TRUE, padNA = FALSE, removeNA = TRUE)

plotSPC(x, width = 0.3, name.style = 'center-center', plot.order = order(x$pi), cex.names = 0.66, shrink = TRUE, max.depth = 200)
axis(side = 1, at = 1:length(x), labels = format(x$pi, digits = 3)[order(x$pi)], cex.axis = 0.75, las = 1)
title('baseline = TRUE, removeNA = TRUE, padNA = FALSE')



x$pi <- profileInformationIndex(x, vars = vars, method = 'j', baseline = FALSE, padNA = FALSE, removeNA = TRUE)

plotSPC(x, width = 0.3, name.style = 'center-center', plot.order = order(x$pi), cex.names = 0.66, shrink = TRUE, max.depth = 200)
axis(side = 1, at = 1:length(x), labels = format(x$pi, digits = 3)[order(x$pi)], cex.axis = 0.75, las = 1)
title('baseline = FALSE, removeNA = FALSE, padNA = FALSE')





pdf(file = 'e:/temp/profileInformationIndex-test.pdf', width = 11, height = 8.5, pointsize = 10)

par(mar = c(3, 0, 1, 2), mfrow = c(2, 1))

vars <- c('hue', 'value', 'chroma', 'hzname')

x$pi <- profileInformationIndex(x, vars = vars, method = 'j', baseline = FALSE, padNA = FALSE)

plotSPC(x, width = 0.3, name.style = 'center-center', plot.order = order(x$pi), cex.names = 0.66, shrink = TRUE, max.depth = 200)
axis(side = 1, at = 1:length(x), labels = format(x$pi, digits = 3)[order(x$pi)], cex.axis = 0.75, las = 1)
title('method = j, baseline = FALSE, padNA = FALSE', cex.main = 1)

x$pi <- profileInformationIndex(x, vars = vars, method = 'i', baseline = FALSE, padNA = FALSE)

plotSPC(x, width = 0.3, name.style = 'center-center', plot.order = order(x$pi), cex.names = 0.66, shrink = TRUE, max.depth = 200)
axis(side = 1, at = 1:length(x), labels = format(x$pi, digits = 3)[order(x$pi)], cex.axis = 0.75, las = 1)
title('method = i, baseline = FALSE, padNA = FALSE', cex.main = 1)



x$pi <- profileInformationIndex(x, vars = vars, method = 'j', baseline = FALSE, padNA = TRUE)

plotSPC(x, width = 0.3, name.style = 'center-center', plot.order = order(x$pi), cex.names = 0.66, shrink = TRUE, max.depth = 200)
axis(side = 1, at = 1:length(x), labels = format(x$pi, digits = 3)[order(x$pi)], cex.axis = 0.75, las = 1)
title('method = j, baseline = FALSE, padNA = TRUE', cex.main = 1)

x$pi <- profileInformationIndex(x, vars = vars, method = 'i', baseline = FALSE, padNA = TRUE)

plotSPC(x, width = 0.3, name.style = 'center-center', plot.order = order(x$pi), cex.names = 0.66, shrink = TRUE, max.depth = 200)
axis(side = 1, at = 1:length(x), labels = format(x$pi, digits = 3)[order(x$pi)], cex.axis = 0.75, las = 1)
title('method = i, baseline = FALSE, padNA = TRUE', cex.main = 1)

dev.off()






x$pi <- profileInformationIndex(x, vars = vars, baseline = TRUE, method = 'median')

par(mar = c(3, 0, 1, 2))
plotSPC(x, width = 0.3, name.style = 'center-center', plot.order = order(x$pi), cex.names = 0.66, shrink = TRUE)
axis(side = 1, at = 1:length(x), labels = format(x$pi, digits = 3)[order(x$pi)], cex.axis = 0.75, las = 1)
title('baseline = TRUE, method = median')


x$pi <- profileInformationIndex(x, vars = vars, baseline = FALSE, method = 'sum')

par(mar = c(3, 0, 1, 2))
plotSPC(x, width = 0.3, name.style = 'center-center', plot.order = order(x$pi), cex.names = 0.66, shrink = TRUE)
axis(side = 1, at = 1:length(x), labels = format(x$pi, digits = 3)[order(x$pi)], cex.axis = 0.75, las = 1)
title('baseline = FALSE, method = sum')


x$pi <- profileInformationIndex(x, vars = vars, baseline = TRUE, method = 'sum')

par(mar = c(3, 0, 1, 2))
plotSPC(x, width = 0.3, name.style = 'center-center', plot.order = order(x$pi), cex.names = 0.66, shrink = TRUE)
axis(side = 1, at = 1:length(x), labels = format(x$pi, digits = 3)[order(x$pi)], cex.axis = 0.75, las = 1)
title('baseline = TRUE, method = sum')


x$pi <- profileInformationIndex(x, vars = 'hue', baseline = TRUE, method = 'sum')

par(mar = c(3, 0, 1, 2))
plotSPC(x, width = 0.3, name.style = 'center-center', plot.order = order(x$pi), cex.names = 0.66, shrink = TRUE)
axis(side = 1, at = 1:length(x), labels = format(x$pi, digits = 3)[order(x$pi)], cex.axis = 0.75, las = 1)
title('baseline = TRUE, method = sum')




.lab <- convertColor(t(col2rgb(x$soil_color)) / 255, from = 'sRGB', to = 'Lab', from.ref.white = 'D65', to.ref.white = 'D65')


x$L <- .lab[, 1]
x$A <- .lab[, 2]
x$B <- .lab[, 3]


x$pi <- profileInformationIndex(x, vars = c('L', 'A', 'B'), baseline = FALSE, method = 'j', padNA = FALSE, removeNA = FALSE)

par(mar = c(3, 0, 1, 2), mfrow = c(1,1))
plotSPC(x, width = 0.3, name.style = 'center-center', plot.order = order(x$pi), cex.names = 0.66, shrink = TRUE)
axis(side = 1, at = 1:length(x), labels = format(x$pi, digits = 3)[order(x$pi)], cex.axis = 0.75, las = 1)
title('baseline = TRUE, method = sum')



vars <- c('hue', 'value', 'chroma', 'texture_class', 'cf_class', 'pH', 'pH_class', 'distinctness', 'topography')

z <- data.frame(
  baseline.j = profileInformationIndex(x, vars = vars, baseline = TRUE, method = 'j'),
  j = profileInformationIndex(x, vars = vars, baseline = FALSE, method = 'j'),
  baseline.i = profileInformationIndex(x, vars = vars, baseline = TRUE, method = 'i'),
  median.i = profileInformationIndex(x, vars = vars, baseline = FALSE, method = 'i')
)

cor(z)
splom(z, par.settings = tactile.theme())


sc <- data.table::fread('https://github.com/ncss-tech/SoilWeb-data/raw/main/files/SC-database.csv.gz')
sc <- as.data.frame(sc)

sc.sub <- subset(sc, subset = taxgrtgroup %in% c('haploxeralfs', 'haploxerepts', 'palexeralfs', 'xerorthents', 'haploxererts', 'endoaquolls'))

table(sc.sub$taxgrtgroup)

s <- sc.sub$soilseriesname
s <- split(s, makeChunks(s, size = 20))

x <- map(
  s,
  .progress = TRUE,
  .f = function(i) {
    fetchOSD(i)
  })

x <- combine(x)

vars <- c('hzname', 'hue', 'value', 'chroma', 'texture_class')


z <- data.frame(
  baseline.joint = profileInformationIndex(x, vars = vars, baseline = TRUE, method = 'j', padNA = FALSE, removeNA = TRUE),
  baseline.individual = profileInformationIndex(x, vars = vars, baseline = TRUE, method = 'i', padNA = FALSE, removeNA = TRUE),
  joint = profileInformationIndex(x, vars = vars, baseline = FALSE, method = 'j', padNA = FALSE, removeNA = TRUE),
  individual = profileInformationIndex(x, vars = vars, baseline = FALSE, method = 'i', padNA = FALSE, removeNA = TRUE)
)

cor(z)

.cols <- hcl.colors(n = 100, palette = 'zissou 1')
.cp <- colorRampPalette(.cols)

hexplom(z, par.settings = tactile.theme(axis.text = list(cex = 0.66)), trans = log, inv = exp, xbins = 30, colramp = .cp, colorkey = FALSE, varname.cex = 0.75, varname.font = 2, main = 'Profile Information Index', xlab = '')


plot(joint ~ individual, data = z, las = 1)
plot(baseline.joint ~ baseline.individual, data = z, las = 1)
hexbinplot(baseline.joint ~ joint, data = z, par.settings = tactile.theme(), trans = log, inv = exp, xbins = 30, colramp = .cp, colorkey = FALSE, varname.cex = 0.75, varname.font = 2, main = 'Profile Information Index')

x$pi <- profileInformationIndex(x, vars = vars, method = 'j', baseline = FALSE, padNA = FALSE, removeNA = TRUE)
x$nhz <- profileApply(x, FUN = nrow, simplify = TRUE)

x$greatgroup <- factor(x$greatgroup, levels = c('palexeralfs', 'haploxeralfs', 'haploxerepts', 'xerorthents', 'haploxererts', 'endoaquolls'))


hist(x$pi, las = 1, breaks = 20, xlab = 'Profile Information Index (baseline sum)', main = '')

# .crit <- mean(x$pi) + (c(-2, 2) * sd(x$pi))
.crit <- quantile(x$pi, probs = c(0.01, 0.99))

par(mar = c(0, 0, 0, 2))
plotSPC(x[x$pi > .crit[2], ], width = 0.3, name.style = 'center-center', cex.names = 0.66, shrink = TRUE, max.depth = 250)

plotSPC(x[x$pi < .crit[1], ], width = 0.3, name.style = 'center-center', cex.names = 0.66, shrink = TRUE, max.depth = 250)





bwplot(greatgroup ~ pi, data = site(x), par.settings = tactile.theme(axis.text = list(cex = 1)), varwidth = TRUE, notch = TRUE, xlab = 'Profile Information Index')

bwplot(greatgroup ~ nhz, data = site(x), par.settings = tactile.theme(), varwidth = TRUE, notch = TRUE, xlab = 'Number of Horizons')

bwplot(pi ~ factor(nhz) | greatgroup, data = site(x), par.settings = tactile.theme(), ylab = 'Profile Information Index', xlab = 'Number of Horizons')


hexbinplot(pi ~ nhz | greatgroup, data = site(x), par.settings = tactile.theme(), ylab = 'Profile Information Index', xlab = 'Number of Horizons', trans = log, inv = exp, xbins = 10, colramp = .cp, colorkey = FALSE)



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
  scale_x_continuous(n.breaks = 16) +
  xlab('Profile Information Index') + ylab('') +
  labs(title = 'Profile Information Index for Select Greatgroup Taxa', color = 'Interval')


##### 



z1 <- lapply(letters[1:10], random_profile, n = 5, exact = TRUE, n_prop = 3, SPC = TRUE, method = 'LPP')
z1 <- combine(z1)

z2 <- lapply(letters[11:20], random_profile, n = 5, exact = TRUE, n_prop = 3, SPC = TRUE)
z2 <- combine(z2)

site(z1)$g <- 'LPP'
site(z2)$g <- 'RW'

z <- combine(z1, z2)

z$pi <- profileInformationIndex(z, vars = c('p1', 'p2', 'p3'), method = 'j', baseline = FALSE, padNA = FALSE, removeNA = TRUE)
z$nhz <- profileApply(z, FUN = nrow, simplify = TRUE)

z$g <- factor(z$g)

z$pi
z$nhz

par(mar = c(0, 0, 3, 2))
groupedProfilePlot(z, groups = 'g', color = 'p1')


bwplot(g ~ pi, data = site(z))



z1 <- lapply(
  1:50, 
  random_profile, 
  n = 5, 
  exact = TRUE, 
  n_prop = 3, 
  SPC = TRUE, 
  method = 'LPP', 
  lpp.a = 5, 
  lpp.b = 10, 
  lpp.d = 5, 
  lpp.e = 5, 
  lpp.u = 25, 
  min_thick = 2, 
  max_thick = 50
)

z1 <- combine(z1)
# z1 <- trunc(z1, 0, min(z1))

z1$pi <- profileInformationIndex(z1, vars = c('p1', 'p2', 'p3'), method = 'i', baseline = FALSE, padNA = FALSE, removeNA = TRUE)

par(mar = c(3, 0, 0, 2))
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



## ... need to resolve this
# effect of profile depth
p1 <- data.frame(id = 1, top = 0, bottom = 75, p = 5)
p2 <- data.frame(id = 2, top = c(0, 10, 20, 30, 40, 50), bottom = c(10, 20, 30, 40, 50, 100), p = rep(5, times = 6))
p3 <- data.frame(id = 3, top = c(0, 10, 20, 30, 40, 50), bottom = c(10, 20, 30, 40, 50, 150), p = c(1, 5, 10, 3, 6, 2))

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




