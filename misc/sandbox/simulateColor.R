
library(aqp)


p <- list(
  list(m = '2.5BG 6/6', thresh = 8, hues = c('2.5BG'))
)

s <- simulateColor(method = 'dE00', n = 100, parameters = p)

pp <- colorChart(s[[1]], annotate = TRUE)

update(pp, asp = 0.5)




data(loafercreek, package = 'soilDB')

# generalize horizon names using REGEX rules
n <- c('Oi', 'A', 'BA','Bt1','Bt2','Bt3','Cr','R')
p <- c('O', '^A$|Ad|Ap|AB','BA$|Bw',
       'Bt1$|^B$','^Bt$|^Bt2$','^Bt3|^Bt4|CBt$|BCt$|2Bt|2CB$|^C$','Cr','R')
loafercreek$genhz <- generalize.hz(loafercreek$hzname, n, p)

# remove non-matching generalized horizon names
loafercreek$genhz[loafercreek$genhz == 'not-used'] <- NA
loafercreek$genhz <- factor(loafercreek$genhz)


cols <- data.frame(
  m = sprintf('%s %s/%s', loafercreek$m_hue, loafercreek$m_value, loafercreek$m_chroma),
  g = loafercreek$genhz
)

colorChart(cols$m, annotate = TRUE)

colorChart(cols$m, g = cols$g, annotate = TRUE)

colorChart(cols$m, g = cols$g)

a <- aggregateColor(loafercreek, 'genhz', k = 8)



## aggregateColor proportions

n.sim <- 100

# using output from aggregateColor()
(cols <- simulateColor(method = 'proportions', n = n.sim, parameters = a))

d <- data.frame(
  m = unlist(cols),
  g = rep(names(cols), each = length(cols[[1]]))
)

d$g <- factor(d$g, levels = names(a$scaled.data))

colorChart(d$m, g = d$g, chip.cex = 3, annotate = TRUE)



previewColors(parseMunsell(d$m), method = 'MDS')


# seed profile
s <- loafercreek[7, ]

# static hz variability
horizons(s)$.hd <- 6

# simulate
n.sim <- 15
ids <- sprintf("%s-%03d", 'sim', 1:n.sim)
z <- perturb(s, id = ids, boundary.attr = '.hd', min.thickness = 4)

# modify new SPC with simulated colors
# hard-coded to use 'soil_color' horizon level attribute
z <- simulateColor(method = 'proportions', n = n.sim, parameters = a, SPC = z)

# include original profile
zz <- combine(z, s)

# cool
par(mar = c(0, 0, 1, 0))
plotSPC(zz, name.style = 'center-center', hz.depths = TRUE, plot.depth.axis = FALSE, width = 0.3)
title('aggregateColor based simulation')



## dE00 approach


# self-calibration
contrastChart(m = '7.5YR 3/3', hues = c('10YR', '7.5YR'), thresh = 10, gridLines = TRUE)
contrastChart(m = '7.5YR 3/3', hues = c('10YR', '7.5YR', '10G'), thresh = 20, gridLines = TRUE)

contrastChart(m = '7.5YR 4/4', hues = c('7.5YR'), thresh = 8)
contrastChart(m = '7.5YR 4/4', hues = c('5YR', '7.5YR'), thresh = 8)
contrastChart(m = '10YR 4/6', hues = c('10YR', '7.5YR'), thresh = 10)
contrastChart(m = '2.5G 6/2', hues = c('2.5G', '2.5GY', '2.5BG'), thresh = 15)


# using dE00 and hue constraints
p <- list(
  'A' = list(m = '7.5YR 3/3', thresh = 5, hues = c('7.5YR')),
  'BA' = list(m = '7.5YR 4/4', thresh = 8, hues = c('7.5YR')),
  'Bt1' = list(m = '7.5YR 4/4', thresh = 8, hues = c('5YR', '7.5YR')),
  'Bt2' = list(m = '5YR 4/5', thresh = 8, hues = c('5YR', '7.5YR')),
  'Bt3' = list(m = '10YR 4/6', thresh = 10, hues = c('10YR', '7.5YR')),
  'Cr' = list(m = '2.5G 6/2', thresh = 15, hues = c('2.5G', '2.5GY', '2.5BG'))
  )

# using dE00 threshold
(cols <- simulateColor(method = 'dE00', n = n.sim, parameters = p))
previewColors(parseMunsell(unlist(cols)), method = 'MDS')

# check L1 and marginal quantiles: looks OK
plotColorQuantiles(colorQuantiles(parseMunsell(cols$A)))
plotColorQuantiles(colorQuantiles(parseMunsell(cols$Bt1)))
plotColorQuantiles(colorQuantiles(parseMunsell(cols$Bt3)))
plotColorQuantiles(colorQuantiles(parseMunsell(cols$Cr)))


# seed profile
s <- loafercreek[7, ]

# static hz variability
horizons(s)$.hd <- 6

# simulate
ids <- sprintf("%s-%03d", 'sim', 1:n.sim)
z <- perturb(s, id = ids, boundary.attr = '.hd', min.thickness = 4)

# modify new SPC with simulated colors
# hard-coded to use 'soil_color' horizon level attribute
z <- simulateColor(method = 'dE00', n = n.sim, parameters = p, SPC = z)

# include original profile
zz <- combine(z, s)

# cool
par(mar = c(0, 0, 1, 0))
plotSPC(zz, name.style = 'center-center', hz.depths = TRUE, plot.depth.axis = FALSE, width = 0.3)
title('dE00 based simulation')


