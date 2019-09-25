library(aqp)
library(soilDB)
library(sharpshootR)
library(colorspace)

# need this after sourcing old version of function
library(plyr)
library(cluster)

# rename latest version for clarity
aggregateColor.new <- aggregateColor

# old version
source('https://raw.githubusercontent.com/ncss-tech/aqp/4b9fc5d215fe0281b613f1cb41c027ac2183ce79/R/aggregateColor.R')
aggregateColor.old <- get(x='aggregateColor', envir = .GlobalEnv)

# get lab / morphologic data
# simplify colors
x <- fetchKSSL(series='clarksville', returnMorphologicData = TRUE, simplifyColors = TRUE)

# extract pedons into SoilProfileCollection
s <- x$SPC

# genhz
s$genhz <- generalize.hz(s$hzn_desgn, c('A', 'E', 'Bt', '2Bt', '3Bt'), pat=c('A', 'E', '^Bt', '2B', '3B'), non.matching.code = NA)

# standard approach using all colors
# new function
a <- aggregateColor.new(s, "genhz", col = 'moist_soil_color')

system.time(a.reduced.old <- aggregateColor.old(s, "genhz", col = 'moist_soil_color', k=6))
system.time(a.reduced <- aggregateColor.new(s, "genhz", col = 'moist_soil_color', k=6))

par(mar = c(4.5, 2.5, 4.5, 0), mfrow=c(3, 1))
aggregateColorPlot(a, label.cex = 0.65, main = "no clustering", print.n.hz = TRUE)

aggregateColorPlot(a.reduced.old, label.cex = 0.65, main = "pam(LAB, stand=TRUE)", print.n.hz = TRUE)

aggregateColorPlot(a.reduced, label.cex = 0.65, main = "pam(dE00)", print.n.hz = TRUE)


swatchplot(list(
  `Top 6, no clustering`=a$scaled.data$A$moist_soil_color[1:6],
  `pam(LAB, stand=TRUE)`=a.reduced.old$scaled.data$A$moist_soil_color,
  `pam(dE00)`=a.reduced$scaled.data$A$moist_soil_color
))

swatchplot(list(
  `Top 6, no clustering`=a$scaled.data$Bt$moist_soil_color[1:6],
  `pam(LAB, stand=TRUE)`=a.reduced.old$scaled.data$Bt$moist_soil_color,
  `pam(dE00)`=a.reduced$scaled.data$Bt$moist_soil_color
))

swatchplot(list(
  `Top 6, no clustering`=a$scaled.data$`2Bt`$moist_soil_color[1:6],
  `pam(LAB, stand=TRUE)`=a.reduced.old$scaled.data$`2Bt`$moist_soil_color,
  `pam(dE00)`=a.reduced$scaled.data$`2Bt`$moist_soil_color
))



cols <- t(col2rgb(c('#6F5F4C', '#725E47'))) / 255
rgb2munsell(cols)
