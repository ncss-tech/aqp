# need the github versions of these 
library(aqp)
# note that the latest aqp requires Gmedian and farver packages
library(soilDB)
library(sharpshootR)


# get data
x <- fetchKSSL('clarksville', returnMorphologicData = TRUE, simplifyColors = TRUE)

# preview colors
previewColors(x$SPC$moist_soil_color)
previewColors(x$SPC$moist_soil_color, method = 'MDS')

# extract pedons into SoilProfileCollection
pedons <- x$SPC

# genhz
pedons$genhz <- generalize.hz(pedons$hzn_desgn, new = c('O', 'A', 'E', 'Bt', '2Bt', '3Bt'), pat=c('O', 'A', 'E', '^Bt', '2B', '3B'), non.matching.code = NA)
pedons$genhz <- factor(pedons$genhz, levels = guessGenHzLevels(pedons, "genhz")$levels)

# aggregate color
a <- aggregateColor(pedons, "genhz", col = 'moist_soil_color')
a.8 <- aggregateColor(pedons, "genhz", col = 'moist_soil_color', k = 8)

par(mar = c(4.5, 2.5, 4.5, 0), mfrow=c(2,1))
aggregateColorPlot(a, label.cex = 0.65, main = "Clarksville Moist Colors\nGeneralized Horizons", print.n.hz = FALSE, print.label = FALSE, rect.border = NA, horizontal.borders = TRUE)

aggregateColorPlot(a.8, label.cex = 0.65, main = "Clarksville Moist Colors\nGeneralized Horizons\n8 Colors", print.n.hz = FALSE, print.label = FALSE, rect.border = NA, horizontal.borders = TRUE)

# marginal quantiles and L1 median of {L,A,B}
x <- colorQuantiles(na.omit(pedons$moist_soil_color[which(pedons$genhz == 'Bt')]))
plotColorQuantiles(x, title = 'Clarksville - Bt')


## RI as described in Barron and Torrent, 1986
lab.data <- as.data.frame(convertColor(cbind(pedons$m_r, pedons$m_g, pedons$m_b), from = 'sRGB', to = 'Lab', from.ref.white = 'D65', clip = FALSE))
pedons$RI <- with(lab.data, (a.x * sqrt((a.x^2 + b^2 )) * 10^10 ) / (b * L^6) )
pedons$ln_RI <- log(pedons$RI)

hist(pedons$ln_RI)

plot(sample(pedons, 25), color='ln_RI')








