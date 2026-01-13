library(aqp)
library(soilDB)


x <- fetchKSSL(series = 'clarksville', returnMorphologicData = TRUE, simplifyColors = TRUE)

# extract pedons into SoilProfileCollection
s <- x$SPC

# genhz
s$genhz <- generalize.hz(
  x = s$hzn_desgn, 
  new = c('A', 'E', 'Bt', '2Bt', '3Bt'), 
  pattern = c('A', 'E', '^Bt', '2B', '3B'),
  non.matching.code = NA
)

s$genhz <- factor(s$genhz, levels = guessGenHzLevels(s, "genhz")$levels)

ms <- sprintf("%s %s/%s", s$m_hue, s$m_value, s$m_chroma)

cols <- split(ms, s$genhz)

colorChart(ms, s$genhz)




# 
# # https://www.itl.nist.gov/div898/software/dataplot/refman2/ch2/weightsd.pdf
# .wsd <- function(x, w) {
# 
#   # remove NA
#   
#   .n_no_zero <- length(w[w > 0])
#   
#   .top <- sum(w * (x - mean(x))^2)
#   
#   .bottom <- ((.n_no_zero - 1) / .n_no_zero) * sum(w)
#   
#   .res <- sqrt(.top / .bottom)
#   
#   return(.res)
#   
# }




sapply(lapply(cols, colorVariation, method = 'frequency'), attr, 'most frequent')

rbind(
  `F` =          lapply(cols, colorVariation, method = 'frequency'),
  `C` =          lapply(cols, colorVariation, method = 'centroid'),
  `R 10YR 3/3` = lapply(cols, colorVariation, method = 'reference', ref = '10YR 3/3'),
  `R N 2/` =     lapply(cols, colorVariation, method = 'reference', ref = 'N 2/')
)




m <- c('10YR 3/3', '5YR 3/4', '7.5YR 4/6')
colorVariation(m)


m <- c('10YR 3/3', '10YR 4/4', '10YR 4/4', '10YR 4/4')
colorVariation(m)

m <- c('10YR 3/3', '10YR 4/4', '10YR 4/4', '10YR 4/6')
colorVariation(m)

m <- c('10YR 3/3', '10YR 4/4', '10YR 4/4', '5GY 6/8')
colorVariation(m)

m <- c('10YR 4/4', '10YR 4/4', '10YR 3/4')
colorVariation(m)


m <- c('10YR 4/4', '10YR 4/4')
colorVariation(m)



p <- list(
  'A' = list(m = '7.5YR 3/3', thresh = 5, hues = c('7.5YR')),
  'BA' = list(m = '7.5YR 4/4', thresh = 8, hues = c('7.5YR')),
  'Bt1' = list(m = '7.5YR 4/4', thresh = 8, hues = c('5YR', '7.5YR')),
  'Bt2' = list(m = '5YR 4/5', thresh = 8, hues = c('5YR', '7.5YR')),
  'Bt3' = list(m = '10YR 4/6', thresh = 10, hues = c('10YR', '7.5YR')),
  'Cr' = list(m = '2.5G 6/2', thresh = 15, hues = c('2.5G', '2.5GY', '2.5BG'))
)

# simulate
(cols <- simulateColor(method = 'dE00', n = 10, parameters = p))

lapply(cols, colorVariation, method = 'frequency')



p <- list(
  'A' = list(m = '7.5YR 3/3', thresh = 20, hues = c('10YR', '7.5YR', '5YR'))
)

# simulate
set.seed(54654)
cols <- simulateColor(method = 'dE00', n = 200, parameters = p)

lapply(cols, colorVariation, method = 'frequency')

data("OSDexamples", package = 'sharpshootR')

o <- OSDexamples$SPC
o$m <- sprintf("%s %s/%s", o$hue, o$value, o$chroma)

colorVariation(o$m, method = 'frequency')


o$colorVar <- profileApply(o, function(i) {
  colorVariation(i$m, method = 'frequency')
})

par(mar = c(0, 0, 0, 3))
plotSPC(o, plot.order = order(o$colorVar), name = NA, width = 0.33)


# approximate hz thickness weighting
o$thick <- o$bottom - o$top
o$hzwt <- pmax(round(o$thick / 10), 1)

o$colorVar2 <- profileApply(o, function(i) {
  colorVariation(rep(i$m, times = i$hzwt), method = 'frequency')
})



par(mar = c(0, 0, 0, 3), mfrow = c(2, 1))
plotSPC(o, plot.order = order(o$colorVar), name = NA, width = 0.33)
plotSPC(o, plot.order = order(o$colorVar2), name = NA, width = 0.33)



