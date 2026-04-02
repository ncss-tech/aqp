# test / demonstrate cached results in estimateSoilColor()
# think about other functions that could benefit from similar strategy


# library(aqp)
library(soilDB)
library(mvtnorm)

data(loafercreek, package = 'soilDB')

# generalize horizon names using REGEX rules
n <- c('Oi', 'A', 'BA','Bt1','Bt2','Bt3','Cr','R')
p <- c('O', '^A$|Ad|Ap|AB','BA$|Bw',
       'Bt1$|^B$','^Bt$|^Bt2$','^Bt3|^Bt4|CBt$|BCt$|2Bt|2CB$|^C$','Cr','R')
loafercreek$genhz <- generalize.hz(loafercreek$hzname, n, p)

# remove non-matching generalized horizon names
loafercreek$genhz[loafercreek$genhz == 'not-used'] <- NA
loafercreek$genhz <- factor(loafercreek$genhz)

# all colors
.hvc <- data.frame(
  hue = loafercreek$m_hue,
  value = loafercreek$m_value,
  chroma = loafercreek$m_chroma
)

p <- list(
  list(hvc = .hvc)
)

# result is a list
m <- simulateColor(method = 'mvnorm', n = 100, parameters = p)

colorChart(m[[1]])


options(.aqp.verbose = TRUE)
z <- estimateSoilColor(.hvc$hue, .hvc$value, .hvc$chroma, sourceMoistureState = 'dry')

nrow(z) == nrow(.hvc)
