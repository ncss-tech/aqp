library(aqp)
library(soilDB)
osd <- fetchOSD(c('appling', 'cecil', 'bonneau'))


# save to package
save(osd, file='../../../data/osd.rda', compress = 'xz')
