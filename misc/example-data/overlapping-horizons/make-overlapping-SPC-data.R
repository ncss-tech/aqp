library(aqp)

x <- read.csv('misc/example-data/overlapping-horizons/2020GRR014SPCdata.csv')

depths(x) <- upedonid ~ hzdept + hzdepb
hzdesgnname(x) <- 'hzname'
site(x) <- ~ NasisSiteName + taxonname + dspplotid + earthcovkind1 + earthcovkind2

x <- x[c(1, 2, 3, 4, 8), ]


SPC.with.overlap <- x

save(SPC.with.overlap, file = 'data/SPC.with.overlap.rda', compress = 'xz')
