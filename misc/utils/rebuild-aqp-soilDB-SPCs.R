# sequence for adding a new slot to SoilProfileCollection class.

data("jacobs2000", package="aqp")
# do some dataset-specific setting if needed
hzdesgnname(jacobs2000) <- "name"
hztexclname(jacobs2000) <- ""
jacobs2000 <- rebuildSPC(jacobs2000)
save(jacobs2000, file = 'data/jacobs2000.rda')

data("rowley2019", package="aqp")
hzdesgnname(rowley2019) <- "name"
hztexclname(rowley2019) <- ""
rowley2019 <- rebuildSPC(rowley2019)
save(rowley2019, file = 'data/rowley2019.rda')

data("sp5", package="aqp")
hzdesgnname(sp5) <- ""
hztexclname(jacobs2000) <- ""
sp5 <- rebuildSPC(sp5)
save(sp5, file = 'data/sp5.rda')

# fixing soilDB SPCs assumes your package repos are in same parent directory...
data("loafercreek", package="soilDB")
hzdesgnname(loafercreek) <- 'hzname'
hztexclname(loafercreek) <- 'texcl'
loafercreek <- rebuildSPC(loafercreek)
save(loafercreek, file = '../soilDB/data/loafercreek.rda')

data("gopheridge", package="soilDB")
hzdesgnname(gopheridge) <- 'hzname'
hztexclname(gopheridge) <- 'texcl'
gopheridge <- rebuildSPC(gopheridge)
save(gopheridge, file = '../soilDB/data/gopheridge.rda')

data("mineralKing", package="soilDB")
hzdesgnname(mineralKing) <- 'hzname'
hztexclname(mineralKing) <- 'texcl'
mineralKing <- rebuildSPC(mineralKing)
save(mineralKing, file = '../soilDB/data/mineralKing.rda')