# sequence for adding a new slot to SoilProfileCollection class.

data("jacobs2000", package="aqp")
# do some dataset-specific setting if needed
# hzdesgnname(jacobs2000) <- "name"
jacobs2000 <- rebuildSPC(jacobs2000)
save(jacobs2000, file = 'data/jacobs2000.rda')

data("rowley2019", package="aqp")
# ...
rowley2019 <- rebuildSPC(rowley2019)
save(rowley2019, file = 'data/rowley2019.rda')

data("sp5", package="aqp")
# ...
sp5 <- rebuildSPC(sp5)
save(sp5, file = 'data/sp5.rda')

# fixing soilDB SPCs assumes your package repos are in same parent directory...
data("loafercreek", package="soilDB")
# ...
loafercreek <- rebuildSPC(loafercreek)
save(loafercreek, file = '../soilDB/data/loafercreek.rda')

data("gopheridge", package="soilDB")
# ...
gopheridge <- rebuildSPC(gopheridge)
save(gopheridge, file = '../soilDB/data/gopheridge.rda')

data("mineralKing", package="soilDB")
# ...
mineralKing <- rebuildSPC(mineralKing)
save(mineralKing, file = '../soilDB/data/mineralKing.rda')