# sequence for adding a new slot to SoilProfileCollection class.
library(aqp)
data("jacobs2000", package="aqp")
# do some dataset-specific setting if needed
hzdesgnname(jacobs2000) <- "name"
hztexclname(jacobs2000) <- ""
jacobs2000 <- rebuildSPC(jacobs2000)
save(jacobs2000, file = 'data/jacobs2000.rda', compress = "xz")

data("rowley2019", package="aqp")
hzdesgnname(rowley2019) <- "name"
hztexclname(rowley2019) <- ""
rowley2019 <- rebuildSPC(rowley2019)
save(rowley2019, file = 'data/rowley2019.rda', compress = "xz")

data("wilson2022", package="aqp")
hzdesgnname(wilson2022) <- "name"
hztexclname(wilson2022) <- ""
wilson2022 <- rebuildSPC(wilson2022)
save(wilson2022, file = 'data/wilson2022.rda', compress = "xz")

data("osd", package="aqp")
hzdesgnname(osd) <- "hzname"
hztexclname(osd) <- "texture_class"
osd <- rebuildSPC(osd)
save(osd, file = 'data/osd.rda', compress = "xz")

data("sierraTransect", package="aqp")
hzdesgnname(sierraTransect) <- "name"
hztexclname(sierraTransect) <- ""
sierraTransect <- rebuildSPC(sierraTransect)
save(sierraTransect, file = 'data/sierraTransect.rda', compress = "xz")

data("sp5", package="aqp")
# replace emdash with hyphen (ascii)
# metadata(sp5)$citation <- gsub("\u2013","-",metadata(sp5)$citation)
hzdesgnname(sp5) <- ""
hztexclname(sp5) <- ""
sp5 <- rebuildSPC(sp5)
save(sp5, file = 'data/sp5.rda', compress = "xz")

# fixing soilDB SPCs assumes your package repos are in same parent directory...
data("loafercreek", package="soilDB")
hzdesgnname(loafercreek) <- 'hzname'
hztexclname(loafercreek) <- 'texcl'
loafercreek <- rebuildSPC(loafercreek)
save(loafercreek, file = '../soilDB/data/loafercreek.rda', compress = "xz")

data("gopheridge", package="soilDB")
hzdesgnname(gopheridge) <- 'hzname'
hztexclname(gopheridge) <- 'texcl'
gopheridge <- rebuildSPC(gopheridge)
save(gopheridge, file = '../soilDB/data/gopheridge.rda', compress = "xz")

data("mineralKing", package="soilDB")
hzdesgnname(mineralKing) <- 'hzname'
hztexclname(mineralKing) <- 'texcl'
mineralKing <- rebuildSPC(mineralKing)
save(mineralKing, file = '../soilDB/data/mineralKing.rda', compress = "xz")

