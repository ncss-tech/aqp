library(aqp)

## hz data
# minor cleaning of column names
hz <- read.table('hz.txt', header=TRUE, stringsAsFactors = FALSE)
str(hz)

# profile ID
ids <- strsplit(hz$sample_id, split='.', fixed = TRUE)
hz$id <- sapply(ids, '[', 1)

# group
hz$group <- factor(substr(hz$id, 1, 1), levels=c('B', 'F'), labels=c('CaCO3-bearing ', 'CaCO3-free'))

# depths
dp <- strsplit(hz$depth, split='-', fixed = TRUE)
hz$top <- as.integer(sapply(dp, '[', 1))
hz$bottom <- as.integer(sapply(dp, '[', 2))

hz$depth <- NULL

## geochem data
geo <- read.table('geochem.txt', header = TRUE, stringsAsFactors = FALSE)

## mineralogy
# removed '*' footnote about calcite peaks
min <- read.table('min.txt', header = TRUE, stringsAsFactors = FALSE)

## calcium resevoirs
ca <- read.table('ca-res.txt', header = TRUE, stringsAsFactors = FALSE)


## merge
x <- merge(hz, geo, by='sample_id', all.x=TRUE, sort = FALSE)
x <- merge(x, min, by='sample_id', all.x=TRUE, sort = FALSE)
x <- merge(x, ca, by='sample_id', all.x=TRUE, sort = FALSE)

## check: ok
str(x)

## init SPC
depths(x) <- id ~ top + bottom

# move site level attr
site(x) <- ~ group

plot(x, color='Feo_Fed')


## re-name and save
rowley2019 <- x
save(rowley2019, file='../../../data/rowley2019.rda')


## ideas for later
# library(aqp)
# library(sharpshootR)
# library(cluster)
# 
# d <- profile_compare(rowley2019, vars=c('Ca', 'Al', 'K_Feldspar', 'Reactive_carbonate'), max_d=45, k=0)
# 
# par(mar=c(1,1,3,1))
# plotProfileDendrogram(rowley2019, diana(d), scaling.factor = 0.75, width = 0.25, y.offset = 3, color='Al', name='name')
# addVolumeFraction(rowley2019, colname = 'Al_Exch_saturation')
# 
# par(mar=c(1,1,3,1))
# plotProfileDendrogram(rowley2019, diana(d), scaling.factor = 0.75, width = 0.25, y.offset = 3, color='Al', name='name')
# addVolumeFraction(rowley2019, colname = 'Quartz')
# 
# 
# 
# data('rowley2019')
# 
# par(mar=c(1,1,3,1))
# plotSPC(rowley2019, color='Feo_Fed', name='name', cex.names=0.85)


