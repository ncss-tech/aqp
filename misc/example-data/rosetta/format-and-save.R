
## TODO: consolidte factor levels / codes: https://github.com/ncss-tech/aqp/issues/141

# from Rosetta documentation, then _manually_ edited
x <- read.csv('centroids-edited.csv', stringsAsFactors = FALSE)

# encode factors according to approx AWC
x$texture <- factor(x$texture, levels=SoilTextureLevels(which = 'names'), ordered = TRUE)

# drop extra levels
x$texture <- droplevels(x$texture)

# rename and save
ROSETTA.centroids <- x
save(ROSETTA.centroids, file='../../../data/ROSETTA.centroids.rda')
