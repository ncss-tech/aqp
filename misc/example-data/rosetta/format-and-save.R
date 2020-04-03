## set factor levels

# from Rosetta manual, then manually edited
x <- read.csv('centroids.csv', stringsAsFactors = FALSE)

# borrowed from fetchOSD()
textures <- c('coarse sand', 'sand', 'fine sand', 'very fine sand', 'loamy coarse sand', 'loamy sand', 'loamy fine sandy', 'loamy very fine sand', 'coarse sandy loam', 'sandy loam', 'fine sandy loam', 'very fine sandy loam', 'loam', 'silt loam', 'silt', 'sandy clay loam', 'clay loam', 'silty clay loam', 'sandy clay', 'silty clay', 'clay')

# set levels
x$texture <- droplevels(factor(x$texture, levels = textures, ordered = TRUE))


