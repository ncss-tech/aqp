library(soilDB)

## TODO: consolidte factor levels / codes: https://github.com/ncss-tech/aqp/issues/141

# from Rosetta documentation, then _manually_ edited
x <- read.csv('centroids-edited.csv', stringsAsFactors = FALSE)

# encode factors according to approx AWC
x$texture <- factor(x$texture, levels=SoilTextureLevels(which = 'names'), ordered = TRUE)

# drop extra levels
x$texture <- droplevels(x$texture)


# iterate over centroids and develop simplistic 
res <- lapply(1:nrow(x), function(i) {
  # fit model using parameters from centroids
  vg <- KSSL_VG_model(VG_params = x[i, ], phi_min = 10^-3, phi_max=10^6)
  
  # extract VWC at specific matric potentials
  d <- data.frame(
    texture = x$texture[i], 
    sat = vg$VG_function(0),
    fc = vg$VG_function(33),
    pwp = vg$VG_function(1500)
  )
  
  # simplistic determination of AWC using 33 kPa -> 1500 kPa interval
  d$awc <- with(d, fc - pwp)
  
  return(d)
})

# list -> DF
res <- do.call('rbind', res)

# merge with centroids
x <- merge(x, res, by='texture', all.x=TRUE, sort = FALSE)

# rename and save
ROSETTA.centroids <- x
save(ROSETTA.centroids, file='../../../data/ROSETTA.centroids.rda')
