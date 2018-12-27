library(aqp)
library(plyr)

x <- read.csv('OSACA-example_soil_data.csv', stringsAsFactors = FALSE)

# assume horizons are measured from 0
x.new <- ddply(x, .(soil), .fun=function(i) {data.frame(i, top=c(0, i$depth[-length(i$depth)]), bottom=i$depth)})

# fix depths
x.new$top <- as.integer(x.new$top)
x.new$bottom <- as.integer(x.new$bottom)

# init SPC
depths(x.new) <- soil ~ top + bottom

## max RGB value?
# R,G,B appear to be color data
x.new$soil_color <- rgb(x.new$R25, g=x.new$G25, b=x.new$B25, maxColorValue = 1)

# plot(x.new[1:25, ])
# plot(sample(x.new, 25))

# metadata
m <- metadata(x.new)
m$citation <- "F. Carre, M.C. Girard. 2002. Quantitative mapping of soil types based on regression kriging of taxonomic distances with landform and land cover attributes. Geoderma. 110: 241-263."
m$dateAdded <- Sys.Date()

metadata(x.new) <- m

# copy for sample data sp5
sp5 <- x.new

# cleanup
sp5@horizons$depth <- NULL
sp5@horizons$hor <- NULL
sp5@horizons$soil <- as.character(sp5@horizons$soil)

# save to pacakge
save(sp5, file='../../data/sp5.rda')
