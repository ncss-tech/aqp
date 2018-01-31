library(plyr)
library(aqp)
data(sp1, package='aqp')
# creation of a data frame with more site data than sp1:
spc <-  sp1
# adding a soil colour slot
spc$soil_color <- with(spc, munsell2rgb(hue, value, chroma))
detach(package:aqp)

source("../branches/aqp_S4/R/Class-SoilProfile.R")
source("../branches/aqp_S4/R/Class-SoilProfileCollection.R")
source('../branches/aqp_S4/R/SoilProfile-methods.R')
source('../branches/aqp_S4/R/SoilProfileCollection-methods.R')
source('../branches/aqp_S4/R/setters.R')

spc$x <- unlist(dlply(spc, .(id), function(x){n <- nrow(x);
rep(runif(1), length.out=n)}))
spc$y <- unlist(dlply(spc, .(id), function(x){n <- nrow(x);
rep(runif(1), length.out=n)}))
spc$z <- unlist(dlply(spc, .(id), function(x){n <- nrow(x);
rep(runif(1), length.out=n)}))

sp1 <- spc[spc$id == "P001",]
sp2 <- spc[spc$id == "P002",]

# let's initialize
# SoilProfile
depths(sp1) <- id ~ top + bottom
depths(sp2) <- id ~ top + bottom

# SoilProfileCollection
depths(spc) <- id ~ top + bottom

# site data
site(spc) <- ~ x + y + z

