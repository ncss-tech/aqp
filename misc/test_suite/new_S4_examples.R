library(plyr)
source("Class-SoilProfile.R")
source("Class-SoilProfileCollection.R")
source('SoilProfile-methods.R')
source('SoilProfileCollection-methods.R')
source('setters.R')

data(sp1, package='aqp')

# creation of a data frame with more site data than sp1:
spc <- sp1
spc$x <- unlist(dlply(spc, .(id), function(x){n <- nrow(x);
rep(runif(1), length.out=n)}))
spc$y <- unlist(dlply(spc, .(id), function(x){n <- nrow(x);
rep(runif(1), length.out=n)}))
spc$z <- unlist(dlply(spc, .(id), function(x){n <- nrow(x);
rep(runif(1), length.out=n)}))

sp <- spc[spc$id == "P001",]

# let's initialize
# SoilProfile
depths(sp) <- id ~ top + bottom

# SoilProfileCollection
depths(spc) <- id ~ bottom + top # GOTYA! the order of top and bottom matters...
depths(spc) <- id ~ top + bottom


summary(sp)
summary(spc)

# SoilProfile
site(sp) <- ~ x+y+z

# SoilProfileCollection
site(spc) <- ~ x+y+z

summary(sp)
summary(spc)




