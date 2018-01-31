library(sp)
library(rgdal)
library(raster)
library(aqp)
library(plyr)
library(stringr)

source('classes-rebooted.r')

source('init.r')
source('accessors.r')
source('setters.r')
source('overloads.r')
source('coerce.r')
source('spatial.r')

data(sp4, package = 'aqp')
foo <- sp4

# Initialisation of a SPC object
#

depths(foo) <- id ~ top + bottom
foo

# Note that we can limit the number of attributes selected
foo <- sp4
depths(foo) <- id ~ top + bottom ~ sand + silt + clay
foo

# Print and summary
#
print(foo)
summary(foo)

# Utility functions
# 
ids(foo)
depths(foo)
horizons(foo)
horizons(foo, as.list = TRUE) # for use with l*ply type functions
# horizons(foo, as.list = TRUE) %>% ldply(identity, .id = "foo")
site(foo)

# Extracting profiles
#
profiles(foo, 1) # in this case note that a single profile is a SoilProfile object
profiles(foo) # list() of SoilProfile objects -- useful when coding other functions

# Various overloads
#
length(foo) # Number of profiles

nrow(foo) # total number of hz
nrow(profiles(foo, 1)) # number of hz in first profile 
lapply(profiles(foo), nrow) # Number of hz in each profile
lapply(profiles(foo), nrow) > 3 # Just an example of application
lapply(profiles(foo), function(i) i$clay)

# Min depths
min(foo) # minimum depth in collection
min(profiles(foo, 1)) # minimum depth in a given profile
lapply(profiles(foo), min) # list of minimum depths
# Same for max
max(foo)
max(profiles(foo, 1))
lapply(profiles(foo), max)

# Coercicion (just back to data.frame at the moment)
as.data.frame(foo) # for the whole collection
as.data.frame(profiles(foo, 1)) # for a specific profile

# Spatial stuff

# Adding some coordinates
rp <- ldply(1:100, aqp::random_profile)
  
foo <- ddply(
  rp, 
  .(id), 
  function(x) data.frame(
    x, 
    x = rep(runif(1), length.out = nrow(x)),   
    y = rep(runif(1), length.out = nrow(x)),
    z = rep(10 * runif(1), length.out = nrow(x))
  )
)
depths(foo) <- id ~ top + bottom
site(foo) <- ~ x + y + z
coordinates(foo) <- ~x+y
proj4string(foo) <- CRS('+init=epsg:4326')

# Create fake env. covariate data
cov <- expand.grid(x = seq(0, 1, length.out = 100), y = seq(0, 1, length.out = 100))
cov$band.1 <- round(100 * runif(nrow(cov)))
cov$band.2 <- factor(sample(LETTERS[1:3], size = nrow(cov), replace = TRUE))
spdf <- cov

# SpatialPixelsDataFrame
coordinates(spdf) <- ~x+y
proj4string(spdf) <- CRS('+init=epsg:4326')
gridded(spdf) <- TRUE

# Raster* objects
r <- raster(spdf, 'band.1')
s <- stack(spdf)

# SpatialPolygonsDataFrame
pcoords <- list(
  rbind(c(0, 1), c(0.5, 1), c(0.5, 0.5), c(0, 0.5), c(0, 1)),
  rbind(c(0.5, 1), c(1, 1), c(1, 0.5), c(0.5, 0.5), c(0.5, 1)),
  rbind(c(0.5, 0.5), c(1, 0.5), c(1, 0), c(0.5, 0), c(0.5, 0.5)),
  rbind(c(0, 0.5), c(0.5, 0.5), c(0.5, 0), c(0, 0), c(0, 0.5))
)
ps <- lapply(1:4, function(x) Polygons(list(Polygon(pcoords[[x]])), ID = LETTERS[x]))
sps <- SpatialPolygons(ps, proj4string = CRS('+init=epsg:4326'))
polydf <- SpatialPolygonsDataFrame(sps, data = data.frame(poly = LETTERS[1:4]), match.ID = FALSE)

extract_covariates(foo, r)
extract_covariates(foo, s)
extract_covariates(foo, spdf)
extract_covariates(foo, spdf[,'band.1'])
extract_covariates(foo, polydf)

extract_covariates(foo, r) %>% class
site(foo) <- df

baz <- add_covariates(foo, r)
site(baz)
baz <- add_covariates(foo, spdf)
site(baz)
