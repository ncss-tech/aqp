## New S4 classes for AQP
##

## The concept here is to detach the physical support of soil data
## from the attribute space.
##

## Class describing a single soil profile
##
setClass(
  Class = 'SoilProfile', 
  # Class definition
  # 
  representation = representation(
    # Unique identifier
    id = 'character', 
    # Support of information
    depths = 'matrix', # matrix with horizons top and bottom depths
    depth_units = 'character',
    sp = 'SpatialPoints', # (optional) spatial support of the information
    # Attribute space
    horizons = 'data.frame', # horizon-level data
    site = 'data.frame' # (optional) site-level data
  ),
  # Prototype of the class
  # 
  prototype = prototype(
    id = as.character(NA),
    depths = matrix(
      nrow = 0, 
      ncol = 2, 
      dimnames = list(NULL, c('top', 'bottom'))
    ),
    depth_units = 'cm',
    sp = new('SpatialPoints'),
    horizons = data.frame(),
    site = data.frame()
  )
)

## Class describing a collection of soil profiles 
## 
setClass(
  Class = 'SPC', 
  representation = representation(
    profiles = 'list' # list of SoilProfiles
  ),
  prototype = prototype(
    profiles = list(new('SoilProfile'))
  )
)
