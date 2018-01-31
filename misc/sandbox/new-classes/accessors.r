# Accessors for SoilProfile and SPC classes
# 

## Get IDs
##
if (!isGeneric('ids'))
  setGeneric('ids', function(object) 
    standardGeneric('ids'))

setMethod("ids", "SoilProfile",
          function(object) {
            object@id
          }
)

setMethod("ids", "SPC",
          function(object) {
            laply(object@profiles, ids)
          }
)

## Get depths
##
if (!isGeneric('depths'))
  setGeneric('depths', function(object) 
    standardGeneric('depths'))

setMethod("depths", "SoilProfile",
          function(object) {
            object@depths
          }
)

setMethod("depths", "SPC",
          function(object) {
            lapply(object@profiles, depths)
          }
)

if (!isGeneric('depth_units'))
  setGeneric('depth_units', function(object) 
    standardGeneric('depth_units'))

setMethod("depth_units", "SoilProfile",
          function(object) {
            object@depth_units
          }
)

setMethod("depth_units", "SPC",
          function(object) {
            unique(laply(object@profiles, depth_units))
          }
)

## Get list or unique SoilProfile
##
if (!isGeneric('profiles'))
  setGeneric('profiles', function(object, i = NULL) 
    standardGeneric('profiles'))

setMethod("profiles", "SPC",
          function(object, i = NULL) {
            if (is.null(i)) object@profiles
            else object@profiles[[i]]
          }
)

## Get horizons data
##
if (!isGeneric('horizons'))
  setGeneric('horizons', function(object, ...) 
    standardGeneric('horizons'))

setMethod("horizons", "SoilProfile",
          function(object) {
            object@horizons
          })

setMethod("horizons", "SPC",
          function(object, as.list = FALSE) {
            res <- lapply(object@profiles, function(x) x@horizons)
            if (!as.list) {
              res <- data.frame(do.call('rbind', res), row.names = NULL)
            }
            res
          })

## Get site data
##
if (!isGeneric('site'))
  setGeneric('site', function(object, ...) 
    standardGeneric('site'))

setMethod("site", "SoilProfile",
          function(object) {
            object@site
          })

setMethod("site", "SPC",
          function(object, as.list = FALSE) {
            res <- lapply(profiles(object), site)
            if (!as.list) {
              res <- data.frame(do.call('rbind', res), row.names = NULL)
            }
            res
          }
)

# return a concatenated vector of horizon + site names
# 

setMethod("names", "SoilProfile",
  function(x) {
    res <- c(horizons = horizonNames(object), site = siteNames(object))
    return(res)
  }
)

setMethod("names", "SPC",
  function(x) {
    names(profiles(object, 1))
  }
)

## get horizon column names
##

if (!isGeneric("horizonNames"))
  setGeneric("horizonNames", function(object, ...) standardGeneric("horizonNames"))

setMethod("horizonNames", "SoilProfile",
          function(object)
            return(names(horizons(object)))
)

setMethod("horizonNames", "SPC",
          function(object)
            # unnecessary coutious implementation:
            # unique(unlist(lapply(profiles(object), horizonNames)))
            return(horizonNames(profiles(object, 1)))
)

## get site column names
## 

if (!isGeneric("siteNames"))
  setGeneric("siteNames", function(object, ...) standardGeneric("siteNames"))

setMethod("siteNames", "SoilProfile",
          function(object) names(site(object))
)

setMethod("siteNames", "SPC",
          function(object) siteNames(profiles(object, 1))
)
