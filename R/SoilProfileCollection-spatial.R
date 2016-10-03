
##
## wrappers to spatial operations via sp, rgdal, raster
##

# Utility function that tests if 
# a SoilProfile has spatial coordinates
.hasSpatialPoint <- function(x) !identical(x@sp, new('SpatialPoints'))

# proj4string

setMethod(f = 'proj4string', signature = 'SoilProfile',
          function(obj){
            obj@sp@proj4string@projargs
          }
)

setMethod(f = 'proj4string', signature = 'SoilProfileCollection',
          function(obj){
            unique(unlist(lapply(profiles(obj), proj4string)))
          }
)

setReplaceMethod("proj4string", "SoilProfile",
                 function(obj, value) {
                   proj4string(obj@sp) <- value
                   obj
                 }
)

setReplaceMethod("proj4string", "SoilProfileCollection",
                 function(obj, value) {
                   obj <- lapply(profiles(obj), function(x){
                     proj4string(x) <- value
                     x
                   })
                   
                   SoilProfileCollection(profiles = obj)
                 }
)

# coordinates

setMethod("coordinates", "SoilProfile",
          function(obj) {
            res <- coordinates(obj@sp)
            # over-writing the default sp::coordinates behaviour: we
            # return NULL if no coordinates
            if (ncol(res) < 2) res <- NULL
            res
          }
)

setMethod("coordinates", "SoilProfileCollection",
          function(obj) {
            do.call(rbind, lapply(profiles(obj), coordinates))
          }
)

setReplaceMethod("coordinates", "SoilProfile",
                 function(object, value) {
                   
                   # basic sanity check... needs work
                   if(! inherits(value, "formula"))
                     stop('invalid formula', call. = FALSE)
                   
                   # extract coordinates as matrix
                   mf <- model.frame(value, site(object), na.action = na.pass)
                   nm <- names(mf)
                   mf <- data.matrix(mf, rownames.force = FALSE)
                   
                   # test for missing coordinates
                   mf.missing <- apply(mf, 2, is.na)
                   
                   if(any(mf.missing))
                     stop('cannot initialize a SpatialPoints object with missing coordinates', call. = FALSE)
                   
                   # Instanciate the sp slot
                   object@sp <- SpatialPoints(coord = mf)
                   
                   # Remove coordinates from site slot
                   object@site <- object@site[,-1 * which(names(object@site) %in% nm), drop = FALSE]
                   
                   object
                 }
)

setReplaceMethod("coordinates", "SoilProfileCollection",
                 function(object, value) {
                   
                   lspc <- lapply(profiles(object), function(x) {
                     coordinates(x) <- value
                     x
                   })
                   
                   SoilProfileCollection(profiles = lspc)
                   
                 }
)
