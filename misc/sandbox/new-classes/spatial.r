## 
## Spatial stuff
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

setMethod(f = 'proj4string', signature = 'SPC',
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

setReplaceMethod("proj4string", "SPC",
                 function(obj, value) {
                   obj <- lapply(profiles(obj), function(x){
                     proj4string(x) <- value
                     x
                   })
                   
                   SPC(profiles = obj)
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

setMethod("coordinates", "SPC",
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

setReplaceMethod("coordinates", "SPC",
                 function(object, value) {
                   
                   lspc <- lapply(profiles(object), function(x) {
                     coordinates(x) <- value
                     x
                   })
                   
                   SPC(profiles = lspc)
                   
                 }
)

## Get SpatialPoints
##

setAs("SoilProfile", "SpatialPoints", 
      function(from) {
        if (.hasSpatialPoint(from)){
          sp <- from@sp
        } else {
          sp <- NULL
        }
        sp
      }
)

setAs("SPC", "SpatialPoints", 
      function(from) {
        do.call("rbind", lapply(profiles(from), function(x) as(x, "SpatialPoints")))
      }
)

setAs("SoilProfile", "SpatialPointsDataFrame", 
      function(from) {
        sp <- as(from, "SpatialPoints")
        
        if (is.null(sp)) spdf <- NULL
        else spdf <- SpatialPointsDataFrame(sp, data = site(from))
        
        spdf
      }
)

setAs("SPC", "SpatialPointsDataFrame", 
      function(from) {
        sp <- as(from, "SpatialPoints")
        
        if (is.null(sp)) spdf <- NULL
        else spdf <- SpatialPointsDataFrame(sp, data = site(from))
        
        spdf
      }
)

# Extract site data from covariates

extract_covariates <- function(spc, cov) {
  
  # Get spatial points
  sp <- as(spc, "SpatialPoints")
  
  if(inherits(cov, "Raster")) {
    
    df <- extract(x = cov, y = sp)
    
    # If this i a RasterLayer
    if (nlayers(cov) < 2) {
      df <- data.frame(df)
      names(df) <- names(cov)
    } else {
      # Just coerce matrix to data.frame
      df <- data.frame(df)
    }
  } else if(inherits(cov, "Spatial")) {
    df <- sp %over% cov
  } else stop('invalid covariate')
  
  df
}

add_covariates <- function(spc, cov) {
  
  df <- extract_covariates(spc, cov)
  
  lsp <- lapply(1:length(spc), function(x){
    p <- profiles(spc, x)
    
    if (nrow(site(p)) == 0) {
      p@site <- df[x,, drop = FALSE]
    } else {
      p@site <- data.frame(p@site, df[x,, drop = FALSE])
    }
    
    p
  })
  
  SPC(profiles = lsp)
}