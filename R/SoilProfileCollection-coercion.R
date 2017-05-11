## Coercition methods

as.data.frame.SoilProfile <- function(x, ...)  {
  
  # ID (always defined)
  df_id <- data.frame(rep(x@id, times = nrow(x)))
  names(df_id) <- names(x@id)
  
  # Depths (always defined)
  df_depths <- data.frame(x@depths)
  
  # Spatial point
  if (.hasSpatialPoint(x)) {
    df_sp <- as.data.frame(coordinates(x@sp))
    # replicate coordinates for each hz
    df_sp <- ldply(1:nrow(x), function(x) df_sp)
  } else {
    df_sp <- NULL
  }
  
  # Horizon data
  if (nrow(horizons(x)) > 0) {
    df_hz <- horizons(x)
  } else {
    df_hz <- NULL
  }
  
  # Site data
  if (nrow(site(x)) > 0) {
    df_site <- site(x)
    # replicate site data for each hz
    df_site <- ldply(1:nrow(x), function(x) df_site)
  } else {
    df_site <- NULL
  }
  
  l_df <- list(df_id, df_depths, df_sp, df_hz, df_site)
  # Eliminate NULL fields
  l_df <- l_df[which(laply(l_df, function(x) !is.null(x)))]
  
  do.call('cbind', c(l_df, row.names = NULL))
}

setAs("SoilProfile", "data.frame", function(from)
  as.data.frame.SoilProfile(from))

as.data.frame.SoilProfileCollection <- function(x, ...) {
  data.frame( do.call('rbind', lapply(profiles(x), as.data.frame)) , row.names = NULL)
}

setAs("SoilProfileCollection", "data.frame", function(from)
  as.data.frame.SoilProfileCollection(from))

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

setAs("SoilProfileCollection", "SpatialPoints", 
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

setAs("SoilProfileCollection", "SpatialPointsDataFrame", 
      function(from) {
        sp <- as(from, "SpatialPoints")
        
        if (is.null(sp)) spdf <- NULL
        else spdf <- SpatialPointsDataFrame(sp, data = site(from))
        
        spdf
      }
)
