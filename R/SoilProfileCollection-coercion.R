## Coercition methods: general

# safely deconstruct as list
setAs("SoilProfileCollection", "list", function(from) {
  
  # get slot names from prototype
  sn <- slotNames(from)
  
  # test for presence of all slots
  # copy contents over to list with same name
  # if missing return NULL + warning
  s.list <- lapply(sn, function(i) {
    if(.hasSlot(from, name=i)) {
      res <- slot(from, i)
    } else {
      res <- NULL
    }
    return(res)
  })
  
  # copy slot names
  names(s.list) <- sn
  
  # test for missing slots
  if(any(sapply(s.list, is.null))) {
    warning("some slots were missing, use reBuildSPC to fix", call. = FALSE)
  }
  
  return(s.list)
  
}
)


setAs("SoilProfileCollection", "data.frame", function(from) {
  
  # horizons + site + coordinates
  if(nrow(site(from)) > 0 & nrow(coordinates(from)) == length(from)) {
    site.coords <- data.frame(site(from), coordinates(from), stringsAsFactors=FALSE)
    return(join(horizons(from), site.coords, by=idname(from)))
  }
  
  # horizons + site
  if(nrow(site(from)) > 0 & ! nrow(coordinates(from)) == length(from))
    return(join(horizons(from), site(from), by=idname(from)))
    
  # horizons + coordinates
  if(! nrow(site(from)) > 0 & nrow(coordinates(from)) == length(from)) {
    ids.coords <- data.frame(profile_id(from), coordinates(from), stringsAsFactors=FALSE)
    return(data.frame(horizons(from), ids.coords, stringsAsFactors=FALSE))
  }
  
  # just horizons
  else {
    return(horizons(from))
  }
  
}
)

## TODO: why does the proj4string get mangled in the conversion?
## Coercition methods: and sp utilities
setAs("SoilProfileCollection", "SpatialPointsDataFrame", function(from) {
    s <- SpatialPointsDataFrame(coordinates(from), data = site(from), proj4string=CRS(proj4string(from)), match.ID = FALSE)
    message('only site data are extracted')
    return(s)
  }
)

# SoilProfilecollection -> SpatialPoints
# requires special handling of "emtpy" SpatialPoints Objects
setAs("SoilProfileCollection", "SpatialPoints", function(from) {
    SpatialPoints(coordinates(from), proj4string=CRS(proj4string(from)))
  }
)





