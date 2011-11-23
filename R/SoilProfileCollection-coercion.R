## Coercition methods: general
setAs("SoilProfileCollection", "data.frame", function(from) {
  
  # horizons + site + coordinates
  if(nrow(site(from)) > 0 & nrow(coordinates(from)) == length(from)) {
    site.coords <- data.frame(site(from), coordinates(from))
    return(join(horizons(from), site.coords))
  }
  
  # horizons + site
  if(nrow(site(from)) > 0 & ! nrow(coordinates(from)) == length(from))
    return(join(horizons(from), site(from)))
    
  # horizons + coordinates
  if(! nrow(site(from)) > 0 & nrow(coordinates(from)) == length(from)) {
    ids.coords <- data.frame(profile_id(from), coordinates(from))
    return(data.frame(horizons(from), ids.coords))
  }
  
  # just horizons
  else {
    return(horizons(from))
  }
  
}
)

## Coercition methods: and sp utilities
setAs("SoilProfileCollection", "SpatialPointsDataFrame", function(from) {
    cat('ony site data are extracted\n')
    SpatialPointsDataFrame(coordinates(from), data = from@site)
  }
)
