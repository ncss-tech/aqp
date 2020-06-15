## Coercion methods: general

#' Coerce SoilProfileCollection with \code{as()}
#' @description SoilProfileCollections can be coerced to other R object types using \code{as(spc, 'type')}.
#' 
#' Possible endpoints include: \code{list}, \code{data.frame}, \code{tbl_df}, \code{data.table}, \code{SpatialPointsDataFrame} and \code{SpatialPoints}.
#'
#' @name as
#' @return list
#' @aliases as,SoilProfileCollection-method
#' @docType methods
#' @rdname coercion-methods
#'
#' @examples 
#' # load example data stored as SoilProfileCollection
#' data(sp5)
#' 
#' # list output
#' as(sp5, 'list')
#'
#' # data.frame output
#' as(sp5, 'data.frame')
#' 
#' # tbl_df output
#' as(sp5, 'tbl_df')
#' 
#' # data.table output
#' as(sp5, 'data.table')
#' 
#' # SPDF output
#' as(sp5, 'SpatialPointsDataFrame')
#' 
#' # SP output
#' as(sp5, 'SpatialPoints')
#' 
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
    warning("some slots were missing, use rebuildSPC to fix", call. = FALSE)
  }
  
  return(s.list)
  
})

#' @name as
#' @return data.frame
#' @aliases as,SoilProfileCollection-method
#' @docType methods
#' @rdname coercion-methods
#'
setAs("SoilProfileCollection", "data.frame", function(from) {
  
  # horizons + site + coordinates
  if(nrow(site(from)) > 0 & nrow(coordinates(from)) == length(from)) {
    site.coords <- data.frame(site(from), coordinates(from), stringsAsFactors = FALSE)
    return(join(horizons(from), site.coords, by=idname(from)))
  }
  
  # horizons + site
  if(nrow(site(from)) > 0 & ! nrow(coordinates(from)) == length(from))
    return(join(horizons(from), site(from), by=idname(from)))
    
  # horizons + coordinates
  if(! nrow(site(from)) > 0 & nrow(coordinates(from)) == length(from)) {
    ids.coords <- data.frame(profile_id(from), coordinates(from), stringsAsFactors = FALSE)
    return(data.frame(horizons(from), ids.coords, stringsAsFactors = FALSE))
  }
  
  # just horizons
  else {
    return(horizons(from))
  }
})

#' @name as
#' @return tbl_df
#' @aliases as,SoilProfileCollection-method
#' @docType methods
#' @rdname coercion-methods
#'
setAs("SoilProfileCollection", "tbl_df", function(from) {
  .as.data.frame.aqp(as(from,'data.frame'), 'tbl_df')
})

#' @name as
#' @return data.table
#' @aliases as,SoilProfileCollection-method
#' @docType methods
#' @rdname coercion-methods
#'
setAs("SoilProfileCollection", "data.table", function(from) {
  .as.data.frame.aqp(as(from, 'data.frame'), 'data.table')
})

#' @name as
#' @return SpatialPointsDataFrame
#' @aliases as,SoilProfileCollection-method
#' @docType methods
#' @rdname coercion-methods
setAs("SoilProfileCollection", "SpatialPointsDataFrame", function(from) {
  s <- SpatialPointsDataFrame(coordinates(from), data = site(from), proj4string=CRS(proj4string(from)), match.ID = FALSE)
  message('only site data are extracted')
  return(s)
}
)

#' @name as
#' @return SpatialPoints
#' @aliases as,SoilProfileCollection-method
#' @docType methods
#' @rdname coercion-methods
setAs("SoilProfileCollection", "SpatialPoints", function(from) {
    SpatialPoints(coordinates(from), proj4string = CRS(proj4string(from)))
  }
)
