## Coercion methods: general

#' Coerce SoilProfileCollection with \code{as()}
#' 
#' @description SoilProfileCollections can be coerced to other R object types using \code{as(spc, 'type')}.
#' 
#' Possible endpoints include: \code{list}, \code{data.frame}, \code{SpatialPointsDataFrame} and \code{SpatialPoints}.
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
#' # sp5
#' str(sp5)
#' 
#' # list output
#' str(as(sp5, 'list'))
#'
#' # data.frame output
#' str(as(sp5, 'data.frame'))
#' 
#' # Spatial Objects
#' # make some random coordinate data for each profile
#' sp5$x <- sp5$y <- rnorm(length(sp5))
#' coordinates(sp5) <- ~ x + y
#' aqp::crs(sp5) <- "EPSG:4326"
#' 
#' # SpatialPointsDataFrame output
#' str(as(sp5, 'SpatialPointsDataFrame'))
#' 
#' # SpatialPoints output
#' str(as(sp5, 'SpatialPoints'))
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
    return(merge(horizons(from), site.coords, by = idname(from), sort = FALSE, all.x = TRUE))
  }
  
  # horizons + site
  if(nrow(site(from)) > 0 & ! nrow(coordinates(from)) == length(from))
    return(merge(horizons(from), site(from), by = idname(from), sort = FALSE, all.x = TRUE))
    
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
  if (!requireNamespace("sf")) {
    stop("Package 'sf' is required to coerce a SoilProfileCollection to SpatialPointsDataFrame object", call. = FALSE)
  }
  sf::as_Spatial(as(from, 'sf'))
}
)

#' @name as
#' @return sf
#' @aliases as,SoilProfileCollection-method
#' @docType methods
#' @rdname coercion-methods
setAs("SoilProfileCollection", "sf", 
  function(from) {

    if (!requireNamespace("sf")) {
      stop("Package 'sf' is required to coerce a SoilProfileCollection to sf object", call. = FALSE)
    }
    
    crd <- metadata(from)$coordinates
    prj <- metadata(from)$crs
    
    if (all(is.null(crd) | is.na(crd)) || length(crd) < 2) {
      stop("Two coordinate (X, Y) column names must be defined to coerce a SoilProfileCollection to sf object", call. = FALSE)
    }
    
    if (is.null(prj)) {
      prj <- NA_character_
    }
    
    # keep empty point geometries, and do not remove original columns
    s <- sf::st_as_sf(site(from), crs = prj, coords = crd, na.fail =  FALSE, remove = FALSE)
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
  
    if (!requireNamespace("sf")) {
      stop("Package 'sf' is required to coerce a SoilProfileCollection to sf object", call. = FALSE)
    }
  
    sf::as_Spatial(sf::st_as_sfc(as(from, 'sf')))
  }
)

#' @param x a SoilProfileCollection
#' @export 
#' @rdname coercion-methods
setMethod("as.data.frame", "SoilProfileCollection", function(x) {
  as(x, 'data.frame')
})
