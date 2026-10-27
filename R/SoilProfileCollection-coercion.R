## Coercion methods: general

#' Coerce `SoilProfileCollection` with `as()`
#' 
#' @description `SoilProfileCollection` objects can be coerced to other R object types using `as(spc, 'type')`.
#' 
#' Possible endpoints include: `list`, `data.frame`, `SpatialPointsDataFrame` and `SpatialPoints`.
#'
#' @return list
#' @docType methods
#' @rdname coercion-methods
#' @param from `SoilProfileCollection`
#' @param to target class
#' @param strict (not used)
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
#' initSpatial(sp5, crs = "OGC:CRS84") <- ~ x + y
#' 
#' if (requireNamespace("sf")) {
#' 
#'   # sf output
#'   str(as(sp5, 'sf'))
#' 
#'   # SpatialPointsDataFrame output
#'   str(as(sp5, 'SpatialPointsDataFrame'))
#' 
#'   # SpatialPoints output
#'   str(as(sp5, 'SpatialPoints'))
#'   
#' }
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


#' @return data.frame
#' @docType methods
#' @rdname coercion-methods
#'
setAs("SoilProfileCollection", "data.frame", function(from) {
  s <- getSpatial(from)
  
  # horizons + site + coordinates
  if(nrow(site(from)) > 0 & nrow(s) == length(from)) {
    site.coords <- data.frame(site(from), s, stringsAsFactors = FALSE)
    return(merge(horizons(from), site.coords, by = idname(from), sort = FALSE, all.x = TRUE))
  }
  
  # horizons + site
  if(nrow(site(from)) > 0 & ! nrow(s) == length(from))
    return(merge(horizons(from), site(from), by = idname(from), sort = FALSE, all.x = TRUE))
    
  # horizons + coordinates
  if(! nrow(site(from)) > 0 & nrow(s) == length(from)) {
    ids.coords <- data.frame(profile_id(from), s, stringsAsFactors = FALSE)
    return(data.frame(horizons(from), ids.coords, stringsAsFactors = FALSE))
  }
  
  # just horizons
  else {
    return(horizons(from))
  }
})


#' @return tbl_df
#' @docType methods
#' @rdname coercion-methods
#'
setAs("SoilProfileCollection", "tbl_df", function(from) {
  .as.data.frame.aqp(as(from,'data.frame'), 'tbl_df')
})

#' @return data.table
#' @docType methods
#' @rdname coercion-methods
#'
setAs("SoilProfileCollection", "data.table", function(from) {
  .as.data.frame.aqp(as(from, 'data.frame'), 'data.table')
})


#' @return SpatialPointsDataFrame
#' @docType methods
#' @rdname coercion-methods
setAs("SoilProfileCollection", "SpatialPointsDataFrame", function(from) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required to coerce a SoilProfileCollection to SpatialPointsDataFrame object", call. = FALSE)
  }
  sf::as_Spatial(as(from, 'sf'))
}
)

#' @return sf
#' @docType methods
#' @rdname coercion-methods
setAs("SoilProfileCollection", "sf", 
  function(from) {

    if (!requireNamespace("sf", quietly = TRUE)) {
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


#' @return SpatialPoints
#' @docType methods
#' @rdname coercion-methods
setAs("SoilProfileCollection", "SpatialPoints", function(from) {
  
    if (!requireNamespace("sf", quietly = TRUE)) {
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
