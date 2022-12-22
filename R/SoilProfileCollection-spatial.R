
##
## proj4string setting
##

#' Get or Set Coordinate Reference System for SoilProfileCollection
#' 
#' @description `proj4string()`: Get Coordinate Reference System as PROJ4 String
#'
#' @param obj A SoilProfileCollection
#' @rdname SoilProfileCollection-crs
setMethod(f = 'proj4string', signature(obj = 'SoilProfileCollection'),
          function(obj) {
            .Deprecated("wkt", package = "aqp")
            return(wkt(obj))
          }
)

#' @description `wkt():` Get Coordinate Reference System as Well-Known Text
#'
#' @rdname SoilProfileCollection-crs
setMethod(f = 'wkt', signature(obj = 'SoilProfileCollection'),
          function(obj) {
            value <- metadata(obj)$projection
            if (length(value) == 0 || (!is.na(value) && nchar(value) == 0)) {
              value <- NA_character_
            }
            return(value)
          }
)
#' @description `proj4string()<-`: Set Coordinate Reference System string for the SoilProfileCollection
#'
#' @param value character. Representation of Coordinate Reference System as WKT or equivalent.
#' @rdname SoilProfileCollection-crs
setReplaceMethod("proj4string", signature(obj = 'SoilProfileCollection'),
                 function(obj, value) {
                   .Deprecated("wkt", package = "aqp")
                   wkt(obj) <- value
                   return(obj)
                 })

#' @description `wkt()<-`: Set Coordinate Reference System string for the SoilProfileCollection
#'
#' @param value character. Representation of Coordinate Reference System as WKT or equivalent.
#' @rdname SoilProfileCollection-crs
#' @export
setGeneric("wkt<-", function(obj, value)
  standardGeneric("wkt<-"))

#' @rdname SoilProfileCollection-crs
#' @export
setReplaceMethod("wkt", signature(obj = 'SoilProfileCollection'),
  function(obj, value) {
    
    # backward compatibility for sp::CRS object
    if (inherits(value, 'CRS')) {
      if (!is.null(attr(value, 'comment'))) {
        # use WKT2019 if available
        value <- attr(value, 'comment')
      } else {
        # otherwise use proj arg string
        value <- value@projargs
      }
    }
    
    # added compatibility for sf::crs object
    if (inherits(value, 'crs')) {
      if (!is.null(value$wkt)) {
        # use WKT2019 if available
        value <- value$wkt
      } else {
        # otherwise use input proj arg string
        value <- value$input
      }
    }
    
    # 0-length or empty character are equivalent to NA CRS
    if (length(value) == 0 || nchar(value) == 0) {
      value <- NA_character_
    }
    
    # "projection" metadata stores the WKT string in the SPC
    metadata(obj)$projection <- value
    return(obj)
  }
)


##
## initialize spatial data
##
#' @aliases coordinates<-,SoilProfileCollection-method
#' @param object A SoilProfileCollection
#' @param value A formula specifying columns containing x and y coordinates
#'
#' @rdname coordinates
#'
#' @examples
#'
#' data(sp5)
#'
#' # coordinates are stored in x and y column of site
#' sp5$x <- rnorm(length(sp5))
#' sp5$y <- rnorm(length(sp5))
#'
#' # coordinates takes a formula object as input
#' coordinates(sp5) <- ~ x + y
#'
setReplaceMethod("coordinates", "SoilProfileCollection",
  function(object, value) {
    # basic sanity check
    if (!inherits(value, "formula"))
      stop('invalid formula: ', quote(value), call. = FALSE)
    
    fterms <- attr(terms(value), "term.labels")
    
    if (all(fterms %in% siteNames(object))) {
      mf <- data.matrix(model.frame(value, site(object), na.action = na.pass))
    } else if (all(fterms %in% horizonNames(object))) {
      mf <- unique(data.matrix(model.frame(value, horizons(object), na.action = na.pass)))
    } else {
      stop("formula terms not found in site or horizon table: ",
           paste0(fterms[!fterms %in% names(object)], collapse = ","))
    }
    
    # make sure that "normalization" worked
    if (nrow(mf) != length(object)) {
      stop("coordinates in horizon data are not unique within site: ", quote(value), call. = FALSE)
    }
   
    # set coordinates metadata entry
    metadata(object)$coordinates <- fterms[1:2]
    return(object)
  }
)
