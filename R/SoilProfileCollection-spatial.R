
##
## proj4string setting
##

#' Get or Set Coordinate Reference System for SoilProfileCollection
#' 
#' @description `proj4string()`: (Deprecated) Get Coordinate Reference System as PROJ4 String
#'
#' @param x A SoilProfileCollection
#' @param obj A SoilProfileCollection
#' @rdname SoilProfileCollection-crs
setMethod("proj4string", signature(obj = 'SoilProfileCollection'),
          function(obj) {
            .Deprecated("crs", package = "aqp")
            return(crs(obj))
          }
)

#' @description `wkt()`: (Deprecated) Get Coordinate Reference System as WKT String
#' @rdname SoilProfileCollection-crs
setMethod("wkt", signature(obj = 'SoilProfileCollection'),
          function(obj) {
            .Deprecated("crs", package = "aqp")
            return(crs(obj))
          }
)

setGeneric("crs", function(x, ...)
  standardGeneric("crs"))

#' @description `crs():` Get Coordinate Reference System metadata
#' @aliases crs
#' @rdname SoilProfileCollection-crs
setMethod("crs", 'SoilProfileCollection',
          function(x) {
            value <- metadata(x)$crs
            if (length(value) == 0 || (!is.na(value) && nchar(value) == 0)) {
              value <- NA_character_
            }
            return(value)
          }
)
#' @description `proj4string()<-`: Set Coordinate Reference System metadatafor the SoilProfileCollection
#'
#' @param value character. Representation of Coordinate Reference System as WKT or equivalent.
#' @rdname SoilProfileCollection-crs
setReplaceMethod("proj4string", signature(obj = 'SoilProfileCollection'),
                 function(obj, value) {
                   .Deprecated("crs", package = "aqp")
                   aqp::crs(obj) <- value
                   return(obj)
                 })

setGeneric("crs<-", function(x, value)
  standardGeneric("crs<-"))

#' @description `crs()<-`: Set Coordinate Reference System metadatafor the SoilProfileCollection
#' @aliases crs<-
#' @param value character. Representation of Coordinate Reference System as WKT or equivalent.
#' @rdname SoilProfileCollection-crs
#' @export
setReplaceMethod("crs", 'SoilProfileCollection',
  function(x, value) {
    
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
    
    # "crs" metadata stores the WKT string in the SPC
    metadata(x)$crs <- value
    return(x)
  }
)


##
## initialize spatial data
##
#' @param object A SoilProfileCollection
#' @param value A formula specifying columns containing x and y coordinates, or character with the column names
#'
#' @rdname coordinates
#' @export
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
setReplaceMethod("coordinates", c("SoilProfileCollection", "formula"),
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
    metadata(object)$coordinates <- fterms
    return(object)
})

#' @rdname coordinates
#' @export
setReplaceMethod("coordinates", c("SoilProfileCollection", "character"),
  function(object, value) {
    coordinates(object) <- as.formula(paste0("~", paste0(value, collapse = "+")))
    return(object)
})
