setGeneric("prj", function(object, ...)
  standardGeneric("prj"))

#' Get or Set Coordinate Reference System for SoilProfileCollection
#'
#' @description `prj()`: Get Coordinate Reference System (Projection) metadata
#' @param object A SoilProfileCollection
#' @param value character. Representation of Coordinate Reference System as `"authority:code"`, integer EPSG code, WKT2019 / PROJ4 string, an sf `crs` or sp `CRS` object.
#' @param ... Additional arguments (not used)
#' @param obj A SoilProfileCollection
#' @export
#' @seealso [initSpatial<-()]
#' @rdname SoilProfileCollection-crs
#' @aliases prj
setMethod("prj", 'SoilProfileCollection',
          function(object, ...) {
            value <- metadata(object)$crs
            if (length(value) == 0 || (!is.na(value) && nchar(value) == 0)) {
              value <- NA_character_
            }
            return(value)
          }
)

setGeneric("prj<-", function(object, ..., value)
  standardGeneric("prj<-"))

#' @description `prj()<-`: Set Coordinate Reference System metadata for the SoilProfileCollection
#' @aliases prj<-
#' @rdname SoilProfileCollection-crs
#' @export
setReplaceMethod("prj", 'SoilProfileCollection',
                 function(object, ..., value) {
                   
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
                   metadata(object)$crs <- value
                   return(object)
                 }
)

#' @description `proj4string()`: (Deprecated) Get Coordinate Reference System as PROJ4 String
#' @rdname SoilProfileCollection-crs
#' @export
setMethod("proj4string", signature(obj = 'SoilProfileCollection'),
          function(obj) {
            .Deprecated("prj", package = "aqp", 
                        msg = "Methods based on sp class definitions have been deprecated and will be removed by October 1, 2023. See ?prj")
            return(prj(obj))
          }
)

#' @description `proj4string()<-`: (Deprecated) Set Coordinate Reference System metadata for the SoilProfileCollection
#' @rdname SoilProfileCollection-crs
setReplaceMethod("proj4string", signature(obj = 'SoilProfileCollection'),
                 function(obj, value) {
                   .Deprecated("prj<-", package = "aqp", 
                               msg = "Methods based on sp class definitions have been deprecated and will be removed by October 1, 2023. See ?`prj<-`")
                   prj(obj) <- value
                   return(obj)
                 })
    
setGeneric("initSpatial<-", function(object, crs = NULL, value)
  standardGeneric("initSpatial<-"))

#' Initialize Spatial Data in a SoilProfileCollection 
#' 
#' `initSpatial()<-`: Set the column names containing spatial data and the corresponding coordinate reference system for a SoilProfileCollection.
#' 
#' @param object A SoilProfileCollection
#' @param value A formula specifying names of columns containing geometry (x and y coordinates), or character with the column names
#' @param crs Optional: character. Representation of Coordinate Reference System as `"authority:code"`, integer EPSG code, WKT2019 or PROJ4 string, an sf `crs` or sp `CRS` object.
#' @rdname initSpatial
#' @name initSpatial<-
#' @aliases initSpatial<-,SoilProfileCollection,ANY,ANY-method
#' @export
#' @seealso [prj()]
#' @examples
#'
#' data(sp5)
#'
#' # coordinates are stored in x and y column of site
#' sp5$x <- rnorm(length(sp5))
#' sp5$y <- rnorm(length(sp5))
#'
#' # coordinates takes a formula object as input
#' initSpatial(sp5) <- ~ x + y
#' 
#' # optionally specify Coordinate Reference System (crs) on left-hand side
#' initSpatial(sp5, crs = "OGC:CRS84") <- ~ x + y
setReplaceMethod("initSpatial", signature(object = "SoilProfileCollection", value = "ANY"),
                 function(object, crs = NULL, value) {
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
                   
                   # set CRS if specified
                   if (!is.null(crs)) {
                     prj(object) <- crs
                   }
                   
                   return(object)
                 })

#' @rdname initSpatial
#' @export
setReplaceMethod("initSpatial", signature(object = "SoilProfileCollection", value = "character"),
                 function(object, crs = NULL, value) {
                   initSpatial(object, crs = crs) <- as.formula(paste0("~", paste0(value, collapse = "+")))
                   object
                  })


setGeneric("getSpatial", function(object, ...)
  standardGeneric("getSpatial"))

#' Get Soil Profile Spatial Data
#'
#' @description `getSpatial()`: Get spatial data associated with a SoilProfileCollection
#'
#' @param object A SoilProfileCollection
#' @docType methods
#' 
#' @export
#' @aliases getSpatial
#' @rdname initSpatial
setMethod("getSpatial", signature(object = "SoilProfileCollection"),
          function(object) {
            cn <- metadata(object)$coordinates
            
            # if not specified or not valid, return a 0x2 matrix
            if (length(cn) == 0 || !validSpatialData(object))
              return(matrix(numeric(0), ncol = 2, dimnames = list(NULL, c("x", "y"))))
            
            return(as.matrix(sapply(cn, function(x) object[[x]])))
          })

#' @param obj A SoilProfileCollection
#' @export
#' @rdname initSpatial
#' @aliases coordinates
setMethod("coordinates", signature(obj = "SoilProfileCollection"),
          function(obj) {
            .Deprecated("initSpatial<-", package = "aqp", 
                        msg = "Methods based on sp class definitions have been deprecated and will be removed by October 1, 2023. See ?getSpatial")
            getSpatial(obj)
          })

#' @rdname initSpatial
#' @export
#' @aliases coordinates<-
setReplaceMethod("coordinates", c("SoilProfileCollection", "ANY"),
                 function(object, value) {
                   .Deprecated("initSpatial<-", package = "aqp", 
                               msg = "Methods based on sp class definitions have been deprecated and will be removed by October 1, 2023. See ?`initSpatial<-`")
                   initSpatial(object) <- value
                   object
                 })

#' @rdname initSpatial
#' @export
setReplaceMethod("coordinates", c("SoilProfileCollection", "character"),
                function(object, value) {
                  .Deprecated("initSpatial<-", package = "aqp", 
                              msg = "Methods based on sp class definitions have been deprecated and will be removed by October 1, 2023. See ?`initSpatial<-`")
                  initSpatial(object) <-  as.formula(paste0("~", paste0(value, collapse = "+")))
                  object
                })
