# SoilProfileCollection metadata

# this method throws an error if required metadata aren't present
.require.metadata.aqp <- function(object, attr, attrlabel, message, required = FALSE) {
  xx <- metadata(object)[[attr]]
  if (length(xx) == 0 || nchar(xx) == 0) {
    if (required) {
      stop(attrlabel, " (Metadata element: '", attr, "') is not specified for this SoilProfileCollection. It is required.", message,
           call. = FALSE)
    } else {
      xx <- ""
    }
  }
  xx
}

# this method handles setting of user metadata
#  key features:
#   - NA, "", and NULL `value` are treated the same
#   - If `value` does not exist in horizonNames()
#   - allowed names is either "site" "horizon" or NULL to allow options:
#       siteNames(), horizonNames() and no limit 
#       
.set.metadata.aqp <- function(object, value, required = FALSE,
                              attr, attrlabel, message = "", 
                              allowednames = NULL) {
  # test: does it exist?
  if (length(value) == 0) {
    value <- ""
  }
  
  if(length(value) > 0) {
    # several ways to "reset" the metadata to the safe but invalid value
    if((value == "") | is.na(value) | is.null(value)) {
      value <- ""
    } else if (!is.null(allowednames)) {
      namegrp <- switch(tolower(allowednames), 
                        "site" = siteNames(object),
                        "horizon" = horizonNames(object), NULL)
      
      if (!is.null(namegrp) && !(value %in% namegrp)) {
        stop(paste0(attrlabel, " (", value, ") not in ", allowednames, " names."), call. = FALSE)
      }
    }
  }
  
  # replace
  metadata(object)[[attr]] <- value
  
  # check if required
  .require.metadata.aqp(object,
                        attr = attr,
                        attrlabel = attrlabel,
                        message = message,
                        required = required)
  
  # done
  return(object)
}

# takes two SPC as input, takes metadata other than original order from source
# returns the destination SPC with modified metadata
.transfer.metadata.aqp <- function(src, dest) {
  if (inherits(src, 'SoilProfileCollection')) {
    m <- metadata(src)
  } else {
    m <- src
  }
  stopifnot(inherits(dest, 'SoilProfileCollection'))
  
  # transfer attributes https://github.com/ncss-tech/aqp/issues/204
  customattr <- attributes(src)
  customattr <- customattr[!names(customattr) %in% names(attributes(SoilProfileCollection()))]
  attributes(dest)[names(customattr)] <- attributes(src)[names(customattr)]
  
  cols <- names(m)[names(m) != "original.order"]
  metadata(dest)[cols] <- m[cols]
  dest
}

## get column containing horizon designations (there is a setter of same name)

setGeneric("hzdesgnname", function(object, ..., required = FALSE)
  standardGeneric("hzdesgnname"))

#' @title Get or Set Horizon Designation Column Name
#' @name hzdesgnname
#' @aliases hzdesgnname,SoilProfileCollection-method
#' @details Store the column name containing horizon designations or other identifiers in the metadata slot of the SoilProfileCollection.
#' @description `hzdesgnname()`: Get column name containing horizon designations 
#' @param object a SoilProfileCollection
#' @param required logical, is this attribute required? If it is, set to `TRUE` to trigger error on invalid result
#' @docType methods
#' @rdname hzdesgnname
setMethod("hzdesgnname", signature(object = "SoilProfileCollection"),
          function(object, required = FALSE) {
            .require.metadata.aqp(object,
                                  attr = "aqp_hzdesgn",
                                  attrlabel = "Horizon designation",
                                  message ="\nSee ??hzdesgnname",
                                  required = required)
          })

setGeneric('hzdesgnname<-', function(object, value, required = FALSE)
  standardGeneric('hzdesgnname<-'))

#' @description `hzdesgnname<-`: Set horizon designation column name
#' @name hzdesgnname<-
#' @aliases hzdesgnname<-,SoilProfileCollection-method
#' @param object a SoilProfileCollection
#' @param value character, name of column containing horizon designations
#' @param required logical, is this attribute required? If it is, set to `TRUE` to trigger error on invalid `value`.
#' @docType methods
#' @seealso [hzDesgn()]
#' @rdname hzdesgnname
#' @examples
#'
#' data(sp1)
#'
#' # promote to SPC
#' depths(sp1) <- id ~ top + bottom
#'
#' # set horizon designation column
#' hzdesgnname(sp1) <- "name"
#'
#' # get horizon designation column
#' hzdesgnname(sp1)
setReplaceMethod("hzdesgnname",
                 signature(object = "SoilProfileCollection"),
                 function(object, value, required = FALSE) {
                   .set.metadata.aqp(
                     object = object,
                     value = value,
                     required =  required,
                     attr = "aqp_hzdesgn",
                     attrlabel = "Horizon designation name",
                     message = "\nSee ??hzdesgnname",
                     allowednames = "horizon"
                   )
                 })


setGeneric("hzDesgn", function(object, ...)
  standardGeneric("hzDesgn"))

#' Get horizon designation column name
#'
#' @description Get horizon designation names
#'
#' @param object a SoilProfileCollection
#' @docType methods
#' @aliases hzDesgn
#' @rdname hzDesgn
setMethod("hzDesgn", signature(object = "SoilProfileCollection"),
          function(object) {
            
            h <- object@horizons
            hzd <- hzdesgnname(object)
            
            if (length(hzd)) {
              
              if (hzd %in% horizonNames(object)) {
                res <- h[[hzd]]
                return(res)
              }
              
            } else {
              
              stop("horizon designation name (",
                   hzd,
                   ") not in horizonNames().",
                   call. = FALSE)
            }
            
          })

## get column containing horizon designations
setGeneric("hztexclname", function(object, required = FALSE)
  standardGeneric("hztexclname"))

setGeneric('hztexclname<-', function(object, value, required = FALSE)
  standardGeneric('hztexclname<-'))

#' @title Get or Set Horizon Texture Class Column Name
#' @name hztexclname
#' @aliases hztexclname,SoilProfileCollection-method
#' @description `hztexclname()`: Get column name containing horizon designation name
#' @details Store the column name containing horizon texture classes or other identifiers in the metadata slot of the SoilProfileCollection.
#' @param object a SoilProfileCollection 
#' @param required logical, is this attribute required? If it is, set to `TRUE` to trigger error on invalid result
#' @docType methods
#' @rdname hztexclname
setMethod("hztexclname", signature(object = "SoilProfileCollection"),
          function(object, required = FALSE) {
            .require.metadata.aqp(object,
                                  attr = "aqp_hztexcl",
                                  attrlabel = "Horizon texture class",
                                  message = "\nSee ??hztexclname",
                                  required = required)
          })
#' @description `hztexclname<-`: Set horizon texture class column name for a SoilProfileCollection
#' @name hztexclname<-
#' @aliases hztexclname<-,SoilProfileCollection-method
#' @param object a SoilProfileCollection
#' @param value character, name of column containing horizon texture classes
#' @param required logical, is this attribute required? If it is, set to `TRUE` to trigger error on invalid `value`.
#' @docType methods
#' @rdname hztexclname
#' @examples
#'
#' data(sp1)
#'
#' # promote to SPC
#' depths(sp1) <- id ~ top + bottom
#'
#' # set horizon texture class column
#' hztexclname(sp1) <- "texture"
#'
#' # get horizon texture class column
#' hztexclname(sp1)
setReplaceMethod("hztexclname", signature(object = "SoilProfileCollection"),
                 function(object, value, required = FALSE) {
                   .set.metadata.aqp(
                     object = object,
                     value = value,
                     required =  required,
                     attr = "aqp_hztexcl",
                     attrlabel = "Horizon texture class name",
                     message = "\nSee ??hztexclname",
                     allowednames = "horizon"
                   )
                 })
