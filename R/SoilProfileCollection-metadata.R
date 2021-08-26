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


setGeneric('hzdesgnname<-', function(object, value, required = FALSE)
  standardGeneric('hzdesgnname<-'))

#' @title Get or Set Horizon Designation Column Name
#' @name hzdesgnname
#' @description Store the column name containing horizon designations or other identifiers in the metadata slot of the SoilProfileCollection.
#' `hzdesgnname<-`: Set horizon designation column name
#'
#' @param object a SoilProfileCollection
#' @param value character, name of column containing horizon designations
#' @param required logical, is this attribute required? If it is, set to `TRUE` to trigger error on invalid `value`.
#' @docType methods
#' @aliases hzdesgnname<-,SoilProfileCollection-method
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


setGeneric('hztexclname<-', function(object, value, required = FALSE)
  standardGeneric('hztexclname<-'))

#' Get or Set Horizon Texture Class Column Name
#' @name hztexclname
#'
#' @description `hztexclname<-`: Set horizon texture class column name for a SoilProfileCollection
#'
#' @param object a SoilProfileCollection
#' @param value character, name of column containing horizon texture classes
#' @param required logical, is this attribute required? If it is, set to `TRUE` to trigger error on invalid `value`.
#' 
#' @docType methods
#' @aliases hztexclname<-,SoilProfileCollection-method
#' @rdname hztexclname
#'
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
