## initialisation of objects
##

"SoilProfile" <- function(
  id = as.character(NA),
  depths = matrix(nrow = 0, ncol = 2, dimnames = list(NULL, c('top', 'bottom'))),
  depth_units = 'cm',
  sp = new('SpatialPoints'),
  horizons = data.frame(),
  site = data.frame()
) {
  new(
    "SoilProfile", 
    id = id, 
    depths = depths, 
    depth_units = depth_units,
    sp = sp,
    horizons = horizons,
    site = site
  )
}

"SPC" <- function(
  ...,
  profiles = list(SoilProfile())
){
  
  dots <- list(...)
  
  # if some of the dots are giving a SoilProfile
  if (any(sapply(dots, inherits, "SoilProfile"))) {
    # Select SoilProfile objects
    idx_sprof <- which(sapply(dots, inherits, "SoilProfile"))    
    sprofs <- dots[[idx_sprof]]
    
    # Retrieve IDs of the listed SoilProfile
    ids <- do.call("c", lapply(sprofs, function(x) x@id))
    
    # Testing unicity of IDs
    if (any(duplicated(ids))) {
      warning("Duplicated SoilProfile IDs. Re-building IDs from scratch.")
      ids <- as.character(1:length(ids))
      names(sprofs) <- ids
    }
    
    # Building object from results
    obj <- new("SPC", profiles = sprofs)
    
  } else {
    obj <- new("SPC", profiles = profiles)
  }
  
  return(obj)
}

## Print and summary

setMethod(
  f = 'show',
  signature = 'SoilProfile',
  definition = function(object){
    cat(paste("Object of class ", class(object), "\n", sep = ""))
    cat("Soil depth range: ", min(object),"-", max(object), " ", depth_units(object), "\n", sep = "")
    cat(ncol(horizons(object))," horizon attributes, ", ncol(site(object))," site attributes\n", sep = "")
  }
)

setMethod(
  f = 'show',
  signature = 'SPC',
  definition = function(object){
    cat(paste("Object of class ", class(object), "\n", sep = ""))
    cat("Collection of ", length(object)," soil profiles\n", sep='')
    cat("Soil depth range: ", min(object),"-", max(object), " ", depth_units(object), "\n", sep = "")
    cat(ncol(horizons(object))," horizon attributes, ", ncol(site(object))," site attributes\n", sep = "")
  }
)

summary.SoilProfile <- function (object, ...){
  obj <- list()
  obj[["class"]] <- class(object)
  obj[["depth_range"]] <- c(min(object), max(object))
  obj[["depth_units"]] <- depth_units(object)
  
  #   sp_data <- ncol(coordinates(object@sp)) >= 2
  n_hz_data <- ncol(horizons(object))
  n_site_data <- ncol(site(object))
  
  if (n_hz_data > 1) {
    obj[["hz_data"]] <- summary(horizons(object))
  } else obj[["hz_data"]] <- NULL
  
  if (n_site_data > 1) {
    obj[["site_data"]] <- site(object)
  } else obj[["site_data"]] <- NULL
  
  class(obj) = "summary.SoilProfile"
  obj
}

summary.SPC <- function (object, ...){
  obj <- list()
  obj[["class"]] <- class(object)
  obj[["depth_units"]] <- depth_units(object)
  obj[["depth_range"]] <- c(min(object), max(object))
  obj[["length"]] <- length(object)
  
  #   sp_data <- ncol(coordinates(object@sp)) >= 2
  n_hz_data <- ncol(horizons(object))
  n_site_data <- ncol(site(object))
  
  if (n_hz_data > 1) {
    obj[["hz_data"]] <- summary(horizons(object))
  } else obj[["hz_data"]] <- NULL
  
  if (n_site_data > 1) {
    obj[["site_data"]] <- summary(site(object))
  } else obj[["site_data"]] <- NULL
  
  class(obj) = "summary.SPC"
  obj
}

print.summary.SoilProfile = function(x, ...) {
  cat(paste("Object of class ", x[["class"]], "\n", sep = ""))
  cat("Soil depth range: ", x[["depth_range"]][1], "-", x[["depth_range"]][2], " ", x[["depth_units"]], "\n", sep = "")
  
  cat("\n")
  
  if (!is.null(x[["hz_data"]])) {
    cat(ncol(x[["hz_data"]]), " horizon attributes:\n")
    print(x[["hz_data"]])
    cat("\n")
  } else {
    cat("No horizon attributes.\n")
  }
  
  if (!is.null(x[["site_data"]])) {
    cat(ncol(x[["site_data"]]), " horizon attributes:\n")
    print(x[["site_data"]])
    cat("\n")
  } else {
    cat("No site attributes.\n")
  }
  
  invisible(x)
}

print.summary.SPC = function(x, ...) {
  cat(paste("Object of class ", x[["class"]], "\n", sep = ""))
  cat("Collection of ", x[["length"]]," soil profiles\n", sep='')
  cat("Soil depth range: ", x[["depth_range"]][1], "-", x[["depth_range"]][2], " ", x[["depth_units"]], "\n", sep = "")
  
  cat("\n")
  
  if (!is.null(x[["hz_data"]])) {
    cat(ncol(x[["hz_data"]]), " horizon attributes:\n")
    print(x[["hz_data"]])
    cat("\n")
  } else {
    cat("No horizon attributes.\n")
  }
  
  if (!is.null(x[["site_data"]])) {
    cat(ncol(x[["site_data"]]), " horizon attributes:\n")
    print(x[["site_data"]])
    cat("\n")
  } else {
    cat("No site attributes.\n")
  }
  
  invisible(x)
}

