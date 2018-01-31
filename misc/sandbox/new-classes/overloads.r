## Various overloads
##

# Number of profiles in the collection
length.SPC <- function(x) length(profiles(x))

# Number of horizons
setMethod("nrow", "SoilProfile",
          function(x) {
            nrow(x@depths)
          }
)

setMethod("nrow", "SPC",
          function(x) {
            sum(laply(profiles(x), nrow))
          }
)

# Minimum depth
setMethod("min", "SoilProfile",
          function(x, na.rm = TRUE) {
            min(x@depths[, 1])
          }
)

setMethod("min", "SPC",
          function(x, na.rm = TRUE) {
            min(laply(profiles(x), min))
          }
)

# Maximum depth
setMethod("max", "SoilProfile",
          function(x, na.rm = TRUE) {
            max(x@depths[, 2])
          }
)

setMethod("max", "SPC",
          function(x, na.rm = TRUE) {
            max(laply(profiles(x), max))
          }
)

setMethod("names", "SoilProfile",
          function(x) {
            c(names(horizons(x)), names(site(x)))
          }
)

setMethod("names", "SPC",
          function(x) {
            c(names(horizons(x)), names(site(x)))
          }
)

## Attributes access

setMethod(
  f = "$", 
  signature = "SoilProfile",
  definition = function(x, name) {
    
    # get names from site and hz data
    s.names <- names(site(x))
    h.names <- names(horizons(x))
    
    # when site data are initialized from an external DF, it is possible that
    # there will be duplicate column names
    if(name %in% h.names & name %in% s.names)
      warning('column name is present in horizon and site data, extracting from horizon data only', call.=FALSE)
    
    # get column from horizon data
    if (name %in% h.names)
      res <- horizons(x)[[name]]
    
    # otherwise check site data
    else
      if (name %in% s.names)
        res <- site(x)[[name]]
    
    # if still missing return NULL
    else
      res <- NULL
    
    return(res)
  }
)

setMethod(
  f = "$", 
  signature = "SPC",
  definition = function(x, name) {
    
    # get names from site and hz data
    s.names <- names(site(x))
    h.names <- names(horizons(x))
    
    # when site data are initialized from an external DF, it is possible that
    # there will be duplicate column names
    if(name %in% h.names & name %in% s.names)
      warning('column name is present in horizon and site data, extracting from horizon data only', call.=FALSE)
    
    # get column from horizon data
    if (name %in% h.names)
      res <- horizons(x)[[name]]
    
    # otherwise check site data
    else
      if (name %in% s.names)
        res <- site(x)[[name]]
    
    # if still missing return NULL
    else
      res <- NULL
    
    return(res)
  }
)