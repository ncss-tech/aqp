# Replacement methods for SoilProfile and SoilProfileCollection classes
#

## Parsing formula interface
##

.parse_formula <- function(formula, object){
  formula <- str_c(deparse(formula, 500), collapse = "")
  
  elements <- str_split(formula, fixed("~"))[[1]]
  n_elements <- length(elements)
  length_elements <- lapply(elements, str_length)
  elements <- lapply(str_split(elements, "[+*]"), str_trim)
  
  # Managing dots
  detect_dots <- lapply(elements, function(x) any(x == "..."))
  idx_dots <- which(unlist(detect_dots))
  
  # If `...` is used
  if (length(idx_dots) > 0 ) {
    
    # Sanity checks
    if (idx_dots %in% c(1, 2)) {
      stop("Wrong formula.\nYou can't use '...' to set the ID or the depths of a SoilProfileCollection object.\nRefer to the documentation for more details.", call. = FALSE)
    }
  
    if (length(idx_dots) > 1) {
      stop("Wrong formula.\nYou can't use '...' more than once. \nRefer to the documentation for more details.", call. = FALSE)
    } else if (length(idx_dots) == 1) {
      # All variables
      nm_df <- names(object)
      # Variables used in other part of the formula
      nm_call <- unlist(elements[-idx_dots])
      # Replace dots by the remaining var names
      elements[[idx_dots]] <- setdiff(nm_df, nm_call)
    }
  }
  
  # Replacing any void elemnt by NULL
  elements <- lapply(elements, function(x) 
    if(any(lapply(x, str_length) == 0)) x = NULL
    else x
  )
  
  #   if (n_elements == 1) {
  #     cols_id <- NULL
  #     cols_depths <- elements[[1]]
  #     cols_hz <- NULL
  #   }
  #   else
  if (n_elements == 2) {
    cols_id <- elements[[1]]
    cols_depths <- elements[[2]]
    cols_hz <- cols_site <- NULL
  }
  else if (n_elements == 3) {
    cols_id <- elements[[1]]
    cols_depths <- elements[[2]]
    cols_hz <- elements[[3]]
    cols_site <- NULL
  }
  else if (n_elements == 4) {
    cols_id <- elements[[1]]
    cols_depths <- elements[[2]]
    cols_hz <- elements[[3]]
    cols_site <- elements[[4]]
  }
  else {
    stop('Wrong formula.\nThere is a problem in the number of elements in your formula.\nRefer to the documentation for more details.', call. = FALSE)
  }
  
  # Sanity check: at this point ID can't be NULL
  if (is.null(cols_id)) stop('Wrong formula.\nYou need to specify an ID column for the SoilProfileCollection.\nRefer to the documentation for more details.', call. = FALSE)
  
  list(id = cols_id, depths = cols_depths, horizons = cols_hz, site = cols_site)
}

## depths<- setter method - to create SP/SoilProfileCollection objects
## 
## 1. puts the rest of stuff in @horizons
## depths(foo) <- id ~ top + bottom 
##
## 2. puts **only** x and y in @horizons and discards the rest
## depths(foo) <- id ~ top + bottom ~ x + y 
##
## 3. puts **only** x and y in @horizons, z in @site, and discards the rest
## depths(foo) <- id ~ top + bottom ~ x + y ~ z
##
## 4. (**not implemented**) puts **only** x and y in @site, and the rest in @horizons
## depths(foo) <- id ~ top + bottom ~ ... ~ x + y
##
if (!isGeneric('depths<-'))
  setGeneric('depths<-', function(object, value) standardGeneric('depths<-'))

## Method that creates SoilProfileCollection from a data.frame
##
setReplaceMethod("depths", "data.frame",
                 function(object, value) {      
                   # Retrieve variables names
                   nm.vars <- .parse_formula(value, object)
                   
                   # If no horizon data has been given we put all the leftover data
                   if (is.null(nm.vars$horizons)) {
                     nm.vars$horizons <- names(object)[! names(object) %in% c(nm.vars$id, nm.vars$depths, nm.vars$site)]
                   }
                   
                   lst_df <- dlply(object, nm.vars$id, identity)
                   
                   # Create list of SoilProfile
                   lst_sp <- llply(lst_df, function(x) {
                     
                     cur_id <- as.character(unique(x[[nm.vars$id]]))
                     names(cur_id) <- nm.vars$id
                     depths <- as.matrix(x[, nm.vars$depths])
                     sp <- new('SpatialPoints')
                     horizons <- x[, nm.vars$horizons, drop = FALSE]
                     
                     if(is.null(nm.vars$site)) site <- data.frame()
                     else site <- x[, nm.vars$site, drop = FALSE]
                     
                     new("SoilProfile", id = cur_id, depths = depths, sp = sp, horizons = horizons, site = site)
                   })
                   
                   SoilProfileCollection(profiles = lst_sp)
                 }
)

# Site data
#

if (!isGeneric('site<-'))
  setGeneric('site<-', function(object, ...) 
    standardGeneric('site<-'))

setReplaceMethod("site", "SoilProfile",
                 function(object, value) {
                   
                   # creation of site data from horizon data
                   if (inherits(value, "formula")) {
                     mf <- model.frame(value, horizons(object), na.action = na.pass)
                     new_site <- unique(mf)
                   }
                   
                   # Throw error if more than one line
                   if (nrow(new_site) > 1) stop('site data error')
                   
                   # Affect data to site slot
                   object@site <- new_site
                   
                   # Remove data from horizons slot
                   object@horizons <- object@horizons[, -1 * which(names(object@horizons) %in% names(new_site)), drop = FALSE]
                   
                   return(object)
                 }
)

setReplaceMethod("site", "SoilProfileCollection",
                 function(object, value) {
                   lspc <- lapply(profiles(object), function(x) {
                     site(x) <- value
                     x
                   })
                   SoilProfileCollection(profiles = lspc)
                 }
)