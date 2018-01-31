# Replacement methods for SoilProfile and SPC classes
#

## Parsing formula interface
##

.parse_formula <- function(formula, object){
  formula <- str_c(deparse(formula, 500), collapse = "")
  
  elements <- str_split(formula, fixed("~"))[[1]]
  n_elements <- length(elements)
  length_elements <- lapply(elements, str_length)
  elements <- lapply(str_split(elements, "[+*]"), str_trim)
  
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
    stop('wrong formula.')
  }
  
  # Sanity check: at this point ID can't be NULL
  if (is.null(cols_id)) stop('wrong formula')
  
  list(id = cols_id, depths = cols_depths, horizons = cols_hz, site = cols_site)
}

## depths<- setter method - to create SP/SPC objects
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

## Method that creates SPC from a data.frame
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
                   
                   SPC(profiles = lst_sp)
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

setReplaceMethod("site", "SPC",
                 function(object, value) {
                   lspc <- lapply(profiles(object), function(x) {
                     site(x) <- value
                     x
                   })
                   SPC(profiles = lspc)
                 }
)