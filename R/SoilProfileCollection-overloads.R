##
## overloads
##

## TODO: rbind
## TODO: overload unique() via digest eval of unique profiles
## TODO: SetReplaceMethod("$", SoilProfileCollection)
## TODO: subsetProfile
## TODO: SetMethod("[", SoilProfileCollection)

# Number of profiles in the collection
length.SoilProfileCollection <- function(x) length(profiles(x))

# Number of horizons
setMethod("nrow", "SoilProfile",
          function(x) {
            nrow(x@depths)
          }
)

setMethod("nrow", "SoilProfileCollection",
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

setMethod("min", "SoilProfileCollection",
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

setMethod("max", "SoilProfileCollection",
          function(x, na.rm = TRUE) {
            max(laply(profiles(x), max))
          }
)

# Concatenation of objects

# TODO Check ID and Top/Bottom colnames consistency 
# TODO Add methods for SoilProfile (concatenating SP = SPC)

rbind.SoilProfileCollection <- function(...) {
  lsp <- unlist(lapply(list(...), profiles))
  SoilProfileCollection(profiles = lsp)
}

c.SoilProfileCollection <- function(...) {
  rbind.SoilProfileCollection(...)
}

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
  signature = "SoilProfileCollection",
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

# Function that returns the index of each profile in the
# data.frame of all profiles together
.get_profiles_idx_in_dataframe <- function(spc) {
  to <- cumsum(unlist(lapply(profiles(spc), nrow)))
  from <- to - unlist(lapply(profiles(spc), nrow)) + 1
  res <- data.frame(from = from, to = to)
  data.frame(id = row.names(res), from = from, to = to, row.names = NULL)
}

## problem: when making new columns how  can the function determine where to insert the replacement>?
setReplaceMethod("$", "SoilProfile",
                 function(x, name, value) {
                   
                   # If the column name is a depth name or id name,
                   # we throw a warning
                   if (name %in% depthNames(x)) {
                     warning("you can't change the column name of the depths", call. = FALSE)
                   } else if (name %in% idname(x)) {
                     warning("you can't change the column name of the ids", call. = FALSE)
                    } else if (name %in% horizonNames(x)) {
                     # modifying existing horizon data
                     x@horizons[[name]] <- value
                   } else if (name %in% siteNames(x)) {
                     # modifying existing site data
                     x@site[[name]] <- value
                   } else {
                     # creation of a new column: use length of replacement 
                     # to determing: horizon / site
                     nhz <- nrow(x)
                     
                     # note: special case when only 1 hz: we create horizon column
                     # by default
                     if ( (length(value) == nhz) | (nhz == 1) ) {
                       x@horizons[[name]] <- value
                     } else if (length(value) == 1) {
                       x@site[[name]] <- value
                     } else {
                       stop('dimensions mismatch', call. = FALSE)
                     }
                   }
                   
                   return(x)
                 }
)

# setReplaceMethod("$", "SoilProfileCollection",
#                  function(x, name, value) {
#                    # Convert object to list of SoilProfiles
#                    lspc <- profiles(x)
#                    
#                    # Test on length of vector
#                    # to see whether it is a hz or site attribute
#                    if (length(value) == nrow(x)) {
#                      # this is a hz attribute
#                      l_value <- dlply(
#                        .get_profiles_idx_in_dataframe(x), .(id), 
#                        function(x) value[x$from: x$to]
#                       )
#                    } else if (length(value) == length(x)) {
#                      # this is a site attribute
#                      l_value <- llply(1:length(x), function(x) value[x])
#                    } else {
#                      stop('dimension mitsmatch')
#                    }
#                    browser()
#                    lspc <- lapply(
#                      1:length(lspc), 
#                      function(i) {
#                        sp <- lspc[[i]]
#                        sp$name <- l_value[[i]]
#                        horizonNames(sp) %>% print
#                        lspc[[i]] <- sp
#                      }
#                   )
#                    SoilProfileCollection(profiles = lspc)
#                  }
# )

## matrix / DF style access: only to horizon data
##
## i = profile index
## j = horizon / slice index
##
setMethod("[", "SoilProfile",
          function(x, i, j, ...) {
            # In this case i is always one
            # Now subsetting horizon data using j
            
            if(!missing(j) & !is.null(j)) {
              
              j <- as.integer(j)
              
              if(any(is.na(j))) {
                stop('NA not permitted in horizon/slice index', call.=FALSE)
              }
              
            } else {
              j <- 1:nrow(x@horizons)
            }
            
            # Subset horizons in the profiles
            SoilProfile(id = x@id,
                  depths = x@depths[j,, drop = FALSE],
                  depth_units = depth_units(x),
                  sp = x@sp,
                  horizons = x@horizons[j,, drop = FALSE],
                  site = x@site
            )
          }
)

setMethod("[", "SoilProfileCollection",
        function(x, i, j, ...) {
            
          # check for missing i and j
          if(missing(i) & missing(j)) {
            stop('must provide either a profile index or horizon/slice index, or both', call.=FALSE)
          }
          
          # convert to integer
          if(!missing(i)) {
            
            if(any(is.na(i))) {
              stop('NA not permitted in profile index', call.=FALSE)
            }
              
            # convert logical to integer per standard vector/list
            # indexing rules (thanks Jos? Padarian for the suggestion!)
            if(is.logical(i)) {
              i <- (1:length(x))[i]
            }
            
            i <- as.integer(i)
            
          } else { # if no index is provided, the user wants all profiles
            i <- 1:length(x)
          }
          
          # Subset profiles 
          spc <- profiles(x)[i]
          
          # Now subsetting horizon data using j
          if(!missing(j)) {
            
            j <- as.integer(j)
            
            if(any(is.na(j))) {
              stop('NA not permitted in horizon/slice index', call.=FALSE)
            }
            
          } else {
            j <- NULL
          }
          
          # Subset horizons in the profiles
          lspc <- lapply(spc, function(s) s[, j])
          
          SoilProfileCollection(profiles = lspc)
          
        }
      )

setMethod("[[", "SoilProfileCollection",
          function(x, i, ...) {
            
            # check for missing i
            if(missing(i)) {
              stop('must provide either a profile index or name', call.=FALSE)
            }
            
            # convert to integer
            if(!missing(i)) {
              
              if(any(is.na(i))) {
                stop('NA not permitted in profile index', call.=FALSE)
              }
              # 
              # convert logical to integer per standard vector/list
              # indexing rules (thanks Jos? Padarian for the suggestion!)
              if(is.logical(i)) {
                i <- (1:length(x))[i]
              }
              
              # convert character to integer
              if (is.character(i)) {
                i <- which(profile_id(x) %in% i)
              }
              
              i <- as.integer(i)
              
            } else { # if no index is provided, the user wants all profiles
              i <- 1:length(x)
            }
            
            # Subset profiles 
            SoilProfileCollection(profiles = profiles(x)[i])
            
          }
)

