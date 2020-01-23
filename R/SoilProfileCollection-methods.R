## init
"SoilProfileCollection" <- function(
idcol='id',
depthcols=c('top','bottom'),
metadata=data.frame(stringsAsFactors=FALSE),
horizons,
site=data.frame(stringsAsFactors=FALSE),
sp=new('SpatialPoints'), # this is a bogus place-holder
diagnostic=data.frame(stringsAsFactors=FALSE),
restrictions=data.frame(stringsAsFactors=FALSE)
){
  # creation of the object (includes a validity check)
  new("SoilProfileCollection", idcol=idcol, depthcols=depthcols, metadata=metadata, horizons=horizons, site=site, sp=sp, diagnostic=diagnostic, restrictions=restrictions)
}



## show
setMethod(
  f='show',
  signature='SoilProfileCollection',
  definition=function(object) {
  	n.profiles <- length(object)
  	
    cat("Object of class ", class(object), "\n", sep = "")
    cat("Number of profiles: ", n.profiles, "\n", sep="")
  	
  	if(n.profiles > 1)
			cat("Depth range: ", min(object), "-", max(object), " ", depth_units(object), "\n", sep="")
		
  	cat("\nHorizon attributes:\n")
  	print(head(horizons(object)))

		# in the presence of site data
    if (nrow(site(object)) > 0) {
      cat("\nSampling site attributes:\n")
      print(head(site(object)))
    }

    # presence of spatial data
    if(nrow(coordinates(object)) == n.profiles) {
    	cat('\nSpatial Data:\n')
    	show(object@sp@bbox)
    	show(proj4string(object))
    }

  }
)



## summary





##
## accessors
##

## ID column name
if (!isGeneric("idname"))
    setGeneric("idname", function(object, ...) standardGeneric("idname"))

setMethod("idname", "SoilProfileCollection",
  function(object)
    return(object@idcol)
)

## horizon ID name
if (!isGeneric("hzidname"))
  setGeneric("hzidname", function(object, ...) standardGeneric("hzidname"))

setMethod("hzidname", "SoilProfileCollection",
          function(object)
            return(object@hzidcol)
)

## get horizon IDs
if (!isGeneric("hzID"))
  setGeneric("hzID", function(object, ...) standardGeneric("hzID"))

setMethod("hzID", "SoilProfileCollection",
          function(object) {
            h <- horizons(object)
            res <- h[[hzidname(object)]]
            return(res)
          }
            
)

## horizon designation name
if (!isGeneric("hzdesgnname"))
  setGeneric("hzdesgnname", function(object, ...) standardGeneric("hzdesgnname"))

## get column containing horizon designations (there is a setter of same name)
setMethod("hzdesgnname", "SoilProfileCollection",
          function(object)
            return(object@hzdesgncol)
)

## get horizon designations (no corresponding setter -- no need)
if (!isGeneric("hzDesgn"))
  setGeneric("hzDesgn", function(object, ...) standardGeneric("hzDesgn"))

setMethod("hzDesgn", "SoilProfileCollection",
          function(object) {
            h <- horizons(object)
            hzd <- hzdesgnname(object)
            if(length(hzd)) {
              if(hzd %in% horizonNames(object)) {
                res <- h[[hzd]]
                return(res)
              }
            } else {
              stop("horizon designation name (",hzd,") not in horizonNames().", call.=FALSE)
            }
          }
            
)

## horizon texture class name
if (!isGeneric("hztexclname"))
  setGeneric("hztexclname", function(object, ...) standardGeneric("hztexclname"))

## get column containing horizon designations (there is a setter of same name)
setMethod("hztexclname", "SoilProfileCollection",
          function(object)
            return(object@hztexclcol)
)


## distinct profile IDs
if (!isGeneric("profile_id"))
  setGeneric("profile_id", function(object, ...) standardGeneric("profile_id"))

## warning! this assumes that horizon data aren't re-shuffled
## will be fixed in aqp 2.0
setMethod("profile_id", "SoilProfileCollection",
  function(object)
    unique(as.character(horizons(object)[[idname(object)]]))
)


## horizon depth column names
if (!isGeneric("horizonDepths"))
    setGeneric("horizonDepths", function(object, ...) standardGeneric("horizonDepths"))

setMethod("horizonDepths", "SoilProfileCollection",
  function(object)
    return(object@depthcols)
)


## spatial data: coordinates
setMethod("coordinates", "SoilProfileCollection",
  function(obj) {
  return(coordinates(obj@sp))
  }
)


## site data
if (!isGeneric("site"))
  setGeneric("site", function(object, ...) standardGeneric("site"))

# retrieves the site data frame
setMethod("site", "SoilProfileCollection",
  function(object) {
  return(object@site)
  }
)

## diagnostic horizons: stored as a DF, must be join()-ed to other data via ID
## note: ordering may or may not be the same as in site data
if (!isGeneric("diagnostic_hz"))
  setGeneric("diagnostic_hz", function(object, ...) standardGeneric("diagnostic_hz"))

setMethod(f='diagnostic_hz', signature='SoilProfileCollection',
  function(object){
    return(object@diagnostic)
  }
)

## restrictions: stored as a DF, must be join()-ed to other data via ID
## note: ordering may or may not be the same as in site data
if (!isGeneric("restrictions"))
  setGeneric("restrictions", function(object, ...) standardGeneric("restrictions"))

setMethod(f='restrictions', signature='SoilProfileCollection',
  function(object){
    return(object@restrictions)
  }
)


## horizon data
# returns a data.frame with horizons data
if (!isGeneric("horizons"))
  setGeneric("horizons", function(object, ...) standardGeneric("horizons"))

setMethod(f='horizons', signature='SoilProfileCollection',
  function(object){
  return(object@horizons)
  }
)

## metadata
# returns a data.frame
if (!isGeneric("metadata"))
  setGeneric("metadata", function(object, ...) standardGeneric("metadata"))

setMethod(f='metadata', signature='SoilProfileCollection',
  function(object){
  return(object@metadata)
  }
)

## depth_units
# returns a data.frame
if (!isGeneric("depth_units"))
  setGeneric("depth_units", function(object, ...) standardGeneric("depth_units"))

setMethod(f='depth_units', signature='SoilProfileCollection',
  function(object){
	u <- as.character(aqp::metadata(object)[['depth_units']])
	  # give a warning if not defined
	if(u == '')
	  message('Note: depth depth_units have not yet been defined.')

	return(u)
  }
)


## TODO: strip-out idname
## get site column names
if (!isGeneric("siteNames"))
  setGeneric("siteNames", function(object, ...) standardGeneric("siteNames"))

setMethod("siteNames", "SoilProfileCollection",
          function(object) {
            res <- names(object@site)
            return(res)
          }
)

## TODO: strip-out idname
## get horizon column names
if (!isGeneric("horizonNames"))
  setGeneric("horizonNames", function(object, ...) standardGeneric("horizonNames"))

setMethod("horizonNames", "SoilProfileCollection",
          function(object) {
            res <- names(object@horizons)
            return(res)
          }
)


## TODO: this seems stupid, NULL would be much simpler to reason over...
## are the contents of @sp valid: n x 2 matrix?
## if not, then contents of @sp is an empty SpatialPoints object
if (!isGeneric("validSpatialData"))
  setGeneric("validSpatialData", function(object, ...) standardGeneric("validSpatialData"))

setMethod("validSpatialData", "SoilProfileCollection",
          function(object) {
            # n x 2 ---> valid / initialized coordinates
            # n x 1 ---> emtpy SP object
            res <- dim(coordinates(object))[[2]]
            
            if(res == 2)
              return(TRUE)
            else
              return(FALSE)
          }
)




##
## overloads
##




# return a concatenated vector of horizon + site names
# note that we strip out the ID column name from @site
setMethod("names", "SoilProfileCollection",
  function(x) {
  res <- c(horizons=horizonNames(x), site=siteNames(x)[-1])
  return(res)
  }
)




# overload min() to give us the min depth within a collection
setMethod(f='min', signature='SoilProfileCollection',
definition=function(x, v=NULL) {
  # get bottom depth column name
  hz_bottom_depths <- horizonDepths(x)[2]
  
  # optionally use a horizon-level property refine calculation
  if(!missing(v)) {
  	# combine bottom depths with IDs and variable
  	h <- horizons(x)[, c(hz_bottom_depths, idname(x), v)]
  } else {
    # combine bottom depths with IDs
  	h <- horizons(x)[, c(hz_bottom_depths, idname(x))]
  	}
  
  # filter out missing data
  h <- h[complete.cases(h), ]
  # compute max by ID
  d <- tapply(h[, 1], h[, 2], max, na.rm=TRUE)
  
  # return the shallowest depth
  return(min(d, na.rm=TRUE))
  }
)

# overload max() to give us the max depth within a collection
setMethod(f='max', signature='SoilProfileCollection',
definition=function(x, v=NULL){
	# get bottom depth column name
	hz_bottom_depths <- horizonDepths(x)[2]
	
	# optionally use a horizon-level property refine calculation
	if(!missing(v)) {
		# combine bottom depths with IDs and variable
		h <- horizons(x)[, c(hz_bottom_depths, idname(x), v)]
	}	else {
	  # combine bottom depths with IDs
		h <- horizons(x)[, c(hz_bottom_depths, idname(x))]
		}
	
	# filter out missing data
	h <- h[complete.cases(h), ]
	# compute max by ID
	d <- tapply(h[, 1], h[, 2], max, na.rm=TRUE)
	
  # return the deepest depth
  return(max(d, na.rm=TRUE))
  }
)

# overload length() to give us the number of profiles in the collection
setMethod(f='length', signature='SoilProfileCollection',
  definition=function(x){
  l <- length(profile_id(x))
  return(l)
  }
)

# overload nrow() to give us the number of horizons in the collection
if (!isGeneric('nrow'))
  setGeneric('nrow', function(x) standardGeneric('nrow'))

setMethod(f='nrow', signature='SoilProfileCollection',
  definition=function(x){
  nrow(x@horizons)
  }
)


# overload unique() via digest eval of unique profiles
uniqueSPC <- function(x, vars){
  # compute hash by profile, for selected variables
  md5 <- profileApply(x, function(i) {
    # unlist in order to drop row names
    digest(unlist(as(i, 'data.frame')[, vars]))
  })

	# get unique hashes
	u.md5 <- unique(md5)

	# list profile idx by hash:
	profiles.by.hash <- sapply(u.md5, function(i) which(md5 == i), simplify=FALSE)

	# get an index of the first copy of each profile
	u.profiles <- sapply(profiles.by.hash, function(i) i[1])
	
	# return an index of unique profiles
	# down-grade to un-named vector of indices
	return(as.vector(u.profiles))
}

setMethod(f='unique', signature='SoilProfileCollection', definition=uniqueSPC)



## standard column access: search horizons, then site
setMethod("$", "SoilProfileCollection",
  function(x, name) {

	# get names from site and hz data
	s.names <- siteNames(x)
	h.names <- horizonNames(x)
  
	# ## note: warnings may be issued when using auto-complete feature in RStudio
	# # when site data are initialized from an external DF, it is possible that
	# # there will be duplicate column names
	# if((name %in% h.names) && (name %in% s.names)) {
	#   warning('column name is present in horizon and site data, extracting from horizon data only', call.=FALSE)
	# }
	
	# get column from horizon data
    if (name %in% h.names) {
      res <- horizons(x)[[name]]
    } else {
      # otherwise check site data
      if (name %in% s.names) {
        res <- site(x)[[name]]
      } else {
        # if still missing return NULL
        res <- NULL
      }
    }

	return(res)
  }
)



setReplaceMethod("$", "SoilProfileCollection",
  function(x, name, value) {
  	# extract hz and site data
  	h <- horizons(x)
		s <- site(x)

    # working with horizon data
    if (name %in% names(h)) {
      h[[name]] <- value
      horizons(x) <- h
      return(x)
      }
      
    # working with site data  
    if(name %in% names(s)) {
      s[[name]] <- value
      # TODO: use site(x) <- s
      x@site <- s
      return(x)
      }
    
    # ambiguous: use length of replacement to determing: horizon / site
		n.site <- nrow(s)
		n.hz <- nrow(h)
		l <- length(value)
		
		if(l == n.hz) {
		  h[[name]] <- value
		  horizons(x) <- h
		  return(x)
		}
		
		if(l == n.site) {
		  s[[name]] <- value
		  # TODO: use site(x) <- s
		  x@site <- s
		  return(x)
		}
		
		# otherwise, there is a problem
		stop('length of replacement must equal number of sites or number of horizons')
  }
)




## subset method for SoilProfileCollection objects
## s: site-level subsetting criteria (properly quoted)
## h: horizon-level subsetting criteria (properly quoted)
## result: SoilProfileCollection with all profiles that match _either_ criteria- i.e. greedy matching
if (!isGeneric("subsetProfiles"))
  setGeneric("subsetProfiles", function(object, s, h, ...) standardGeneric("subsetProfiles"))
  
setMethod("subsetProfiles", "SoilProfileCollection",
  function(object, s, h, ...) {
  	
  	# sanity checks
  	if(missing(s) & missing(h))
  		stop('must provide either, site or horizon level subsetting criteria', call.=FALSE)
  	
  	# extract parts
  	s.d <- site(object)
  	h.d <- horizons(object)
  	id.col <- idname(object)
  	object.ids <- profile_id(object)
  	
  	# subset using conventional data.frame methods
  	if(!missing(s))
  		s.d.sub.IDs <- subset(s.d, select=id.col, subset=eval(parse(text=s)))[, 1] # convert to vector
  	else
  		s.d.sub.IDs <- NA
  	
  	if(!missing(h))
  		h.d.sub.IDs <- subset(h.d, select=id.col, subset=eval(parse(text=h)))[, 1] # convert to vector
  	else
  		h.d.sub.IDs <- NA
  	
    # intersect IDs if s and h were used
    if(!missing(h) & !missing(s))
      matching.IDs <- intersect(s.d.sub.IDs, h.d.sub.IDs)
    
  	# if only h, or only s were used, then 
    else
  	  matching.IDs <- unique(na.omit(c(s.d.sub.IDs, h.d.sub.IDs)))
  	
  	# convert IDs into a numerical profile index
  	# note: no matches results in idx == 0
  	idx <- match(matching.IDs, object.ids)
  	
  	# subset SoilProfileCollection
  	return(object[idx, ])
  	}
)




### NOTE: this DOES NOT re-order data, only subsets!
##
## matrix / DF style access: only to horizon data
##
## i = profile index
## j = horizon / slice index
##
setMethod("[", signature=c("SoilProfileCollection", i="ANY", j="ANY"),
  function(x, i, j) {
		
  	# check for missing i and j
  	if(missing(i) & missing(j))
  		stop('must provide either a profile index or horizon/slice index, or both', call.=FALSE)
  	
  	# convert to integer
  	if(!missing(i)) {
  	  if(any(is.na(i)))
  	    stop('NA not permitted in profile index', call.=FALSE)
  	  
      # convert logical to integer per standard vector/list indexing rules (thanks Jos? Padarian for the suggestion!)
  	  if(is.logical(i)) 
  	    i <- (1:length(x))[i]
  	  
  	  can.cast <- is.numeric(i) 
  	  if(can.cast) {
  	    if(all(abs(i - round(i)) < .Machine$double.eps^0.5))
  	      i <- as.integer(i)
  	    else stop("Numeric site index does not contain whole numbers.")
  	  } else {
  	    stop("Failed to coerce site index to integer.")
  	  }
  	}
    else # if no index is provided, the user wants all profiles
      i <- 1:length(x)

    # sanity check
    if(!missing(j)) {
      
      # AGB -- added logical handling to horizon index -- there have been times I've expected j index to behave like i
      if(is.logical(j)) 
        j <- (1:length(x))[j]
      
      can.cast <- is.numeric(j) 
      if(can.cast) {
        if(all(abs(j - round(j)) < .Machine$double.eps^0.5))
          j <- as.integer(j)
        else stop("Numeric horizon/slice index does not contain whole numbers.")
      } else {
        stop("Failed to coerce horizon/slice index to integer.")
      }
      
      if(any(is.na(j)))
        stop('NA not permitted in horizon/slice index', call.=FALSE)
    }
    
    #### TODO: implicit sub-setting of horizon records should affect all slots 
    ####      https://github.com/ncss-tech/aqp/issues/89
    
    # extract requested profile IDs
    p.ids <- profile_id(x)[i]

    # extract all horizon data
    h <- horizons(x)
	
    # keep only the requested horizon data (filtered by profile ID)
    h <- h[h[[idname(x)]] %in% p.ids, ]
    
    # keep only the requested site data, (filtered by profile ID)
    s.all <- site(x)
    s.i <- which(s.all[[idname(x)]] %in% p.ids)
  	s <- s.all[s.i, , drop=FALSE] # need to use drop=FALSE when @site contains only a single column
    
  	# subset spatial data, but only if valid
  	if(validSpatialData(x)) {
  	  sp <- x@sp[i]
  	}
  	# copy emtpy SpatialPoints object
  	else {
  	  sp <- x@sp
  	}
      
    
    # subset diagnostic data, but only if it exists
    # note that not all profiles have diagnostic hz data
    d <- diagnostic_hz(x)
    if(length(d) > 0) # some data
    	d <- d[which(d[[idname(x)]] %in% p.ids), ]
    
    # subset restriction data, but only if it exists
    # note that not all profiles have restrictions
    r <- restrictions(x)
    if(length(r) > 0) # some data
      r <- r[which(r[[idname(x)]] %in% p.ids), ]
    
    ## this is almost correct, but subsetting does not propagate to other slots (https://github.com/ncss-tech/aqp/issues/89)
    # subset horizons/slices based on j --> only when j is given
    if(!missing(j)) {
      # work via list-wise iteration
      hh <- split(h, h[[idname(x)]])
      
      # safely extract horizon by index, could have length > 1
      hh <- lapply(hh, function(this.profile, idx=j) {
        
        # total horizon records available
        this.profile.n <- nrow(this.profile)
        
        # the total number that can be collected
        # assumes correct depth sorting
        safe.idx <- idx[which(idx <= this.profile.n)]
        
        # conditionally return all available records up to this.profile.n
        if(length(safe.idx) > 0) {
          res <- this.profile[safe.idx, ]
        }  else {
          res <- NULL
        }
        
        # done
        return(res)
      })
      
      # put it all back together and replace what we started with
      h <- do.call('rbind', hh)
    }
      

    # if there is REAL data in @sp, and we only have 1 row of hz per coordinate- return SPDF
    # valid spatial data is now tested via validSpatialData()
    # also need to test that there is only 1 horizon/slice per location
  	# only produces a SPDF when j index is present
    if(validSpatialData(x) & length(p.ids) == nrow(h) & !missing(j)) {
      # combine with coordinates
      message('result is a SpatialPointsDataFrame object')
      # note that we are filtering based on 'i' - an index of selected profiles
			
      # since the order of our slices and coordinates are the same
      # it is safe to use 'match.ID=FALSE'
      # this gets around a potential problem when dimnames(x)[[1]] aren't consecutive 
      # values-- often the case when subsetting has been performed
      
      ## TODO: there should always be something in @site
      # if site data, join hz+site
      if(nrow(s) > 0) {
      	return(SpatialPointsDataFrame(as(x, 'SpatialPoints')[i, ], data=join(h, s, by=idname(x)), match.ID=FALSE))
      }
      ## TODO: can this ever happen?
      # no site data
      else {
      	return(SpatialPointsDataFrame(as(x, 'SpatialPoints')[i, ], data=h, match.ID=FALSE))	
      }
    }

    # in this case there may be missing coordinates, or we have more than 1 slice of hz data
    else {
      res <- SoilProfileCollection(idcol=idname(x), depthcols=horizonDepths(x), metadata=aqp::metadata(x), horizons=h, site=s, sp=sp, diagnostic=d, restrictions=r)
      
      # preserve one off slots that may have been customised relative to defaults 
      #  in prototype or resulting from construction of SPC 
      suppressMessages(hzidname(res) <- hzidname(x))
      suppressMessages(hzdesgnname(res) <- hzdesgnname(x))
      suppressMessages(hztexclname(res) <- hztexclname(x))
      
      
      ## integrity checks: these will be implicit in the aqp 2.0 SPC
      
      # https://github.com/ncss-tech/aqp/issues/89
      # there should be as many records in @site as there are profile IDs
      if(length(profile_id(res)) != length(site(res)[[idname(res)]]))
        warning("Some profiles have been removed from the collection.", call. = FALSE)
      
      # the order of profile_ids should be the same as in @site
      if(! all(profile_id(res) == site(res)[[idname(res)]]))
        warning("profile ID order does not match order in @site", call. = FALSE)
      
      
      return(res)
    }
    
  # done
  }
)


