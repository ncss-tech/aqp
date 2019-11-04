
## horizon IDs
if (!isGeneric('hzID<-'))
  setGeneric('hzID<-', function(object, value) standardGeneric('hzID<-'))

setReplaceMethod("hzID", "SoilProfileCollection",
                 function(object, value) {
                   
                   # can't be missing
                   if(is.null(value)) {
                     stop('horizon IDs cannot be NULL or NA', call. = FALSE) 
                   }
                   
                   if(any(is.na(value)) | any(is.null(value))) {
                     stop('horizon IDs cannot be NULL or NA', call. = FALSE) 
                   }
                   
                   # length
                   if(length(value) != nrow(object)) {
                     stop('replacement horizon IDs must have same length as original', call. = FALSE)
                   }
                   
                   # unique
                   if(length(value) != length(unique(value))) {
                     stop('replacement horizon IDs must be unique', call. = FALSE)
                   }
                   
                   
                   # extract horizon and replace IDs
                   h <- object@horizons
                   # note that horizon IDs may be specified in custom-set column
                   h[[hzidname(object)]] <- value
                   # re-pack horizons
                   object@horizons <- h
                   
                   return(object)
                 }
)


## profile IDs
if (!isGeneric('profile_id<-'))
  setGeneric('profile_id<-', function(object, value) standardGeneric('profile_id<-'))

setReplaceMethod("profile_id", "SoilProfileCollection",
                 function(object, value) {
                   
                   # can't be missing
                   if(is.null(value)) {
                     stop('profile IDs cannot be NULL or NA', call. = FALSE) 
                   }
                   
                   if(any(is.na(value)) | any(is.null(value))) {
                     stop('profile IDs cannot be NULL or NA', call. = FALSE) 
                   }
                   
                   # length
                   if(length(value) != length(profile_id(object))) {
                     stop('replacement profile IDs must have same length as original', call. = FALSE)
                   }
                   
                   # unique
                   if(length(value) != length(unique(value))) {
                     stop('replacement profile IDs must be unique', call. = FALSE)
                   }
                   
                   # lookup table for converting old -> new IDs
                   idn <- idname(object)
                   pids <- profile_id(object)
                   lut <- cbind(pids, value)
                   
                   # change @site
                   s <- site(object)
                   s[[idn]] <- value
                   object@site <- s
                   
                   # change @horizons
                   h <- horizons(object)
                   update.idx <- match(h[[idn]], lut[, 1])
                   # apply edits via LUT
                   h[[idn]] <- lut[update.idx, 2]
                   object@horizons <- h
                   
                   # search in @diagnostic
                   dg <- diagnostic_hz(object)
                   dg.nm <- names(dg)
                   idx <- grep(idn, dg.nm)
                   
                   if(length(idx) > 0) {
                     # apply edits via LUT
                     update.idx <- match(dg[[idx]], lut[, 1])
                     dg[[idx]] <- lut[update.idx, 2]
                     suppressWarnings(diagnostic_hz(object) <- dg)
                   }
                   
                   return(object)
                 }
)


## horizon depth columns
if (!isGeneric('horizonDepths<-'))
  setGeneric('horizonDepths<-', function(object, value) standardGeneric('horizonDepths<-'))

setReplaceMethod("horizonDepths", "SoilProfileCollection",
                 function(object, value) {
                   
                   # can't be missing
                   if(is.null(value)) {
                     stop('cannot assign NA or NULL depth column names', call. = FALSE) 
                   }
                   
                   if(any(is.na(value)) | any(is.null(value))) {
                     stop('cannot assign NA or NULL depth column names', call. = FALSE) 
                   }
                   
                   # length
                   if(length(value) != 2) {
                     stop('horizon depth names must be a vector with two items', call. = FALSE)
                   }
                   
                   # warn about changes in names
                   if(any(value != make.names(value))) {
                     warning('names have been modified to legal data.frame column names')
                   }
                   
                   # must be safely convertable to character and safe for DF
                   value <- make.names(value)
                   
                   # save old values
                   hd <- horizonDepths(object)
                   
                   # change @horizons, just the names
                   hn <- horizonNames(object)
                   idx <- match(hd, hn)
                   hn[idx] <- value
                   horizonNames(object) <- hn
                   
                   # change @depthcols
                   object@depthcols <- value
                   
                   return(object)
                 }
)




## set horizon names
if (!isGeneric('horizonNames<-'))
  setGeneric('horizonNames<-', function(object, value) standardGeneric('horizonNames<-'))

## TODO: strip-out idname
setReplaceMethod("horizonNames", "SoilProfileCollection",
  function(object, value) {
    
    # sanity check
    if(any(is.null(value)))
      stop('cannot assign NA or NULL column names', call. = FALSE)
    
    if(any(is.na(value)))
      stop('cannot assign NA or NULL column names', call. = FALSE)
    
    # must be same length
    if(length(value) != length(horizonNames(object))) {
      stop('replacement must have same length as original', call. = FALSE)
    }
    
    # warn about changes in names
    if( any(value != make.names(value))) {
      warning('names have been modified to legal data.frame column names')
    }
    
    # assign
    names(object@horizons) <- make.names(value)
    return(object)
  }
)



## set site names
if (!isGeneric('siteNames<-'))
  setGeneric('siteNames<-', function(object, value) standardGeneric('siteNames<-'))

## TODO: strip-out idname
setReplaceMethod("siteNames", "SoilProfileCollection",
  function(object, value) {
    # sanity check
    if(is.na(value) | is.null(value))
      stop('cannot assign NA or NULL column names', call. = FALSE)
                   
      names(object@horizons) <- make.names(value)
        return(object)
  }
)





##
## reset hz ID name
##
if (!isGeneric('hzidname<-'))
  setGeneric('hzidname<-', function(object, value) standardGeneric('hzidname<-'))

setReplaceMethod("hzidname", "SoilProfileCollection",
                 function(object, value) {
                   
                   # quick sanity check
                   if(length(value) != 1)
                     stop("horizon ID name should have length of 1", call.=FALSE)
                   
                   
                   # sanity checks
                   
                   # test: does it exist?
                   if(! value %in% horizonNames(object)) {
                     stop("ID name not in horizon data", call.=FALSE)
                   }
                   
                   # test: unique?
                   x <- horizons(object)[[value]]
                   if(length(unique(x)) != nrow(object)){
                     stop("target ID name not unique", call.=FALSE)
                   }
                   
                   # replace
                   object@hzidcol <- value
                   
                   # done
                   return(object)
                 }
)


##
## initialize metadata: object modification in-place
##
if (!isGeneric('metadata<-'))
  setGeneric('metadata<-', function(object, value) standardGeneric('metadata<-'))

setReplaceMethod("metadata", "SoilProfileCollection",
  function(object, value) {

	# quick sanity check
	if(nrow(value) > 1 | nrow(value) < 1)
	  stop("metadata should be a 1-row data frame", call.=FALSE)

	# otherwise assign
	object@metadata <- value

	# done
	return(object)
	}
)

##
## initialize depth_units: object modification in-place, depth_units stored in @metadata
##
if (!isGeneric('depth_units<-'))
  setGeneric('depth_units<-', function(object, value) standardGeneric('depth_units<-'))

setReplaceMethod("depth_units", "SoilProfileCollection",
  function(object, value) {

	# quick sanity check: character, length 1

	# keep existing metadata
	md <- metadata(object)

	# default depth_units are always in metadata
	# replace what ever is there
	md[['depth_units']] <- value

	# replace metadata
	metadata(object) <- md

	# done
	return(object)
	}
)


##
## depths<- setter method - to create AQP objects: sorts based on ID and top depth
##
if (!isGeneric('depths<-'))
  setGeneric('depths<-', function(object, value) standardGeneric('depths<-'))

setReplaceMethod("depths", "SoilProfileCollection",
	function(object, value) {
		message('This is already a SoilProfilecollection-class object, doing nothing.')
		object
	})


setReplaceMethod("depths", "data.frame",
  function(object, value) {
    if (inherits(value, "formula")) {
      # extract components of formula: 1. user id, 2. top, 3. bottom
      mf <- model.frame(value, object)
      res <- .initSPCfromMF(data=object, mf=mf)
    }
    else {
      if (inherits(value, "character")) { # initialization by colnames
	      mf <- object[,value]
	      res <- .initSPCfromMF(data=object, mf=mf)
      }
      else
	      stop('invalid initialization for SoilProfile object', call.=FALSE)
    }

    # add default metadata: depths are cm
    metadata(res) <- data.frame(depth_units='cm', stringsAsFactors=FALSE)
    
    # add default site data: profile IDs in same order as hz
    site.temp <- data.frame(xxx=profile_id(res), stringsAsFactors=FALSE)
    names(site.temp) <- idname(res)
    res@site <- site.temp
    
    # done
    return(res)
  }
)


##
## initialize SP/SPC objects from a model.frame
##
.initSPCfromMF <- function(data, mf){
  # get column names containing id, top, bottom
  nm <- names(mf)
  
  
  ## danger! this will shuffle profile IDs: https://github.com/ncss-tech/aqp/issues/90
  ## 
  ## convert to character, then sort
  ## ... test
  ##
  ##
  
  # re-order data: IDs, top hz depths
  new.order <- order(data[[nm[1]]], data[[nm[2]]])
  
  # check for factor-class ID
  if(class(data[[nm[1]]]) == 'factor') {
    warning('converting IDs from factor to character', call.=FALSE)
    data[[nm[1]]] <- as.character(data[[nm[1]]])
  }
  
  # depths
  depthcols <- c(nm[2], nm[3])
  
  # create object
  res <- SoilProfileCollection(idcol=nm[1], depthcols=depthcols, horizons=data[new.order, ])
  
  # check for horizon ID name conflict
  if(hzidname(res) %in% names(data)) {
    
    # original hz ID
    o.hzid <- hzidname(res)
    
    # is this a good candidate horizon ID?
    res.status <- try(hzID(res) <- data[[o.hzid]], silent = TRUE)
    
    # if not, re-make one
    if(class(res.status) == 'try-error') {
      # add unique horizon IDs to a new column
      n.hzid <- sprintf("%s_", o.hzid)
      
      # add non-conflicting hz ID
      res@horizons[[n.hzid]] <- 1:nrow(res)
      
      # update object
      hzidname(res) <- n.hzid
      
      # notify
      warning(sprintf('`%s` is not a unique horizon ID, using `%s`', o.hzid, n.hzid), call. = FALSE)
    } else {
      # notify that everything is fine
      message(sprintf('using `%s` as a unique horizon ID', o.hzid))
    }
    
  } else {
    # no conflict, add a reasonable horizon ID
    hzID(res) <- 1:nrow(res)
  } 
  
  
  # done
  return(res)
}


##
## initialize site data
##
if (!isGeneric('site<-'))
  setGeneric('site<-', function(object, value) standardGeneric('site<-'))

setReplaceMethod("site", "SoilProfileCollection",
  function(object, value) {
	# get the corresponding vector of IDs, will be used to compute distinct site attributes
    ids <- as.character(horizons(object)[[idname(object)]])

	# creation of site data from horizon data
    if (inherits(value, "formula")) {
      mf <- model.frame(value, horizons(object), na.action=na.pass)
      nm <- names(mf)
      mf <- data.frame(ids, mf, stringsAsFactors=FALSE) # don't automatically make strings into factors
      names(mf) <- c(idname(object), nm)
      object <- .createSiteFromHorizon(object, mf)
    }
    
    # creation of site data from an external data.frame via join(..., type='left')
    if (inherits(value, "data.frame")) {
      # get column names from proposed site, and existing horizons
      ns <- names(value)
      nh <- horizonNames(object)
      
      ## remove ID column from names(horizons)
      ID.idx <- match(idname(object), nh)
      
      # check to make sure there is no overlap in proposed site + hz variable names
      if(any(ns %in% nh[-ID.idx]))
        stop('duplicate names in new site / existing horizon data not allowed', call.=FALSE)
      
      # existing site data (may be absent == 0-row data.frame)
      s <- site(object)
      
      # join to existing data: by default it will only be idname(object)
      
      ## an appropriate ID must exist in 'value' AND @site for this to work
      # LEFT-join in - assumes that appropriate IDs exist in both @site and 'value'
      # we are suppressing the 'Joining by:' output from join()
      suppressMessages(site.new <- join(s, value, type='left'))
      
      # sanity check: site + new data should have same number of rows as original
      if(nrow(s) != nrow(site.new)) {
      	message(paste('original data (', nrow(s), ' rows) new data (', nrow(site.new), ' rows)', sep=''))
        stop('invalid join condition, site data not changed', call.=FALSE)
      }
            
      # look good, proceed
      object@site <- site.new
	  }
  	
    ## TODO: finer reporting on what the problem might be
    # check to make sure the the number of rows in @site is the same as length(object)
    if(length(object) != nrow(site(object))){
    	print(paste('pedons (', length(object), ') rows of site data (', nrow(site(object)), ')', sep=''))
    	stop('invalid site data, non-unique values present in horizon data?', call.=FALSE)
    }
    
    # done
    return(object)
  }
)

# update an SPC object:
# add site data
# remove named columns from horizons
# return new SPC object
.createSiteFromHorizon <- function(object, mf){
  # create a numeric index for named site columns, as we will remove them
  # from the horizon data
  names_attr <- names(mf)
  idx <- match(names_attr, horizonNames(object))
  # remove the index to the ID columnm, as we do not want to remove this from
  # the horizon data !
  idx <- idx[-match(idname(object), names_attr)]
	
  # this will break when multiple horizons in the same pedon have different site data!
  # this seems to work fine in all cases, as we keep the ID column
  # and it ensures that the result is in the same order as the IDs
  new_site_data <- ddply(mf, idname(object),
      .fun=function(x) {
	      unique(x[, names_attr])
      }
  )

  # if site data is already present in the object, we don't want to erase it
  site_data <- join(site(object), new_site_data, by=idname(object))

  # remove the named site data from horizon_data
  object@horizons <- horizons(object)[, -idx]
	
  # replace existing site data
  object@site <- site_data

  # done
  return(object)
}


##
## horizon data replacement
##
## horizons<- setter method
##
if (!isGeneric('horizons<-'))
  setGeneric('horizons<-', function(object, value) standardGeneric('horizons<-'))


## TODO: the new class structure will eliminate problems caused by this function
setReplaceMethod("horizons", "SoilProfileCollection",
  function(object, value) {
  # testing the class of the horizon data to add to the object
  if (!inherits(value, "data.frame"))
	  stop("value must be a data.frame", call.=FALSE)
    
  ## 
  ## not sure if this test is important... as sometimes we want to delete horizons
  ##
  # testing the number of rows of the horizon data
  # if (nrow(value) != nrow(object))
	  # stop("inconsistent number of rows")

  # basic test of ids:
  if(!idname(object) %in% names(value)) # is there a matching ID column in the replacement?
  	stop("there is no matching ID column in replacement", call.=FALSE)

  if(length(setdiff(unique(as.character(value[[idname(object)]])), profile_id(object))) > 0)
  	stop("there are IDs in the replacement that do not exist in the original data", call.=FALSE)

  # NEW: more extensive test of ids -- is it possible to merge rather than replace?
  if(hzidname(object) %in% names(value)) {
    # if hzidname for the SPC is present in the new data,
    
    # only merge if all horizon IDs in the SPC are also present in the new data
    #   and there are new columns in value that are not in horizons already
    if((length(setdiff(unique(as.character(value[[hzidname(object)]])), hzID(object))) == 0) &
       any(!unique(names(value)) %in% unique(names(object@horizons)))) {
      to_merge <- c(names(value)[!names(value) %in% names(object@horizons)], idname(object), hzidname(object))
      object@horizons <- merge(object@horizons, value[,to_merge], all.x = TRUE, by = c(idname(object), hzidname(object)))
      
      # now, do updates to "old" columns so we do not duplicate
      to_update <- names(value)[!names(value) %in% to_merge]
      object@horizons[,to_update] <- value[match(object@horizons[,hzidname(object)], value[,hzidname(object)]), to_update]
      return(object)
    }
  }
    
  ##
  ## 2017-01-05: holy shit, why are we re-ordering the horizon data? 
  ## causes SPC corruption after rbind with keys that overlap
  ## https://github.com/ncss-tech/aqp/issues/23
  ## fixed: b959963edca37c2d89fa3994be0027638560f902
  ## thanks: Andrew Brown
    
  ## replacement: order by IDs, then top horizon boundary
  # hz_top_depths <- horizonDepths(object)[1]
  # object@horizons <- value[order(value[[idname(object)]], value[[hz_top_depths]]), ]
  
  # replace existing horizons with modified version
  object@horizons <- value

  # done
  return(object)
  }
)

##
## intit diagnotic horizon data
##
## NOTE: these data are likely to be free-form, may not exist for every profile, and are usually 1:many
##
if (!isGeneric('diagnostic_hz<-'))
  setGeneric('diagnostic_hz<-', function(object, value) standardGeneric('diagnostic_hz<-'))

setReplaceMethod("diagnostic_hz", "SoilProfileCollection",
  function(object, value) {
  
  # get the initial data
  d <- diagnostic_hz(object)
  
  # get column and ID names
  nm <- names(value)
  idn <- idname(object)
  pIDs <- profile_id(object)
  
  # testing the class of the new data
  if (!inherits(value, "data.frame"))
    stop("diagnostic horizon data must be a data.frame", call.=FALSE)
	
  # test for the special case where internally-used functions 
  # are copying over data from one object to another, and diagnostic_hz(obj) is a 0-row data.frame
  # short-circut, and return original object
  if(nrow(d) == 0 & nrow(value) == 0)
  	return(object)
  
  # test to make sure that our common ID is present in the new data
  if(! idn %in% nm)
  	stop(paste("diagnostic horizon data are missing a common ID:", idn), call.=FALSE)
  
  # test to make sure that at least one of the IDS in candidate data are present within SPC
  if(all( ! unique(value[[idn]]) %in% pIDs) )
  	warning('candidate diagnostic horizon data have NO matching IDs in target SoilProfileCollection object!', call. = FALSE)
  
  # warn user if some of the IDs in the candidate data are missing
  if(any( ! unique(value[[idn]]) %in% pIDs) ) {
    warning('some records in candidate diagnostic horizon data have no matching IDs in target SoilProfileCollection object')
  }
  
  # if data are already present, warn the user
  if(nrow(d) > 0)
  	warning('overwriting existing diagnostic horizon data!', call.=FALSE)
  
  # copy data over
  object@diagnostic <- value
  
  # done
  return(object)
  }
)
