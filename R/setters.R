
## horizon IDs
if (!isGeneric('hzID<-'))
  setGeneric('hzID<-', function(object, value) standardGeneric('hzID<-'))

setReplaceMethod("hzID", signature(object = "SoilProfileCollection"),
                 function(object, value) {
                   
                   if (!inherits(value, 'character')) {
                     message("converting horizon IDs from integer to character")
                     value <- as.character(value)
                   }
                   
                   # can't be missing
                   if (is.null(value)) {
                     stop('horizon IDs cannot be NULL or NA', call. = FALSE) 
                   }
                   
                   if (any(is.na(value)) | any(is.null(value))) {
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
                   object@horizons[[hzidname(object)]] <- value
                   
                   return(object)
                 }
)

## profile IDs
if (!isGeneric('profile_id<-'))
  setGeneric('profile_id<-', function(object, value) standardGeneric('profile_id<-'))

setReplaceMethod("profile_id", signature(object = "SoilProfileCollection"),
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
                   object@site <- .as.data.frame.aqp(s, metadata(object)$aqp_df_class)
                   
                   # change @horizons
                   h <- object@horizons
                   update.idx <- match(h[[idn]], lut[, 1])
                   # apply edits via LUT
                   h[[idn]] <- lut[update.idx, 2]
                   object@horizons <- .as.data.frame.aqp(h, metadata(object)$aqp_df_class)
                   
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
                   
                   # search in @restrictions
                   re <- restrictions(object)
                   re.nm <- names(re)
                   idx <- grep(idn, re.nm)
                   
                   if(length(idx) > 0) {
                     # apply edits via LUT
                     update.idx <- match(re[[idx]], lut[, 1])
                     re[[idx]] <- lut[update.idx, 2]
                     suppressWarnings(restrictions(object) <- re)
                   }
                   
                   return(object)
                 }
)


## horizon depth columns
if (!isGeneric('horizonDepths<-'))
  setGeneric('horizonDepths<-', function(object, value) standardGeneric('horizonDepths<-'))

setReplaceMethod("horizonDepths", signature(object = "SoilProfileCollection"),
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
setReplaceMethod("horizonNames", signature(object = "SoilProfileCollection"),
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
setReplaceMethod("siteNames", signature(object = "SoilProfileCollection"),
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

setReplaceMethod("hzidname", signature(object = "SoilProfileCollection"),
                 function(object, value) {
                   
                   # quick sanity check
                   if(length(value) != 1)
                     stop("horizon ID name should have length of 1", call.=TRUE)
                   
                   
                   # sanity checks
                   
                   # test: does it exist?
                   if(! value %in% horizonNames(object)) {
                     stop("horizon ID name not in horizon data", call.=TRUE)
                   }
                   
                   # test: unique?
                   x <- object@horizons[[value]]
                   if(length(unique(x)) != nrow(object)){
                     # convert error to warning, 
                     # prevent stoppage from nonunique, 
                     # fail gracefully and retain to default
                     warning("horizon ID name (",value,") not unique. unique ID not changed.", call.=TRUE)
                   } else {
                     # replace
                     object@hzidcol <- value
                     
                     # convert contents to character, if needed
                     if(!is.character(x)) {
                       message(sprintf("converting horizon IDs in column `%s` to character", value))
                       object@horizons[[value]] <- as.character(object@horizons[[value]])
                     }
                   }
                   
                   # done
                   return(object)
                 }
)

##
## set hz designation name
##
if (!isGeneric('hzdesgnname<-'))
  setGeneric('hzdesgnname<-', function(object, value) standardGeneric('hzdesgnname<-'))

setReplaceMethod("hzdesgnname", signature(object = "SoilProfileCollection"),
                 function(object, value) {
                   # test: does it exist?
                   if(!length(value))
                     value <- ""
                   
                   if(length(value)) {
                     # several ways to "reset" the hzdesgnname
                     if((value == "") | is.na(value) | is.null(value)) {
                       value <- character(0)
                       message("set horizon designation name column to `character` of length zero")
                     } else if (!(value %in% horizonNames(object))) {
                       stop(paste0("horizon designation name (",value,") not in horizon data"), call.=FALSE)
                     }
                   } 
                   
                   # replace
                   object@hzdesgncol <- value
                   
                   # done
                   return(object)
})

##
## set hz designation name
##
if (!isGeneric('hztexclname<-'))
  setGeneric('hztexclname<-', function(object, value) standardGeneric('hztexclname<-'))

setReplaceMethod("hztexclname", signature(object = "SoilProfileCollection"),
                 function(object, value) {
                   # test: does it exist?
                   if(!length(value))
                     value <- ""
                   
                   if(length(value)) {
                     # several ways to "reset" the hzdesgnname
                     if((value == "") | is.na(value) | is.null(value)) {
                       value <- character(0)
                       #message("set horizon texture class name to `character` of length zero")
                     } else if (! value %in% horizonNames(object)) {
                       stop("horizon texture class name not in horizon data", call.=TRUE)
                     }
                   } 
                   
                   # replace
                   object@hztexclcol <- value
                   
                   # done
                   return(object)
                 })

##
## initialize metadata: object modification in-place
##
if (!isGeneric('metadata<-'))
  setGeneric('metadata<-', function(object, value) standardGeneric('metadata<-'))

setReplaceMethod("metadata", signature(object = "SoilProfileCollection"),
  function(object, value) {

  # metadata() is now stored in a list()
  # 
	# quick sanity check
	#if(nrow(value) > 1 | nrow(value) < 1)
	#  stop("metadata should be a 1-row data frame", call.=FALSE)

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

setReplaceMethod("depth_units", signature(object = "SoilProfileCollection"),
  function(object, value) {

	# quick sanity check: character, length 1

	# keep existing metadata
	md <- metadata(object)

	# default depth_units are always in metadata
	# replace what ever is there
	md$depth_units <- value

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

setReplaceMethod("depths", signature(object = "SoilProfileCollection"),
	function(object, value) {
		message('This is already a SoilProfilecollection-class object, doing nothing.')
		object
	})


setReplaceMethod("depths", "data.frame",
  function(object, value) {
    if (inherits(value, "formula")) {
      # extract components of formula: 1. user id, 2. top, 3. bottom
      mf <- model.frame(value, object)
      res <- .initSPCfromMF(data = object, mf = mf)
    } else {
      if (inherits(value, "character")) { # initialization by colnames
	      mf <- object[,value]
	      res <- .initSPCfromMF(data = object, mf = mf)
      } else {
	      stop('invalid initilization for SoilProfileCollection object', call. = FALSE)
      }
    }
    
    # add default site data: profile IDs in site same order as horizons
    site.temp <- data.frame(id = profile_id(res), stringsAsFactors = FALSE)
    names(site.temp) <- idname(res)
    
    res@site <- .as.data.frame.aqp(site.temp, aqp_df_class(res))
    res@horizons <- .as.data.frame.aqp(res@horizons, aqp_df_class(res))
    
    # done
    return(res)
  }
)


##
## initialize SP/SPC objects from a model.frame
##
.initSPCfromMF <- function(data, mf, use_class){
  # get column names containing id, top, bottom
  nm <- names(mf)
  
  # check for factor-class ID
  if (inherits(data[[nm[1]]], 'factor')) {
    message('converting profile IDs from factor to character')
    data[[nm[1]]] <- as.character(data[[nm[1]]])
  }
  
  # check for integer IDs
  if (inherits(data[[nm[1]]], 'integer')) {
    message('converting profile IDs from integer to character')
    data[[nm[1]]] <- as.character(data[[nm[1]]])
  }
  
  # depths 
  depthcols <- c(nm[2], nm[3])
  
  # create a site table with just IDs
  nusite <- .as.data.frame.aqp(data.frame(.coalesce.idx(data[[nm[1]]]), stringsAsFactors = FALSE), class(data)[1])
  names(nusite) <- nm[1]
  
  if(nrow(nusite) != length(unique(data[[nm[1]]]))) {
    warning("unsorted input data will be ordered during promotion to SoilProfileCollection", call. = FALSE)
  
    # reorder based on site ID and top depth column
    ## note: forced character sort on ID -- need to impose some order to check depths
    data <- data[order(as.character(data[[nm[1]]]), data[[depthcols[1]]]), ]
  }
  
  # create object
  res <- SoilProfileCollection(idcol = nm[1], 
                               hzidcol = 'hzID',
                               depthcols = depthcols, 
                               site = nusite,
                               horizons = data)
  
  # check for horizon ID name conflict
  if(hzidname(res) %in% names(data)) {
    
    # original hz ID
    o.hzid <- hzidname(res)
    
    # is this a good candidate horizon ID?
    res.status <- try(hzID(res) <- data[[o.hzid]], silent = TRUE)
    
    # if not, re-make one
    if(inherits(res.status, 'try-error')) {
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
  setGeneric('site<-', function(object, value) 
    standardGeneric('site<-'))

setReplaceMethod("site", signature(object = "SoilProfileCollection"),
  function(object, value) {

    # get profile IDs from horizon table
    ids <- as.character(horizons(object)[[idname(object)]])
    ids.coalesce <- .coalesce.idx(ids)

	# creation of site data from horizon data
    if (inherits(value, "formula")) {
      mf <- model.frame(value, object@horizons, na.action = na.pass)
      nm <- names(mf)
      mf <- data.frame(ids, mf, stringsAsFactors = FALSE)
      names(mf) <- c(idname(object), nm)
      object <- .createSiteFromHorizon(object, mf)
    }
    
    # creation of site data from an external data.frame via merge (LEFT JOIN)
    if (inherits(value, "data.frame")) {
      # get column names from proposed site, and existing horizons
      ns <- names(value)
      nh <- horizonNames(object)
      
      ## remove ID column from names(horizons)
      ID.idx <- match(idname(object), nh)
      
      # check to make sure there is no overlap in proposed site + hz variable names
      if(any(ns %in% nh[-ID.idx]))
        stop('duplicate names in new site / existing horizon data not allowed', call. = FALSE)
      
      # existing site data (may be absent == 0-row data.frame)
      s <- object@site
      
      if(any(s[[idname(object)]] != ids.coalesce)) {
        warning("site and horizon data are out of sync!")
      }
      
      # join to existing data: by default it will only be idname(object)
      
      ## an appropriate ID must exist in 'value' AND @site for this to work
      # LEFT JOIN
      suppressMessages(site.new <- merge(s, value, all.x = TRUE, sort = FALSE))
      
      new.id.order <- site.new[[idname(object)]]
      if(any(new.id.order != ids.coalesce)) {
        message("join condition resulted in sorting of sites, re-applying original order")
        site.new <- site.new[match(ids.coalesce, new.id.order),]
      }     
      
      # sanity check: site + new data should have same number of rows as original
      if(nrow(s) != nrow(site.new)) {
      	message(paste('original data (', nrow(s), ' rows) new data (', nrow(site.new), ' rows)', sep=''))
        stop('invalid join condition, site data not changed', call.=FALSE)
      }
            
      # 2020-05-30: subclasses of data.frame have more than one class
      object@site <- .as.data.frame.aqp(site.new, metadata(object)$aqp_df_class)
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
	      unique(x[, names_attr, drop = FALSE])
      }
  )

  # if site data is already present, we don't overwrite/erase it
  site_data <- merge(object@site, new_site_data, by = idname(object),
                     all.x = TRUE, sort = FALSE)

  # remove the named site data from horizon_data
  h <- object@horizons
  hnames <- colnames(h)
  for(i in idx) {
    h[[hnames[i]]] <- NULL
  }
  
  if(!inherits(h, 'data.frame'))
     print(h)
     
  object@horizons <- .as.data.frame.aqp(h, aqp_df_class(object))
	
  # replace existing site data
  object@site <- .as.data.frame.aqp(site_data, aqp_df_class(object))

  # done
  return(object)
}

##
## horizon replacement method
##
if (!isGeneric('replaceHorizons<-'))
  setGeneric('replaceHorizons<-', function(object, value) 
    standardGeneric('replaceHorizons<-'))

setReplaceMethod("replaceHorizons", 
                 signature(object = "SoilProfileCollection"),
                 function(object, value) {
  
  required.columns <-  c(idname(object), horizonDepths(object))                 
  required.missing <- !required.columns %in% names(value)
                    
  if(any(required.missing))
    stop(paste0("required horizon data are missing: ",
         paste0(required.columns[required.missing], collapse=", ")), call. = FALSE)
                   
  ids.match1 <- all(profile_id(object) %in% value[[idname(object)]])
  if(!ids.match1)
    stop("profile IDs in site are missing from replacement horizons!", call. = FALSE)
  
  ids.match2 <- all(value[[idname(object)]] %in% profile_id(object)) 
  if(!ids.match2)
    stop("profile IDs in replacement are missing from site!", call. = FALSE)
  
  optional.columns <-  c(hzidname(object), 
                         hzdesgnname(object), 
                         hztexclname(object))
  
  optional.missing <- !optional.columns %in% names(value)
  
  #if(any(optional.missing))
    #message(paste0("optional columns are missing: ", 
    #               paste0(optional.columns[optional.missing], 
    #               collapse=", ")))
  
  # assign hzID if hzidname() is missing
  if(optional.missing[1]) {
    value$hzID <- 1:nrow(value)
    hzidname(object) <- "hzID"
    message("no horizon ID present, defaulting to `hzID`")
  }
  
  object@horizons <- .as.data.frame.aqp(value, aqp_df_class(object))
  return(object)
})

##
## horizon data left join
##
## horizons<- left join method
##
if (!isGeneric('horizons<-'))
  setGeneric('horizons<-', function(object, value) 
    standardGeneric('horizons<-'))

setReplaceMethod("horizons", signature(object = "SoilProfileCollection"),
  function(object, value) {
    
  # testing the class of the horizon data to add to the object
  if (!inherits(value, "data.frame"))
	  stop("new horizon data input value must inherit from data.frame", call.=FALSE)
    
  # not required: enforce idname and/or hzidname presence
  # idnames <- c(idname(object), hzidname(object))
  # if(!all(idnames %in% names(value)))
  #   stop(sprintf("new horizon data input value should contain column names: %s",
  #                paste0(idnames, collapse=",")))
  #   
  # get the corresponding vector of IDs, will be used to compute distinct attributes
  ids <- as.character(horizons(object)[[idname(object)]])

  # get column names from proposed horizons, and existing site    
  ns <- names(value)
  nh <- siteNames(object)
    
  ## remove ID column from names(site)
  ID.idx <- match(idname(object), nh)
    
  # check to make sure there is no overlap in proposed site + hz variable names
  if(any(ns %in% nh[-ID.idx]))
    stop('horizons left join value contains duplicate names', call.=FALSE)
  
  h.id <- as.character(object@horizons[[hzidname(object)]])
  original.horizon.order <- 1:length(h.id)
  names(original.horizon.order) <- h.id

  original.site.order <- match(.coalesce.idx(object@site[[idname(object)]]),
                               object@site[[idname(object)]])
    
  ## debug
  # print(original.order)
  
  # in keeping with tradition of site<-, we let the join happen 
  # left join to existing data
  suppressMessages(horizon.new <- merge(object@horizons, 
                                        value, 
                                        #by = c(idname(object), hzidname(object)),
                                        all.x = TRUE, sort = FALSE))
  
  new.horizon.order <- match(names(original.horizon.order), 
                             horizon.new[[hzidname(object)]])
  chnew <- .coalesce.idx(horizon.new[[idname(object)]])
  if(length(chnew) != length(original.site.order) |
     sum(suppressWarnings(original.site.order != chnew)) > 0) {
    message("join condition resulted in sorting of horizons, re-applying original order")
    horizon.new <- horizon.new[new.horizon.order,]
  }
  
  # sanity check: horizons + new data should have same number of rows as original
  if(nrow(object@horizons) != nrow(horizon.new)) {
    message(paste('original data (', nrow(s), ' rows) new data (', nrow(horizon.new), ' rows)', sep=''))
    stop("invalid horizons left join condition, data not changed", call.=FALSE)
  }
    
  # 2020-05-30: subclasses of data.frame have more than one class
  object@horizons <- .as.data.frame.aqp(horizon.new, aqp_df_class(object))
  
  # check to make sure same profile IDs are present
  if(any(!(ids %in% as.character(object@horizons[[idname(object)]])))) {
    print(paste('pedons (', nrow(object), ') rows of horizon data (', nrow(object@horizons), ')', sep=''))
    stop('profile IDs are missing from join result, data not changed', call.=FALSE)
  }
  
  # done
  return(object)
})

##
## init diagnostic horizon data
##
## NOTE: these data are likely to be free-form, may not exist for every profile, and are usually 1:many
##
if (!isGeneric('diagnostic_hz<-'))
  setGeneric('diagnostic_hz<-', function(object, value) 
    standardGeneric('diagnostic_hz<-'))

setReplaceMethod("diagnostic_hz", 
                 signature(object = "SoilProfileCollection"),
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
  	stop(paste("diagnostic horizon data are missing pedon ID column: ", idn), call.=FALSE)
  
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
  object@diagnostic <- .as.data.frame.aqp(value, metadata(object)$aqp_df_class)
  
  # done
  return(object)
})

# restriction data
# likely to either have no restrictions or possibly more than one
if (!isGeneric('restrictions<-'))
  setGeneric('restrictions<-', function(object, value) 
    standardGeneric('restrictions<-'))

setReplaceMethod("restrictions", signature(object = "SoilProfileCollection"),
                 function(object, value) {
                   
                   # get the initial data
                   d <- restrictions(object)
                   
                   # get column and ID names
                   nm <- names(value)
                   idn <- idname(object)
                   pIDs <- profile_id(object)
                   
                   # testing the class of the new data
                   if (!inherits(value, "data.frame"))
                     stop("restriction data must be a data.frame", call.=FALSE)
                   
                   # test for the special case where internally-used functions 
                   # are copying over data from one object to another, and diagnostic_hz(obj) is a 0-row data.frame
                   # short-circuit, and return original object
                   if(nrow(d) == 0 & nrow(value) == 0)
                     return(object)
                   
                   # test to make sure that our common ID is present in the new data
                   if(! idn %in% nm)
                     stop(paste("restriction data are missing pedon ID column: ", idn), call.=FALSE)
                   
                   # test to make sure that at least one of the IDs in candidate data are present within SPC
                   if(all(!unique(value[[idn]]) %in% pIDs) )
                     warning('restriction data have no matching IDs in target SoilProfileCollection object!', call. = FALSE)
                   
                   # warn user if some of the IDs in the candidate data are missing
                   if(any( ! unique(value[[idn]]) %in% pIDs) ) {
                     warning('some records in restriction data have no matching IDs in target SoilProfileCollection object')
                   }
                   
                   # if data are already present, warn the user
                   if(nrow(d) > 0)
                     warning('overwriting existing restriction data!', call.=FALSE)
                   
                   # copy data over
                   object@restrictions <- .as.data.frame.aqp(value, metadata(object)$aqp_df_class)
                   
                   # done
                   return(object)
                 }
)
