
if (!isGeneric("profileApply"))
 	setGeneric("profileApply", function(object, FUN, 
 	                                    simplify = TRUE,
 	                                    frameify = FALSE,
 	                                    chunk.size = 100,
 	                                    column.names = NULL,
 	                                    ...) standardGeneric("profileApply"))

# made internal by AGB 2020/01/19
#
# analog to apply():
# operates by profile, returns vector of length(object) or nrow(object)
.profileApply <- function(object, FUN, simplify=TRUE, ...) {

		# get profile IDs
		pIDs <- profile_id(object)

		# pre-allocate list
		l <- vector(mode = 'list', length = length(pIDs))
		# must set list names based on expected assignment in for() loop
		names(l) <- pIDs

		# iterate over SPC, spliting into a list of SPC_i ... SPC_n
		for(i in seq_along(pIDs)) {
			pID <- pIDs[i]
			l[[pID]] <- do.call(FUN, list(object[i, ], ...))
		}

		# optionally simplify
		if(simplify) 
			return(unlist(l))
		
		return(l)
}

setMethod(f='profileApply', signature='SoilProfileCollection', function(object, FUN, 
                        simplify = TRUE, 
                        frameify = FALSE,
                        chunk.size = floor(sqrt(length(object))), 
                        column.names = NULL, 
                        ...) {
  if(simplify & frameify) {
    # simplify and frameify are both TRUE -- ignoring simplify argument
    simplify <- FALSE
  }
            
  # total number of profiles we have to iterate over  
  n <- length(object)
  o.name <- idname(object)
  o.hname <- hzidname(object)
  
  # split the SPC of size n into chunk.size chunks
  chunk <- sort(1:n %% max(1, round(n / chunk.size))) + 1
  
  # we first iterate over list of chunks of size chunk.size, keeping lists smallish
  # by dividing by a tunable factor -- set as 100 by default
  # then we iterate through each chunk, calling FUN on each element (profile)
  # then, concatenate the result into a list (or vector if simplify == TRUE)
  res <- do.call('c', lapply(split(1:n, chunk), function(idx) {
    .profileApply(object[idx,], FUN, simplify, ...)
  }))
  
  # return profile IDs if it makes sense for result
  if(length(res) == length(object))
    names(res) <- profile_id(object)
  
  # return horizon IDs if it makes sense for result
  if(!simplify & inherits(res, 'data.frame'))
    if(nrow(res) == nrow(object)) 
      names(res) <- hzID(object)   
  
  # combine a list (one element per profile) into data.frame result
  if(!simplify & frameify) {
    
    # make sure the first result is a data.frame (i.e. FUN returns a data.frame)
    if(is.data.frame(res[[1]])) {
      
      # make a big data.frame
      res <- as.data.frame(do.call('rbind', res), stringsAsFactors = FALSE)
      
      # get ids
      pid <- profile_id(object)
      hz.id <- hzID(object)
      
      # determine if merge of res into @horizon or @site is feasible/reasonable
      # if so, we want to keep track of unique site and horizon IDs
      
      # if hzidname is in big df and all hzID in result are from parent object...
      if(o.hname %in% colnames(res) & all(res[[o.hname]] %in% hz.id)) {
        
        # make a master site/horizon id table (all in SPC)
        pid.by.hz <- horizons(object)[[o.name]]
        id.df <- data.frame(pid.by.hz, hz.id, stringsAsFactors = FALSE)
        colnames(id.df) <- c(o.name, o.hname)
        
        # warn if some hz IDs are missing in result
        if(!all(hz.id %in% res[[o.hname]])) {
          warning("frameify found horizon ID (", o.hname, ") in result but some IDs are missing!", call.=FALSE)
        }
        
        # do a left join, filling in any missing idname, hzidname from res with NA
        res <- merge(id.df, res, all.x = TRUE, sort=FALSE)
        
      } else if(o.name %in% colnames(res) & all(res[[o.name]] %in% pid)) {
        
        # same as above, only for site level summaries (far more common)
        id.df <- data.frame(pid, stringsAsFactors = FALSE)
        colnames(id.df) <- c(o.name)
        
        if(!all(pid %in% res[[o.name]])) {
          # this shouldn't really happen -- usually a problem with FUN
          warning("frameify found site ID (", o.name, ") in result but some IDs are missing!", call.=FALSE)
        }
        
        # do a left join, filling in any missing idname from res with NA
        res <- merge(id.df, res, all.x = TRUE, sort=FALSE)
      }
      
      if(!is.null(column.names))
        colnames(res) <- column.names
      
    } else {
      warning("first result is not class `data.frame` and frameify is TRUE. defaulting to list output.", call. = FALSE)
    }
  }
  
  if(simplify)
   return(unlist(res))
  
  return(res)
})
