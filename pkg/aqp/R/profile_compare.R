
##############################################################
## profile classification functions ##
##############################################################

## consider using 'ff' package for file-based storage of VERY large objects. Probably just the dissimilarity matrix

## soil.slot() would be a better approach than using every nth depth slice

## careful -- we are getting the labels for the final dissimilarity matrix from the levels of the profile ID

## TODO: site/hz properties combined:
## 2012-02-28: partiall implemented, but no way to control weighting
## D = (D_hz/max(D_hz) * w_hz) + (D_site/max(D_site) * w_site) / (w_hz + w_site)

## TODO: we are suppressing warnings from daisy() when input is all NA
##       this is fine for now, but we should figure out a better way


## low-level function that the user will probably not ever use directly
# Seems to scale to 1000 profiles with 5 variables, could use optimization
# function requires at least two attributes
# hard coded reference to s$id
# set k to 0 for no depth weighting 
pc <- function(s, vars, max_d, k, sample_interval=NA, replace_na=TRUE, add_soil_flag=TRUE, return_depth_distances=FALSE, strict_hz_eval=FALSE, progress='none', plot.depth.matrix=FALSE, 
rescale.result=FALSE)
	{
	
	# currently this will only work with integer depths
	if(!is.integer(na.omit(s$top)) | !is.integer(na.omit(s$bottom)))
		stop('this function can only accept integer horizon depths', call.=FALSE)
	
	# check to make sure that there is an 'id' column
	if(is.null(s$id))
		stop("'s' must contain a column named 'id' ", call.=FALSE)
	
	# if the id column is not a factor, convert it to one:
	if(class(s$id) != 'factor')
		s$id <- factor(s$id)
	
	# identify the number of profiles
	n.profiles <- length(levels(s$id))
	
	# number of variables
	n.vars <- length(vars)
	
	# sequence describing depth slice indices	
	# use a sequence from 1 ... max depth
	depth_slice_seq <- 1:max_d
	
	# use decimated sampling if requested
	if(!is.na(sample_interval) & sample_interval != 1)
		depth_slice_seq <- depth_slice_seq[depth_slice_seq %% sample_interval == 1]
	
	# compute a weighting vector based on k	
	w <- 1 * exp(-k * depth_slice_seq)
	
	# TODO: if max_d < profile depth, s.unrolled is not truncated to max_d
	## unroll each named soil property, for each soil profile
	## the result is a list matricies with dimensions: depth, num_properties 
	# this approach requires a named list of soil properties
	message(paste("Unrolling ", n.profiles, " Profiles", sep=""))
	s.unrolled <- dlply(s, .(id), .progress=progress, .fun=function(di, p=vars, d=max_d, strict=strict_hz_eval, .parallel=getOption('AQP_parallel', default=FALSE)) 
		{
		
		# iterate over the set of properties, unrolling as we go
		# the result is a [z by p] matrix unrolled to max_d
		m <- try(sapply(p, function(p_i) unroll(di$top, di$bottom, prop=di[,p_i], max_depth=d, strict=strict) ))
		
		## TODO: could be better
		# check for a non-NULL attribute of 'class'
		# this will only happen when there was an error
		if( !is.null(attr(m, 'class')))
			{
			if(attr(m, 'class') == 'try-error')
				{
				stop(paste('Error: bad horizon structure in soil id', as.character(unique(di$id))), call.=FALSE)
				}
			}
		else
			return(m)
		}
	)

	
	## NOTE: this will not work when a user-defined interval is used for slicing!!
	## 
	## generate a matrix storing a flag describing soil vs. non-soil at each slice
	## note that this does not take into account missing horizon data within the profile
	if(add_soil_flag)
		{
		# get the depth of each profile
		## could be converted to 'vaggregate' in latest version of plyr
		s.slices_of_soil <- tapply(s$bottom, s$id, function(i) max(i, na.rm=TRUE) )
		
		# truncate to the max requested depth
		s.slices_of_soil <- ifelse(s.slices_of_soil <= max_d, s.slices_of_soil, max_d)
		s.slices_of_non_soil <- max_d - s.slices_of_soil
		
		s.slices_of_soil.length <- length(s.slices_of_soil)
		
		# init a matrix with dimensions: depth slices, number of profiles
		soil.matrix <- matrix(ncol=s.slices_of_soil.length, nrow=max_d)
		
		# file with TRUE for 'soil' or FALSE for 'non-soil'
		for(s.i in 1:s.slices_of_soil.length)
			soil.matrix[, s.i] <- c(rep(TRUE, s.slices_of_soil[s.i]), rep(FALSE, s.slices_of_non_soil[s.i]))
		
		
		# plot a diagnostic image, but only when reasonable to do so (< 100 profiles)
		if(n.profiles <= 100 & plot.depth.matrix)
		  {
			# define color scheme: if all TRUE, then we only need 1 color
			if(length(table(soil.matrix)) > 1)
				image.cols <- c(NA, 'grey')
			else
				image.cols <- c('grey')
			
		  labs <- levels(s$id)
		  image(x=1:n.profiles, y=1:max_d, z=t(soil.matrix), col=image.cols, ylim=c(max_d, 1), xlab='ID', ylab='Slice Number (usually eq. to depth)', main='Soil / Non-Soil Matrix', axes=FALSE)
		  box()
		  abline(v=seq(1, n.profiles)+0.5, lty=2)
		  axis(side=2, at=pretty(c(0, depth_slice_seq)), las=1)
		  axis(side=1, at=1:n.profiles, labels=labs, las=2, cex.axis=0.5)
		  }
		}
		
	
	##
	## new version for computing slice-wise dissimilarities... fast! 
	## 
	message("Computing Dissimilarity Matrices", appendLF=FALSE)
	
	## TEMP HACK to supress warnings generated by calling daisy with all NA input
	ow <- options('warn')
	options(warn=-1)
	
	d <- llply(depth_slice_seq, .parallel=getOption('AQP_parallel', default=FALSE), .progress=progress, .fun=function(i, su=s.unrolled) 
	  {
	  
	  ## this could be a source of slowness, esp. the t()
	  ps <- sapply(su, function(dz, z_i=depth_slice_seq[i]) { dz[z_i,] })
	  sp <- t(ps)
	  
		# compute distance metric for this depth
		# distance metric has large effect on results
		# Gower's distance gives the best looking results, and automatically standardizes variables
		
		## this is where we run into memory-size limitations
		## an ff object would help here... however it can not preserve all of the information 
		## that a list can... we would need to store these data as raw matrices
	  
	  ## TODO: don't call daisy on bogus input data, temp fix: disable warnings
	  ## if all of the input to daisy is NA, then we get warnings from min() and max()
	  ## this happens when we set a max depth that is beyond most profiles
	  d.i <- daisy(sp, metric='gower')
		return(d.i)
	  }
	)
	options(ow)
	
	## TODO: does this actually do anything?
	# clean-up
	rm(s.unrolled) ; gc()	
	
	# print total size of D
	message(paste(" [", round(object.size(d) / 1024^2, 2), " Mb]", sep=''))
	
	# should NA in the dissimilarity matrix be replaced with max(D) ?
	if(replace_na)
		{
		# replace all NA with the MAX distance between any observations
		# note that down deep, there may not be enough data for any pair-wise comparisons
		# therefore, we should not attempt to calculate max() on a matrix of all NA
		max.distance.vect <- sapply(d, function(i) if(all(is.na(i))) NA else max(i, na.rm=TRUE))
		max.distance <- max(max.distance.vect, na.rm=TRUE)
		
		## note: this will not work with sample_interval set
		# should we use a more expensive approach, that uses the soil/non_soil flag?
		if(add_soil_flag)
			{
			# kind of messy: re-using an object like this
			for(i in 1:length(d))
				{
				d_i <- as.matrix(d[[i]])

				# set all pairs that are made between deep vs. shallow soil
				# to the maximum distance- by row and column
				cells.with.na.rows <- which(is.na(d_i[, which(soil.matrix[i, ])]))
				cells.with.na.cols <- which(is.na(d_i[which(soil.matrix[i, ]), ]))
				
				d_i[, which(soil.matrix[i, ])][cells.with.na.rows] <- max.distance
				d_i[which(soil.matrix[i, ]), ][cells.with.na.cols] <- max.distance
				
				# convert back to dist object
				d_i <- as.dist(d_i)
				
				# copy original attributes
				attributes(d_i) <- attributes(d[[i]])
				
				# save back to original position in list
				d[[i]] <- d_i
				}
			
			# remove the soil.matrix object to save some space
			rm(soil.matrix) ; gc()
			}
		# use a less expensive approach, where all NA are replaced by the max distance
		else
			{
			d <- lapply(d, function(d_i) 
				{
				cells.with.na <- which(is.na(d_i))
				d_i[cells.with.na] <- max.distance
				return(d_i)
				} )
			}
		}
	
	
	
	# perform depth-weighting 	
	for(i in seq_along(depth_slice_seq))
		d[[i]] <- d[[i]] * w[i]
	
	
	# optionally return the distances for each depth, after weighting
	if(return_depth_distances)
		return(d)
	
	
	## this could be a source of slow-ness: t()
	# compute the total distance, for all dept intervals,
	# by pedon:
	# consider using mean diss, or something different that total
	message("Computing Profile Total Dissimilarities")
	d.vect <- colSums(t(sapply(d, '[')), na.rm=TRUE)
	
	# remove list of dissimilarities to save RAM
	rm(d) ; gc()
	
	# now make into a combined distance matrix
	m.ref <- lower.tri(matrix(ncol=n.profiles, nrow=n.profiles), diag=FALSE)
	m.ref[which(m.ref == FALSE)] <- NA
	m.ref[which(m.ref)] <- d.vect
	
	# remove unformatted disimilarities
	rm(d.vect) ; gc()
	
	# coerce to 'dist' class
	D <- as.dist(m.ref)
	
	# update labels from our list of hz-dissimilarities
	attr(D, 'Labels') <- levels(s$id)
	
	# add distance metric
	attr(D, 'Distance Metric') <- 'Gower'
	
	# optionally rescale to 0-1
	# this is important when incorporating site data
	# causes problems for some functions like sammon
	if(rescale.result)
		D <- rescaler(D, type='range')
	
	# return the distance matrix, class = 'dist'
	return(D)	
	}


##############
## S4 stuff ##
##############

## NOTE: don't mess with this!
# setup generic function
if (!isGeneric("profile_compare"))
  setGeneric("profile_compare", function(s, ...) standardGeneric("profile_compare"))

# temp interface to SPC class objects
setMethod(f='profile_compare', signature='SoilProfileCollection',
  function(s, vars, rescale.result=FALSE, ...){
  
  # default behavior: do not normalize D
  	
  # extract horizons
  s.hz <- horizons(s)
  
  # extract site
  s.site <- site(s)
  sn <- names(s.site)
  
  # check for any site data, remove and a save for later
  if(any(vars %in% sn)) {
  	
  	# extract site-level vars
  	matching.idx <- na.omit(match(sn, vars))
  	site.vars <- vars[matching.idx]
  	
  	# remove from hz-level vars
  	vars <- vars[-matching.idx]
  	
  	
  	## TODO: allow user to pass-in variable type information
  	# compute dissimilarty on site-level data: only works with 2 or more variables
  	# rescale to [0,1]
  	if(length(site.vars) >= 2) {
  		message(paste('site-level variables included:', paste(site.vars, collapse=', ')))
  		d.site <- daisy(s.site[, site.vars], metric='gower')
  		d.site <- rescaler(d.site, type='range')
  		
  		# reset default behavior of hz-level D
  		rescale.result=TRUE
  		
  		## TODO: there might be cases where we get an NA in d.site ... seems like it happens with boolean variables
  		## ... but why ? read-up on daisy
  		if(any(is.na(d.site))) {
  			warning('NA in site-level dissimilarity matrix, replacing with min dissimilarity', call.=FALSE)
  			# we have re-scaled to [0,1] so D_min is 0
  			d.site[which(is.na(d.site))] <- 0
  		}
  			
  		## TODO: ordering of D_hz vs D_site ... assumptions safe?
  	}
  	
  	else
  		warning("cannot compute site-level dissimilarity with fewer than 2 variables", call.=FALSE)	
  }
  
  # setup a dummy D_site
  else
  	d.site <- NULL
  
  ## 
  ## TODO: update this next part
  ##
  # add old-style, hard-coded {id, top, bottom} column names        
  s.hz$id <- s.hz[[idname(s)]]
  hzDepthCols <- horizonDepths(s)
  s.hz$top <- s.hz[[hzDepthCols[1]]]
  s.hz$bottom <- s.hz[[hzDepthCols[2]]]
  
  # invoke data.frame method
  res <- profile_compare(s.hz, vars=vars, rescale.result=rescale.result, ...)
  
  # if we have site-level data and a valid D_site
  # combine via weighted average: using weights of 1 for now
  if(inherits(d.site, 'dist')) {
  	res <- 	(res + d.site) / 2
  }
  
  # result is a distance matrix
  return(res)
  }
)

# temp interface for dataframes
setMethod(f='profile_compare', signature='data.frame', definition=pc)
