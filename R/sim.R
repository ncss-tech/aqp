# function is more useful when supplied with a meaningful sd for each horizon
sim <- function(x, n=1, iterations=25, hz.sd=2, min.thick=2) {	
	
  hd <- horizonDepths(x)
	h <- horizons(x)
	thick <- h[[hd[2]]] - h[[hd[1]]]
	
	# remove original depth columns
	h[[hd[1]]] <- NULL
	h[[hd[2]]] <- NULL
	
	# remove horizon ID so as not to create conflicts later on
	h[[hzidname(x)]] <- NULL
	
	# keep track of old id
	old.id.name <- idname(x)
	
	# sanity checks
	if(length(x) > 1)
		stop('this function can only simulate data from a SoilProfileCollection containing a single profile')
	
	if(length(thick) %% length(hz.sd) != 0)
		stop('the length of hz.sd must divide evenly into the number of horizons', call.=FALSE)
	
	# define function to wrap rnorm for use with outer
	rnorm.vect <- function(mean, sd, n) {
		rnorm(n=n, mean=mean, sd=sd)
	}
	rnorm.vect <- Vectorize(rnorm.vect, vectorize.args=c('mean', 'sd'))
	
	# allocate storage for simulated horizon depths
	l <- list()
	
	# generate n-simulated horizon depths
	for(i in 1:n) {
		# simulate
		s <- mapply(rnorm.vect, thick, hz.sd, n=iterations)
		
		# sample a single value per horizon, and use as the representative value
		s <- round(apply(s, 2, sample, size=1))
		
		# convert thickness values that are below min.thick -> min.thick
		s <- pmax(s, min.thick)
		
		# convert thickness -> depths
		s <- cumsum(s)
		# use a ID column name that isn't likely to conflict with existing column names
		d <- data.frame(.new_id=i, top=c(0, s[-length(s)]), bottom=s)
		
		# combine with original horizon data, and save to list element
		l[[i]] <- cbind(d, h)
	}
	
	# convert list -> data.frame
	x.s <- do.call(rbind, l)
	
	# upgrade to SoilProfileCollection
	depths(x.s) <- .new_id ~ top + bottom
	
  # copy over depth units
	depth_units(x.s) <- depth_units(x)
	
	# move old ID into @site
	fm <- as.formula(paste0('~ ', old.id.name))
	site(x.s) <- fm
	
	# done
	return(x.s)
}
