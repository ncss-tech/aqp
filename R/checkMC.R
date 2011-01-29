
## once this is better tested, put the number of cores into an options() var
checkMC <- function(n=2)
	{
	plyr.ver <- as.numeric(paste(package_version(packageDescription("plyr")$Version)[[1,1]], package_version(packageDescription("plyr")$Version)[[1,2]], sep='.'))
	if( getOption('AQP_parallel', default=FALSE) & plyr.ver >= 1.4 & require(foreach) & require(doMC) )
		{
		# setup the parallel environment if it hasn't been already
		if(is.null(getDoParName()))
			{
			registerDoMC(cores=n)
		
			# if everything worked, then use it!
			cat(paste('using parallel computation [', n, ' cores]\n', sep=''))
			}
		
		parallel_flag <- TRUE
		}
	# otherwise don't
	else
	  {
# 	  cat('something is wrong, not using parallel computation')
	  parallel_flag <- FALSE
	  }
		
	return(parallel_flag)
	}
