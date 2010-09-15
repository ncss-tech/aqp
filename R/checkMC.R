
## once this is better tested, put the number of cores into an options() var
checkMC <- function(n=2)
	{
	if(
	getOption('AQP_parallel', default=FALSE) & 
	as.numeric(packageDescription('plyr')$Version) >= 1.2 & 
	require(foreach) & 
	require(doMC)
	)
		{
		# setup the parallel environment if it hasn't been already
		if(is.null(getDoParName()))
			{
			registerDoMC(cores=n)
		
			# if everything worked, then use it!
			cat(paste('using parallel computation [', n, ' cores]\n', sep=''))
			}
		
		parallel <- TRUE
		}
	# otherwise don't
	else
		parallel <- FALSE
		
	return(parallel)
	}
