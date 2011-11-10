## see the convertColor() function from grDevices


# convert munsell Hue, Value, Chroma into RGB
# user can adjust how rgb() function will return an R-friendly color
# TODO if alpha is greater than maxColorValue, there will be an error
# TODO: use join() from plyr package for faster look-up
munsell2rgb <- function(the_hue, the_value, the_chroma, alpha=1, maxColorValue=1, return_triplets=FALSE)
	{
	# check for missing data
	if(missing(the_hue) | missing(the_chroma) | missing(the_value))
		stop('Must supply a valid Munsell color.')

	
	# check to make sure that each vector is the same length
	if(length(unique( c(length(the_hue),length(the_value),length(the_chroma)))) != 1)
		stop('All inputs must be vectors of equal length.')
	
	# load lookup table from our package
	data(munsell)
	
	## TODO: this is kind of slow
	# perform subset
	n.cols <- length(the_hue)
	s <- vector(mode='list', length=n.cols)
	for(i in 1:n.cols)
		{
		s.i <- subset(munsell, 
		select=c('r','g','b'), 
		# hue, value, and chroma are columns in munsell dataframe
		subset=hue == the_hue[i] & value == the_value[i] & chroma == the_chroma[i])
		
		# check for searches returning no matches
		if(nrow(s.i) == 0)
			s[[i]] <- data.frame(r=NA, g=NA, b=NA)
		else
			s[[i]] <- s.i
		}
	
	# convert to DF
	s.df <- ldply(s)
	
	# if the user wants the raw RGB triplets, give those back
	if(return_triplets)
		return(s.df)
	
	# keep track of NA values
	s.na <- which(is.na(s.df$r))
	
	# not really an ideal solution, but seems to work
	# if alpha > maxColorValue -- clamp alpha at maxColorValue
	if(alpha > maxColorValue)
		alpha <- maxColorValue
	
	# convert to R color
	# init an empy column
	s.df$soil_color <- NA
	
	# account for missing values if present
	if(length(s.na > 0))
		s.df$soil_color[-s.na] <- with(s.df[-s.na,], rgb(red=r, green=g, blue=b, alpha=alpha, maxColorValue=maxColorValue) )
	else
		s.df$soil_color <- with(s.df, rgb(red=r, green=g, blue=b, alpha=alpha, maxColorValue=maxColorValue) )
		
	return(s.df$soil_color)
	}
