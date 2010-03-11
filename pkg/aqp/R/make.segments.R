
# TODO: document this
make.segments <- function(df)
	{
	
	n_hz <- length(df$prop) / 2
	
	# lookup color
	ll <- levels(df$groups)	
	m <- match(unique(df$group), ll)
	
	# need at least 2 horizons
	if(n_hz > 1)
		{
		# re-make dataframe for plotting segments
		df.new <- data.frame(top=df$bnd[1:n_hz], bottom=df$bnd[(n_hz+1):length(df$prop)], prop=df$prop[1:n_hz])
		
		# vertical segments
		panel.segments(df.new$prop, df.new$top, df.new$prop, df.new$bottom, 
		lwd=trellis.par.get('superpose.line')$lwd, col=trellis.par.get('superpose.line')$col[m], 
		lty=trellis.par.get('superpose.line')$lty[m]) 
		 
		# horizontal segments
		panel.segments(df.new$prop[-n_hz], df.new$bottom[-n_hz], df.new$prop[-1], df.new$top[-1], 
		lwd=trellis.par.get('superpose.line')$lwd, col=trellis.par.get('superpose.line')$col[m], 
		lty=trellis.par.get('superpose.line')$lty[m])
		}
		
	else
		{
		print(paste('only 1 horizon, skipping!', df$groups[1]))
		}
	
	}
	