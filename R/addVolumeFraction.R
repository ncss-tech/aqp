
# convert volume pct [0,100] into DF of points along a res x res grid
.volume2df <- function(v, res=10) {
	# test for no data
	if(is.na(v))
		return(data.frame())
	
	# convert volume pct into fraction
	v <- v / 100
	
	# init matrix with NA
	nc <- nr <- res
	cells <- (nc * nr)
	m <- matrix(nrow=nr, ncol=nc)
	m[] <- NA
	
	# determine number of cells required to symbolize volume fraction
	v.n <- round(v * cells)
	v.cells <- sample(1:cells, size=v.n)
	# mark cells with 1
	m[v.cells] <- 1
	
	# convert matrix into data.frame
	d <- expand.grid(x=(1:nr), y=(1:nc))
	d$val <- m[1:cells]
	# keep only those cells with val = 1
	d <- d[which(d$val == 1), ]
	
	# scrub extra columns and return
	d$val <- NULL
	return(d)
	}


# add volume fraction information to an existing SPC plot
addVolumeFraction <- function(x, colname, res=10, cex.min=0.1, cex.max=0.5, pch=1, col='black') {
	
	# get plotting details from aqp environment
	lsp <- get('last_spc_plot', envir=aqp.env)
	w <- lsp$width
	plot.order <- lsp$plot.order
	
	# horizontal shrinkage factor
	w.offset <- w / 5
	depth.offset <- 1
	
	# get top/bottom colnames
	hd <- horizonDepths(x)
	
	# iterate over profiles
	for(p.i in 1:length(x)) {
		
		h <- horizons(x[plot.order[p.i], ])
	
		# determine left/right extent of symbols
		x.center <- p.i
		x.left <- x.center - (w - w.offset)
		x.right <- x.center + (w - w.offset)
	
		# iterate over horizons
		for(h.i in 1:nrow(h)) {
			# convert this horizon's data
			v <- .volume2df(h[[colname]][h.i], res=res)
			
			## TODO: still throws errors
			# just those with data
			if(nrow(v) > 0 ) {
				# rescale x-coordinates
				v$x <- rescale(v$x, to=c(x.left, x.right))
		
				# rescale y-coordinates
				y.top <- h[[hd[1]]][h.i] + depth.offset
				y.bottom <- h[[hd[2]]][h.i] - depth.offset
				v$y <- rescale(v$y, to=c(y.top, y.bottom))
		
				# generate random symbol size
				p.cex <- runif(nrow(v), min=cex.min, max=cex.max)
		
				# add jittered points
				points(jitter(v$x), jitter(v$y), cex=p.cex, col=col, pch=pch)
			}
		}
	}	
}








