# take a look at between-profile D
# for the range of indexes
plot_distance_graph <- function(D, idx=1:dim(as.matrix((D)))[1])
	{
	# convert to full matrix representation
	# careful with very large D
	d.mat <- as.matrix(D)
	
	# mask out NA
	d.mat[d.mat == 0] <- NA
	
	# get number of individuals
	n <- dim(d.mat)[1]
	
	# get the range in D and rescale to {0,1}
	d.range <- range(d.mat, na.rm=TRUE)
	d.mat <- d.mat / d.range[2]
	
	# empty plot region
	plot(1,1, type='n', xlim=c(1, n), ylim=c(d.range[1]/d.range[2],1), 
	axes=FALSE, xlab='ID', ylab='Relative Distance')
	
	# add embellishments
	box()
	axis(2, las=2)
	axis(1, 1:n)
	grid(col=grey(0.25))
	
	# add lines, labels, and symbols
	for(i in idx)
		{
		# which individual is the most similar?
		min.idx <- which.min(d.mat[, i])
		lines(1:n, d.mat[, i], type='b', pch=NA, col=(i %% 5) + 1, lty=(i %% 3) + 1)
		text(1:n, d.mat[, i], label=i, font=2, cex=0.75, col=(i %% 5) + 1)		
		points(min.idx, d.mat[min.idx, i], cex=2.35)
		}
	}
	