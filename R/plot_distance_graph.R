# TODO: better colors
# take a look at between-profile D
# for the range of indexes

#' Between Individual Distance Plot
#'
#' Plot pair-wise distances between individuals as line segments.
#'
#' By default all individuals are plotting on the same axis. When there are
#' more than about 10 individuals, the plot can become quite messy. See
#' examples below for ideas.
#'
#' @param D distance matrix, should be of class 'dist' or compatible class
#' @param idx an integer sequence defining which individuals should be compared
#' @return No value is returned.
#' @author Dylan E Beaudette
#' @seealso \code{\link{sp2}}, \code{\link{profile_compare}}
#' @references http://casoilresource.lawr.ucdavis.edu/
#' @keywords hplot
#' @export
#' @examples
#'
#' data(sp2)
#' depths(sp2) <- id ~ top + bottom
#' d <- NCSP(
#'   sp2,
#'   vars = c('prop', 'field_ph', 'hue', 'value'),
#'   maxDepth = 100,
#'   k = 0.01
#' )
#'
#' par(mfcol=c(3,1), mar=c(2.5,4.5,1,1))
#' plot_distance_graph(d, idx=1:6)
#' plot_distance_graph(d, idx=7:12)
#' plot_distance_graph(d, idx=12:18)
plot_distance_graph <- function(D, idx=1:dim(as.matrix((D)))[1]) {
	# convert to full matrix representation
	# careful with very large D
	d.mat <- as.matrix(D)

	# mask out NA
	d.mat[d.mat == 0] <- NA

	# get number of individuals
	n <- dim(d.mat)[1]

	# get the original labels
	labs <- attr(D, 'Labels')
	# abbreviate them
	labs <- abbreviate(labs, minlength=3)

	# get the range in D and rescale to {0,1}
	d.range <- range(d.mat, na.rm=TRUE)
	d.mat <- d.mat / d.range[2]

	# empty plot region
	plot(1,1, type='n', xlim=c(1, n), ylim=c((d.range[1]/d.range[2])-0.1, 1),
	axes=FALSE, xlab='', ylab='Relative Distance')

	# add embellishments
	box()
	axis(2, las=2)
	# use original labels
	axis(1, at=1:n, labels=names(labs), las=2, cex.axis=0.75)
	grid(col=grey(0.25))

	# add lines, labels, and symbols
	for(i in idx) {
		# which individual is the most similar (min distance)
		min.idx <- which.min(d.mat[, i])
		# plot a line for each individual
		lines(1:n, d.mat[, i], type='b', pch=NA, col=(i %% 5) + 1, lty=(i %% 3) + 1)
		# label each line at nodes, except for the node of max similarity (min distance)
		text(c(1:n)[-min.idx], d.mat[-min.idx, i], label=labs[i], font=1, cex=0.75, col=(i %% 5) + 1)

		# mark the pair with min distance
		points(min.idx, d.mat[min.idx, i], cex=2, pch='*', col=(i %% 5) + 1)
		text(min.idx, d.mat[min.idx, i], label=labs[i], cex=0.78, font=2, pos=1, col=(i %% 5) + 1)
		}
	}

