\name{soil.slot}
\Rdversion{1.1}
\alias{soil.slot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Soil Profile Slotting
}
\description{
Align a single soil property to a user-defined basis.
}
\usage{
soil.slot(data, seg_size = NA, seg_vect = NA, return.raw = FALSE, use.wts = FALSE, compute.depth.prob = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A dataframe representing a 'stack' of soil profiles and having the following format:
	\describe{
		\item{id}{An id that is unique across soil profiles within the dataframe.}
		\item{top}{The horizon top boundary.}
		\item{bottom}{The horizon bottom boundary.}
		\item{prop}{A property to be aggregated. If numeric mean and standard deviation are returned, if character class-wise probabilities are returned.}
	}
}
  \item{seg_size}{
User-degined segment size, default is 1.
}
  \item{seg_vect}{
User-degined segment structure (needs work)
}
  \item{return.raw}{
If TRUE, un-aggregated data will be returned by depth-slice. Useful for debugging, or for the generation of box and whisker plots.
}
  \item{use.wts}{
If TRUE, then a column called 'wt' should be present in the source dataframe, and must make sense within the context of the data. Weighted means and standard deviations will be returned by depth-slice.
}
  \item{compute.depth.prob}{
If TRUE, depth probability is computed and returned. Setting a dummy 'prop' column to 1 in the source dataframe is suggested. See examples below.
}
}
\details{
See the documentation for the sample dataset 'sp1' for further examples.
}
\value{
A dataframe with slice-wise aggregation, weighted aggregation, class probabilities, or depth probabilities.
}
\references{http://casoilresource.lawr.ucdavis.edu/}
\author{Dylan E Beaudette}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sp1}, \link{unroll}}, \code{\link{soil.slot.multiple}}
}
\examples{
# setup environment
library(reshape)
library(lattice)

# load example data
data(sp1)

#
# 1 standard usage, and plotting example
#

# slot at two different segment sizes
a <- soil.slot(sp1)
b <- soil.slot(sp1, seg_size=5)

# stack into long format
ab <- make.groups(a, b)
ab$which <- factor(ab$which, levels=c('a','b'), labels=c('1-cm Interval', '5-cm Interval'))

# manually add mean +/- SD
ab$upper <- with(ab, p.mean+p.sd)
ab$lower <- with(ab, p.mean-p.sd)

# use mean +/- 1 SD
# custom plotting function for uncertainty viz.
xyplot(top ~ p.mean | which, data=ab, ylab='Depth',
xlab='mean bounded by +/- 1 SD',
lower=ab$lower, upper=ab$upper, ylim=c(250,-5), alpha=0.5, 
panel=panel.depth_function, 
prepanel=prepanel.depth_function,
layout=c(2,1), scales=list(x=list(alternating=1))
)

# use median and IQR
# custom plotting function for uncertainty viz.
xyplot(top ~ p.q50 | which, data=ab, ylab='Depth',
xlab='median bounded by 25th and 75th percentiles',
lower=ab$p.q25, upper=ab$p.q75, ylim=c(250,-5), alpha=0.5, 
panel=panel.depth_function, 
prepanel=prepanel.depth_function,
layout=c(2,1), scales=list(x=list(alternating=1))
)



#
# 1.1 try slotting categorical variables
#

# normalize horizon names:
sp1$name[grep('O', sp1$name)] <- 'O'
sp1$name[grep('A[0-9]', sp1$name)] <- 'A'
sp1$name[grep('AB', sp1$name, ignore.case=TRUE)] <- 'A'
sp1$name[grep('BA', sp1$name)] <- 'B'
sp1$name[grep('Bt', sp1$name)] <- 'B'
sp1$name[grep('Bw', sp1$name)] <- 'B'
sp1$name[grep('C', sp1$name)] <- 'C'
sp1$name[grep('R', sp1$name)] <- 'R'

# generate new data for testing soil.slot()
y <- with(sp1, data.frame(id=id, top=top, bottom=bottom, prop=name))
# convert name back to a character
y$prop <- as.character(y$prop)
# fix factor levels
y$id <- factor(y$id)

# default slotting-- 1cm intervals
a <- soil.slot(y)

# reshape into long format for plotting
a.long <- melt(a, id.var=c('top','bottom'))

a.long$variable <- factor(a.long$variable, levels=c('O','A','B','C','R'))

# plot horizon type proportions
xyplot(top ~ value | variable, data=a.long, subset=value > 0,
ylim=c(150, -5), type=c('S','g'), horizontal=TRUE, layout=c(4,1), col=1 )


#
# 2. depth probability
#
y <- with(sp1, data.frame(id=id, top=top, bottom=bottom, prop=1))
a <- soil.slot(y, compute.depth.prob=TRUE)
xyplot(top ~ p.prop, data=a, ylim=c(250, -5), type='S', horizontal=TRUE, asp=4)


#
# 3.1 standard aggregation
#
a <- soil.slot(sp1)

# manually add mean +/- SD
a$upper <- with(a, p.mean+p.sd)
a$lower <- with(a, p.mean-p.sd)

# use custom plotting function for uncertainty viz.
xyplot(top ~ p.mean, data=a, 
lower=a$lower, upper=a$upper, ylim=c(250,-5), alpha=0.5, 
panel=panel.depth_function, 
prepanel=prepanel.depth_function
)


#
# 3.2 standard aggregation, by grouping variable
#
require(plyr)

# try str(sp1) for hints
sp1$group <- with(sp1, factor(group))
a <- ddply(sp1, .(group), .fun=soil.slot)

# manually add mean +/- SD
a$upper <- with(a, p.mean+p.sd)
a$lower <- with(a, p.mean-p.sd)



# use custom plotting function for uncertainty viz.
xyplot(
top ~ p.mean, data=a, groups=group, subscripts=TRUE, 
lower=a$lower, upper=a$upper, ylim=c(250,-5), alpha=0.5,
panel=panel.depth_function, 
prepanel=prepanel.depth_function
)




}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
