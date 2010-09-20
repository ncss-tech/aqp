\name{soil.slot}
\Rdversion{1.1}
\alias{soil.slot}
\alias{conditional.sd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Soil Profile Slotting
}
\description{
Align a single soil property to a user-defined basis, and perform slice-wise aggregation.
}
\usage{
soil.slot(data, seg_size = NA, seg_vect = NA, 
use.wts = FALSE, strict = FALSE, user.fun = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A dataframe representing a 'stack' of soil profiles and having the following format:
	\describe{
		\item{id}{An id that is unique across soil profiles within the dataframe.}
		\item{top}{The horizon top boundary, must be an integer.}
		\item{bottom}{The horizon bottom boundary, must be an integer.}
		\item{prop}{A property to be aggregated. can be numeric or a factor.}		
	}
}
  \item{seg_size}{
User-degined segment size, default is 1.
}
  \item{seg_vect}{
User-degined segment structure: should start from 0, and deepest boundary should be deeper than the deepest soil profile in the collection. For example, if the deepest profile in the collection is 200 cm, then the following segmenting vector would be reasonable: \code{c(0,10,20,30,60,100,150,210)}. The resulting aggregation will automatically be truncated at 200 cm. The user is responsible for supplying sensible values.
}
  \item{use.wts}{
If TRUE, then a column called 'wt' should be present in the source dataframe, and must make sense within the context of the data. Weighted means and standard deviations will be returned by depth-slice. Use with caution, this feature is still experimental and likely not quite correct.
}
\item{strict}{should horizons be strictly checked for self-consistency? defaults to FALSE}
\item{user.fun}{User-defined function that should accept a vector and return a scalar. This function should know how to properly deal with unexpected, NA, NULL, or Inf values and trap them accordingly.}
}
\details{
The \code{conditional.sd} function is a wrapper to \code{sd}, that replicates the pre-R 2.8 behavior of \code{sd}. See the sample dataset 'sp1' documentation for further examples on how to used \code{soil.slot}. Basic error checking is performed to make sure that bottom and top horizon boundaries make sense. Note that the horizons should be sorted according to depth before using this function.

Data are returned according to the following:
A dataframe with slice-wise aggregation. When \code{prop} is numeric a dataframe is returned in the following format:
	\describe{
		\item{top}{The slice top boundary.}
		\item{bottom}{The slice bottom boundary.}
		\item{contributing_fraction}{The fraction of profiles contributing to the aggregate value, ranges from 1/n_profiles to 1.}
		\item{p.mean}{The horizon-thickness weighted mean.}
		\item{p.sd}{An approximation of the horizon-thickness weighted mean: use with caution, may be too low.}
		\item{p.q5}{The horizon-thickness weighted 5th percentile.}
		\item{p.q25}{The horizon-thickness weighted 25th percentile}
		\item{p.q50}{The horizon-thickness weighted 50th percentile (median)}
		\item{p.q75}{The horizon-thickness weighted 75th percentile}
		\item{p.q95}{The horizon-thickness weighted 95th percentile}
	}

When \code{prop} is a factor variable, slice-wise probabilities for each level of \code{prop}:
	\describe{
		\item{top}{The slice top boundary.}
		\item{bottom}{The slice bottom boundary.}
		\item{contributing_fraction}{The fraction of profiles contributing to the aggregate value, ranges from 1/n_profiles to 1.}
		\item{A}{The slice-wise probability of level A}
		\item{B}{The slice-wise probability of level B}
		\item{\dots}{}
		\item{n}{The slice-wise probability of level n}
	}

}

\note{The calculation of standard deviation is correct when computing along 1 unit slices. When a user-defined segmenting vector is supplied, the resulting standard deviation is lower than it should be because we are inflating our sample size. When in doubt, use quantiles instead as these are properly weighted by horizon thickness.}

\value{See Details section above.}

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


# test that mean of 1cm slotted property is equal to the 
# hz-thickness weighted mean value of that property
sp1.sub <- subset(sp1, sub=id == 'P009')
hz.wt.mean <- with(sp1.sub, 
sum((bottom - top) * prop) / sum(bottom - top) 
)
a <- soil.slot(sp1.sub)
all.equal(mean(a$p.mean), hz.wt.mean)

#
# 1 standard usage, and plotting example
#

# slot at two different segment sizes
a <- soil.slot(sp1)
b <- soil.slot(sp1, seg_size=5)

# stack into long format
ab <- make.groups(a, b)
ab$which <- factor(ab$which, levels=c('a','b'), 
labels=c('1-cm Interval', '5-cm Interval'))

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
# convert name to a factor
y$prop <- factor(y$prop)
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
# 2. depth probability via contributing fraction
# note that this assumes that we are not missing data in 'prop'
# get around NA by making makeing a fake column filled with 1
# like this:
# sp1$prop <- 1
#
a <- soil.slot(sp1)
xyplot(top ~ contributing_fraction, data=a, ylim=c(250, -5), type='S', horizontal=TRUE, asp=4)


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
