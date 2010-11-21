\name{soil.slot}
\Rdversion{1.1}
\alias{soil.slot}
\alias{conditional.sd}
\alias{seg.summary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Soil Profile Slotting
}
\description{
Align a single soil property to a user-defined basis, and perform slice-wise aggregation.
}
\usage{
soil.slot(data, seg_size = NA, seg_vect = NA, 
use.wts = FALSE, strict = FALSE, user.fun = NULL, class_prob_mode=1)
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
If TRUE, then a column called 'wt' should be present in the source dataframe, and must make sense within the context of the data (i.e. area weights, etc.). Weighted means and standard deviations will be returned by depth-slice. Use with caution, this feature is still experimental. See examples below.
}
\item{strict}{should horizons be strictly checked for self-consistency? defaults to FALSE}
\item{user.fun}{User-defined function that should accept a vector and return a scalar. This function should know how to properly deal with unexpected, NA, NULL, or Inf values and trap them accordingly.}
\item{class_prob_mode}{Strategy for normalizing slice-wise probabilities, dividing by either: number of profiles with data at the current slice (class_prob_mode=1), or by the number of profiles in the collection (class_prob_mode=2). Mode 2 values will always sum to the contributing fraction, while mode 1 values will always sum to 1. Mode 2 is likely the best way to communicate horizon probability near the lower range in soil depth within a collection of soil profiles.}
}
\details{
The \code{conditional.sd} function is a wrapper to \code{sd}, that replicates the pre-R 2.8 behavior of \code{sd}. See the sample dataset 'sp1' documentation for further examples on how to used \code{soil.slot}. Basic error checking is performed to make sure that bottom and top horizon boundaries make sense. Note that the horizons should be sorted according to depth before using this function.

Data are returned according to the following:
A dataframe with slice-wise aggregation. When \code{prop} is numeric a dataframe is returned in the following format:
	\describe{
		\item{top}{The slice top boundary.}
		\item{bottom}{The slice bottom boundary.}
		\item{contributing_fraction}{The fraction of profiles contributing to the aggregate value, ranges from 1/n_profiles to 1.}
		\item{p.mean}{The slice-wise (optionally weighted) mean.}
		\item{p.sd}{An slice-wise (optionally weighted) standard deviation.}
		\item{p.q5}{The slice-wise 5th percentile.}
		\item{p.q25}{The slice-wise 25th percentile}
		\item{p.q50}{The slice-wise 50th percentile (median)}
		\item{p.q75}{The slice-wise 75th percentile}
		\item{p.q95}{The slice-wise 95th percentile}
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

\note{If a user-defined function (\code{user.fun}) is specified, care must be taken if sample size is used within the calculation _and_ slice sizes are > 1 depth unit.}

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

# default slotting-- 1cm intervals, 
# adjusting slice-wise probability with contributing fraction
a <- soil.slot(y, class_prob_mode=1)

# reshape into long format for plotting
a.long <- melt(a, id.var=c('top','bottom'))

a.long$variable <- factor(a.long$variable, levels=c('O','A','B','C','R'))

# plot horizon type proportions
xyplot(top ~ value | variable, data=a.long, subset=value > 0,
ylim=c(150, -5), type=c('S','g'), horizontal=TRUE, layout=c(4,1), col=1 )


## ajust probability to size of collection
a.1 <- soil.slot(y, class_prob_mode=2)

# reshape into long format for plotting
a.1.long <- melt(a.1, id.var=c('top','bottom'))

# group mode 1 and mode 2 data
g <- make.groups(mode_1=a.long, mode_2=a.1.long)
g$variable <- factor(g$variable, levels=c('O','A','B','C','R'))

# plot horizon type proportions
xyplot(top ~ value | variable, groups=which, data=g, subset=value > 0,
ylim=c(150, -5), type=c('S','g'), horizontal=TRUE, layout=c(4,1), 
auto.key=list(lines=TRUE, points=FALSE, columns=2),
par.settings=list(superpose.line=list(col=c(1,2))))





#
# 2. depth probability via contributing fraction
# note that this assumes that we are not missing data in 'prop'
# get around NA by making makeing a fake column filled with 1
# like this:
# sp1$prop <- 1
#
a <- soil.slot(sp1)
xyplot(top ~ contributing_fraction, data=a, 
ylim=c(250, -5), type='S', horizontal=TRUE, asp=4)


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


# 
# 3.3 use of weights
# 
data(sp1)

# some fake weights
wts <- data.frame(id=levels(sp1$id), wt=c(3,1,2,1,2,1,4,1,2))

# merge wtih original data
g <- merge(sp1, wts, by='id')

# generate horizon mid points
g$mid <- with(g, (bottom + top) / 2)

# aggregate and add upper/lower intervals via SD
a <- soil.slot(g, use.wts=TRUE)
a$upper <- with(a, p.mean + p.sd)
a$lower <- with(a, p.mean - p.sd)

a$wt.upper <- with(a, p.wtmean + p.wtsd)
a$wt.lower <- with(a, p.wtmean - p.wtsd)

# check influence of weights
plot(mid ~ prop, data=g, ylim=c(240,0), cex=sqrt(g$wt), xlim=c(-5,60))
lines(top ~ p.mean, data=a, lwd=2)
lines(top ~ upper, data=a, lty=2)
lines(top ~ lower, data=a, lty=2)

lines(top ~ p.wtmean, data=a, col=2, lwd=2)
lines(top ~ wt.upper, data=a, col=2, lty=2)
lines(top ~ wt.lower, data=a, col=2, lty=2)

# annotate with explanation
legend('bottomright', 
legend=c('wt = 1','wt = 2','wt = 3','wt = 4','un-weighted','weighted'), 
pch=c(1,1,1,1,NA,NA), lwd=c(1,1,1,1,2,2), lty=c(NA,NA,NA,NA,1,1), 
pt.cex=sqrt(c(1,2,3,4,1,1)), col=c(1,1,1,1,1,2), bty='n')




# 
# try again with larger segment sizes 
# 

# aggregate and add upper/lower intervals via SD
a <- soil.slot(g, use.wts=TRUE, seg_size=10)

a$upper <- with(a, p.mean + p.sd)
a$lower <- with(a, p.mean - p.sd)
a$wt.upper <- with(a, p.wtmean + p.wtsd)
a$wt.lower <- with(a, p.wtmean - p.wtsd)

# convert to long format
library(reshape)
a.long <- melt(a, id.var=c('top','bottom'), 
measure.var=c('p.mean','p.wtmean','upper','lower','wt.upper','wt.lower'))

# red lines are weighted
# point symbols are sized proportional to their weights
xyplot(cbind(top, bottom) ~ value, groups=variable, data=a.long, 
ylim=c(260,-10), ylab='Depth', xlab='Property',
par.settings=list(superpose.line=list(
col=c('black','red','black','black','red','red'), 
lwd=c(2,2,1,1,1,1), 
lty=c(1,1,2,2,2,2))), 
panel=function(...) {
panel.points(g$prop, g$mid, cex=sqrt(g$wt), col=1)
panel.depth_function(...)
}
)






}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
