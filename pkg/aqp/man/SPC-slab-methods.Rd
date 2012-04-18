\name{slab-methods}
\docType{methods}
\alias{slab}
\alias{slab,data.frame-method}
\alias{slab,SoilProfileCollection-method}

\title{Slab-Wise Aggregation of SoilProfileCollection Objects}
\description{Aggregate soil properties along user-defined `slabs`, and optionally within groups.}

\usage{
slab(data, fm, progress = 'none', seg_size = NA, seg_vect = NA, 
use.wts = FALSE, strict = FALSE, user.fun = NULL, 
class_prob_mode=1)
}

\arguments{
  \item{data}{a SoilProfileCollection or data.frame}
  
  \item{fm}{a formula: either `groups ~ var1 + var2 + var3' where named variables are aggregated within `groups' OR where named variables are aggregated across the entire collection ` ~ var1 + var2 + var3'}
  
  \item{progress}{'none' (default): argument passed to \code{ddply} and related functions, see \code{\link{create_progress_bar}} for all possible options; 'text' is usually fine.}
  
  \item{seg_size}{user-defined segment size, default is 1}
  
  \item{seg_vect}{User-defined segment structure: should start from 0, and deepest boundary should be deeper than the deepest soil profile in the collection. For example, if the deepest profile in the collection is 200 cm, then the following segmenting vector would be reasonable: \code{c(0,10,20,30,60,100,150,210)}. The resulting aggregation will automatically be truncated at 200 cm. The user is responsible for supplying sensible values.}
  
  \item{use.wts}{If TRUE, then a column called 'wt' should be present in the source dataframe, and must make sense within the context of the data (i.e. area weights, etc.). Weighted means, standard deviations, quantiles, and optionally proportions will be returned by depth-slice. See details and examples below.}
  
\item{strict}{Should horizons be strictly checked for self-consistency? defaults to FALSE}

\item{user.fun}{User-defined function that should accept a vector and return a scalar. This function should know how to properly deal with unexpected, NA, NULL, or Inf values and trap them accordingly.}

\item{class_prob_mode}{Strategy for normalizing slice-wise probabilities, dividing by either: number of profiles with data at the current slice (class_prob_mode=1), or by the number of profiles in the collection (class_prob_mode=2). Mode 2 values will always sum to the contributing fraction, while mode 1 values will always sum to 1. Mode 2 is likely the best way to communicate horizon probability near the lower range in soil depth within a collection of soil profiles.}
}

\section{Methods}{
\describe{
\item{data = "SoilProfileCollection"}{Typical usage, where input is a \code{\link{SoilProfileCollection}}.}
\item{data = "data.frame"}{Alternate method for special cases where input is a \code{data.frame}. This method is provided for backwards compatibility and extreme cases where promoting to \code{SoilProfileCollection} class objects is problematic. Note that this method requires the following columns in `data': `id', `top', `bottom'.}
}
}


\details{
Multiple continuous variables OR a single categorical (factor) variable can be aggregated within a call to \code{slab}.

Unweighted and weighted summary stats are computed with the Hmisc functions \code{wtd.mean}, \code{wtd.var}, and \code{wtd.quantile}. Weighted probabilities (proportions) will be implemented in a future release. See the sample dataset \code{\link{sp1}} documentation for further examples. Basic error checking is performed to make sure that top and bottom horizon boundaries make sense. If a user-defined function (\code{user.fun}) is specified, care must be taken if sample size is used within the calculation _and_ slice sizes are > 1 depth unit.
}

\value{
Output is returned in long format, such that slice-wise aggregates are returned once for each combination of grouping variable (optional) and variable described in the \code{fm} argument.

When a single numeric variable is used:
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

When a single factor variable is used, slice-wise probabilities for each level of that factor are returned as:
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

\references{http://casoilresource.lawr.ucdavis.edu/}
\author{Dylan E Beaudette}
\note{This function has replaced the previously defined `soil.slot.multiple' function as of aqp 0.98-8.58}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{slice}}
}
\examples{
data(sp3)

# add new grouping factor
sp3$group <- 1
sp3$group[as.numeric(sp3$id) > 5] <- 2
sp3$group <- factor(sp3$group)

# slot several variables at once
# within each level of 'group'
# using default parameters to soil.slot()
a <- slab(sp3, fm=group ~ L + A + B)

# pre-compute intervals
a$upper <- with(a, p.mean + p.sd)
a$lower <- with(a, p.mean - p.sd)

# check the results:
# note that 'group' is the column containing group labels
library(lattice)
xyplot(
top ~ p.mean | variable, data=a, groups=group, subscripts=TRUE, 
lower=a$lower, upper=a$upper, ylim=c(125,-5), alpha=0.5,
layout=c(3,1), scales=list(x=list(relation='free')),
panel=panel.depth_function, 
prepanel=prepanel.depth_function
)


# convert mean value for each variable into long format
library(reshape)
a.wide <- cast(a, group + top + bottom ~ variable, value=c('p.mean'))

## again, this time for a user-defined slab from 40-60 cm
a <- slab(sp3, fm=group ~ L + A + B, seg_vect=c(40,60))

# now we have weighted average properties (within the defined slab) for each variable, and each group
(a.wide <- cast(a, group + top + bottom ~ variable, value=c('p.mean')))

## this time, compute the weighted mean of selected properties, by profile ID
a <- slab(sp3, fm=id ~ L + A + B, seg_vect=c(40,60))
(a.wide <- cast(a, id + top + bottom ~ variable, value=c('p.mean')))


## aggregate the entire collection: 
## note the missing left-hand side of the formula
a <- slab(sp3, fm= ~ L + A + B)



## this will work in the next version of plyr
\dontrun{library(doSMP)

# setup global option
options('AQP_parallel'=TRUE)

# register workers (2 cores)
w <- startWorkers(workerCount=2, FORCE=TRUE)
registerDoSMP(w)

# run in parallel
a <- slab(sp3, fm=group ~ L + A + B, seg_size=5)

# stop workers (critical!)
stopWorkers(w)
}
}

\keyword{methods}
\keyword{manip}
