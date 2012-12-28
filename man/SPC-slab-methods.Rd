\name{slab-methods}
\docType{methods}
\alias{slab}
\alias{slab,SoilProfileCollection-method}


\title{Slab-Wise Aggregation of SoilProfileCollection Objects}
\description{Aggregate soil properties along user-defined `slabs`, and optionally within groups.}

\usage{
# method for SoilProfileCollection objects
slab(object, fm, slab.structure=1, progress='none', strict=FALSE, slab.fun=.slab.fun.numeric.default, cpm=1, ...)
}

\arguments{
  \item{object}{a SoilProfileCollection}
  
  \item{fm}{A formula: either `groups ~ var1 + var2 + var3' where named variables are aggregated within `groups' OR where named variables are aggregated across the entire collection ` ~ var1 + var2 + var3'}
  
  \item{slab.structure}{A user-defined slab thickness (defined by an integer), or user-defined structure (numeric vector). See details below.}
  
  \item{progress}{'none' (default): argument passed to \code{ddply} and related functions, see \code{\link{create_progress_bar}} for all possible options; 'text' is usually fine.}
  
  \item{strict}{logical: should horizons be strictly checked for self-consistency?}
  
  \item{slab.fun}{Function used to process each 'slab' of data, returning a \code{data.frame} with a single row. See details.}
  
  \item{cpm}{Strategy for normalizing slice-wise probabilities, dividing by either: number of profiles with data at the current slice (cpm=1), or by the number of profiles in the collection (cpm=2). Mode 1 values will always sum to the contributing fraction, while mode 2 values will always sum to 1.}

	\item{\dots}{further arguments passsed to \code{slab.fun}}

}

  
  
  
\section{Methods}{
\describe{
\item{data = "SoilProfileCollection"}{Typical usage, where input is a \code{\link{SoilProfileCollection}}.}
}
}


\details{
\code{soil.slot} is used internally by the various \code{slab} methods, and should be be directly invoked by the user.

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
##
## basic examples
##
library(lattice)
library(grid)

# load sample data, upgrade to SoilProfileCollection
data(sp1)
depths(sp1) <- id ~ top + bottom

# aggregate entire collection with two different segment sizes
a <- slab(sp1, fm = ~ prop)
b <- slab(sp1, fm = ~ prop, slab.structure=5)

# check output
str(a)

# stack into long format
ab <- make.groups(a, b)
ab$which <- factor(ab$which, levels=c('a','b'), 
labels=c('1-cm Interval', '5-cm Interval'))

# plot median and IQR
# custom plotting function for uncertainty viz.
xyplot(top ~ p.q50 | which, data=ab, ylab='Depth',
			 xlab='median bounded by 25th and 75th percentiles',
			 lower=ab$p.q25, upper=ab$p.q75, ylim=c(250,-5), alpha=0.5, 
			 panel=panel.depth_function, 
			 prepanel=prepanel.depth_function,
			 cf=ab$contributing_fraction,
			 layout=c(2,1), scales=list(x=list(alternating=1))
			 )


##
## categorical variable example
##
library(reshape)

# normalize horizon names: result is a factor
sp1$name <- generalize.hz(sp1$name, 
													new=c('O','A','B','C'), 
													pat=c('O', '^A','^B','C'))

# compute slice-wise probability so that it sums to contributing fraction, from 0-150
a <- slab(sp1, fm= ~ name, cpm=1, slab.structure=0:150)

# reshape into long format for plotting
a.long <- melt(a, id.vars=c('top','bottom'), measure.vars=c('O','A','B','C'))

# plot horizon type proportions using panels
xyplot(top ~ value | variable, data=a.long, subset=value > 0,
			 ylim=c(150, -5), type=c('S','g'), horizontal=TRUE, layout=c(4,1), col=1 )

# again, this time using groups
xyplot(top ~ value, data=a.long, groups=variable, subset=value > 0,
			 ylim=c(150, -5), type=c('S','g'), horizontal=TRUE, asp=2)


# adjust probability to size of collection, from 0-150
a.1 <- slab(sp1, fm= ~ name, cpm=2, slab.structure=0:150)

# reshape into long format for plotting
a.1.long <- melt(a.1, id.vars=c('top','bottom'), measure.vars=c('O','A','B','C'))

# group mode 1 and mode 2 data
g <- make.groups(mode_1=a.long, mode_2=a.1.long)

# plot horizon type proportions
xyplot(top ~ value | variable, groups=which, data=g, subset=value > 0,
			 ylim=c(240, -5), type=c('S','g'), horizontal=TRUE, layout=c(4,1), 
			 auto.key=list(lines=TRUE, points=FALSE, columns=2),
			 par.settings=list(superpose.line=list(col=c(1,2))))


# apply slice-wise evaluation of max probability, and assign ML-horizon at each slice
(gen.hz.ml <- get.ml.hz(a, c('O','A','B','C')))


##
## multivariate examples
##
data(sp3)

# add new grouping factor
sp3$group <- 1
sp3$group[as.numeric(sp3$id) > 5] <- 2
sp3$group <- factor(sp3$group)

# upgrade to SPC
depths(sp3) <- id ~ top + bottom
site(sp3) <- ~ group

# aggregate several variables at once, using a custom aggregate function
# within each level of 'group'

# this function returns the mean +/- 1SD
mean.and.sd <- function(values) {
	m <- mean(values, na.rm=TRUE)
	s <- sd(values, na.rm=TRUE)
	upper <- m + s
	lower <- m - s
	res <- data.frame(mean=m, lower=lower, upper=upper)
	return(res)
	}

a <- slab(sp3, fm=group ~ L + A + B, slab.fun=mean.and.sd)

# check the results:
# note that 'group' is the column containing group labels
library(lattice)
xyplot(
top ~ mean | variable, data=a, groups=group, subscripts=TRUE, 
lower=a$lower, upper=a$upper, ylim=c(125,-5), alpha=0.5,
layout=c(3,1), scales=list(x=list(relation='free')),
panel=panel.depth_function, 
prepanel=prepanel.depth_function
)


# convert mean value for each variable into long format
library(reshape)

# note that depths are no longer in order 
a.wide <- cast(a, group + top + bottom ~ variable, value=c('mean'))

## again, this time for a user-defined slab from 40-60 cm
a <- slab(sp3, fm=group ~ L + A + B, slab.structure=c(40,60), slab.fun=mean.and.sd)

# now we have weighted average properties (within the defined slab) for each variable, and each group
(a.wide <- cast(a, group + top + bottom ~ variable, value=c('mean')))

## this time, compute the weighted mean of selected properties, by profile ID
a <- slab(sp3, fm= id ~ L + A + B, slab.structure=c(40,60), slab.fun=mean.and.sd)
(a.wide <- cast(a, id + top + bottom ~ variable, value=c('mean')))


## aggregate the entire collection, using default slab function (hdquantile)
## note the missing left-hand side of the formula
a <- slab(sp3, fm= ~ L + A + B)

}

\keyword{methods}
\keyword{manip}
