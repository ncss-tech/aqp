% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SoilProfileCollection-slice-methods.R
\docType{methods}
\name{slice-methods}
\alias{slice-methods}
\alias{slice.fast}
\alias{get.slice}
\alias{slice}
\alias{slice,SoilProfileCollection-method}
\title{Slicing of SoilProfileCollection Objects}
\usage{
slice.fast(object, fm, top.down = TRUE, just.the.data = FALSE, strict = TRUE)

\S4method{slice}{SoilProfileCollection}(object, fm, top.down = TRUE, just.the.data = FALSE, strict = TRUE)

get.slice(h, id, top, bottom, vars, z, include = "top", strict = TRUE)
}
\arguments{
\item{object}{a SoilProfileCollection}

\item{fm}{A formula: either \code{integer.vector ~ var1 + var2 + var3} where
named variables are sliced according to \code{integer.vector} OR where all
variables are sliced according to \code{integer.vector}: \code{integer.vector ~ .}.}

\item{top.down}{logical, slices are defined from the top-down: \code{0:10} implies 0-11 depth units.}

\item{just.the.data}{Logical, return just the sliced data or a new \code{SoilProfileCollection} object.}

\item{strict}{Check for logic errors? Default: \code{TRUE}}

\item{h}{Horizon data.frame}

\item{id}{Profile ID}

\item{top}{Top Depth Column Name}

\item{bottom}{Bottom Depth Column Name}

\item{vars}{Variables of Interest}

\item{z}{Slice Depth (index).}

\item{include}{Either \code{'top'} or \code{'bottom'}. Boundary to include in slice. Default: \code{'top'}}
}
\value{
Either a new \code{SoilProfileCollection} with data sliced according to \code{fm}, or a \code{data.frame}.
}
\description{
A method for "slicing" of SoilProfileCollection objects into constant depth intervals. Now deprecated, see \verb{[dice()]}.
}
\section{Details}{
 By default, slices are defined from the top-down:
\code{0:10} implies 0-11 depth units.
}

\examples{

library(aqp)

# simulate some data, IDs are 1:20
d <- lapply(1:20, random_profile)
d <- do.call('rbind', d)

# init SoilProfileCollection object
depths(d) <- id ~ top + bottom
head(horizons(d))

# generate single slice at 10 cm
# output is a SoilProfileCollection object
s <- dice(d, fm = 10 ~ name + p1 + p2 + p3)

# generate single slice at 10 cm, output data.frame
s <- dice(d, 10 ~ name + p1 + p2 + p3, SPC = FALSE)

# generate integer slices from 0 - 26 cm
# note that slices are specified by default as "top-down"
# result is a SoilProfileCollection
# e.g. the lower depth will always by top + 1
s <- dice(d, fm = 0:25 ~ name + p1 + p2 + p3)
par(mar=c(0,1,0,1))
plotSPC(s)

# generate slices from 0 - 11 cm, for all variables
s <- dice(d, fm = 0:10 ~ .)
print(s)

# compute percent missing, for each slice,
# if all vars are missing, then NA is returned
d$p1[1:10] <- NA
s <- dice(d, 10 ~ ., SPC = FALSE, pctMissing = TRUE)
head(s)

\dontrun{
##
## check sliced data
##

# test that mean of 1 cm slices property is equal to the
# hz-thickness weighted mean value of that property
data(sp1)
depths(sp1) <- id ~ top + bottom

# get the first profile
sp1.sub <- sp1[which(profile_id(sp1) == 'P009'), ]

# compute hz-thickness wt. mean
hz.wt.mean <- with(
  horizons(sp1.sub),
  sum((bottom - top) * prop) / sum(bottom - top)
)

# hopefully the same value, calculated via slice()
s <- dice(sp1.sub, fm = 0:max(sp1.sub) ~ prop)
hz.slice.mean <- mean(s$prop, na.rm = TRUE)

# they are the same
all.equal(hz.slice.mean, hz.wt.mean)
}

}
\references{
D.E. Beaudette, P. Roudier, A.T. O'Geen, Algorithms for
quantitative pedology: A toolkit for soil scientists, Computers &
Geosciences, Volume 52, March 2013, Pages 258-268,
10.1016/j.cageo.2012.10.020.
}
\seealso{
\code{\link{slab}}
}
\author{
D.E. Beaudette
}
\keyword{manip}
\keyword{methods}
