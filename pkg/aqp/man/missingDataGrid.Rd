\name{missingDataGrid}
\alias{missingDataGrid}

\title{Missing Data Grid}
\description{Generate a levelplot of missing data from a SoilProfileCollection object.}
\usage{
missingDataGrid(s, s.fm, cols = NULL)
}

\arguments{
  \item{s}{a SoilProfilecollection object}
  \item{s.fm}{a `slicing` formula, defining depths and variable of interest, see \code{\link{slice}} for details}
  \item{cols}{a vector of colors}
}

\details{This function evaluates a `missing data fraction` based on slice-wise evaulation of named variables in a SoilProfileCollection object.}
\value{No object is returned, but a lattice graphic is printed to the active output device.}
\author{D.E. Beaudette}

\seealso{\code{\link{slice}}}
\examples{
## visualizing missing data
# 10 random profiles
require(plyr)
s <- ldply(paste('soil', 1:10), random_profile)

# randomly sprinkle some missing data
s[sample(nrow(s), 5), 'p1'] <- NA
s[sample(nrow(s), 5), 'p2'] <- NA
s[sample(nrow(s), 5), 'p3'] <- NA

# set all p4 and p5 attributes of `soil 1' to NA
s[which(s$id == 'soil 1'), 'p5'] <- NA
s[which(s$id == 'soil 1'), 'p4'] <- NA

# upgrade to SPC
depths(s) <- id ~ top + bottom

# plot missing data via slicing + levelplot
missingDataGrid(s, 0:100 ~ p1 + p2 + p3 + p4)
}

\keyword{hplots}
