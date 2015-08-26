\name{guessGenHzLevels}
\alias{guessGenHzLevels}
\title{Guess Appropriate Ordering for Generalized Horizon Labels}
\description{This function makes an (educated) guess at an appropriate set of levels for generalized horizon labels using the median of horizon depth mid-points.}
\usage{
guessGenHzLevels(x, hz = "genhz")
}

\arguments{
  \item{x}{a \code{SoilProfileCollection} object}
  \item{hz}{name of horizon-level attribute containing generalized horizon labels, see details}
}

\details{This function is useful when groups of horizons have been generalized via some method other than \code{generalize.hz}. For example, it may be useful to generalize horizons using labels derived from slice depths. The default sorting of these labels will not follow a logicial depth-wise sorting when converted to a factor. \code{guessGenHzLevels} does a good job of "guessing" the proper ordering of these labels based on median horizon depth mid-point.}

\value{a list:
  \item{levels}{a vector of levels sorted by median horizon depth mid-point}
  \item{median.depths}{a vector of median horizon mid-points}
}

\author{D.E. Beaudette}


\seealso{\code{\link{generalize.hz}}}

\examples{
# load some example data
data(sp1, package='aqp')

# upgrade to SoilProfileCollection
depths(sp1) <- id ~ top + bottom

# generalize horizon names
n <- c('O', 'A', 'B', 'C')
p <- c('O', 'A', 'B', 'C')
sp1$genhz <- generalize.hz(sp1$name, n, p)

# note: levels are in the order in which originally defined:
levels(sp1$genhz)

# generalize horizons by depth slice
s <- slice(sp1, c(5, 10, 15, 25, 50, 100, 150) ~ .)
s$slice <- paste0(s$top, ' cm')
# not a factor
levels(s$slice)

# the proper ordering of these new labels can be guessed from horizon depths
guessGenHzLevels(s, 'slice')

# convert to factor, and set proper order
s$slice <- factor(s$slice, levels=guessGenHzLevels(s, 'slice')$levels)

# that is better
levels(s$slice)
}

\keyword{ manip}

