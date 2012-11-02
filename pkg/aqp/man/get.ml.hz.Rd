\name{get.ml.hz}
\alias{get.ml.hz}
\title{Determine ML Horizon Boundaries}
\description{This function accepts input from \code{slab()} along with a vector of horizon names, and returns a data.frame of the most likely horizonat boundaries.}
\usage{
get.ml.hz(x, hz.names)
}
\arguments{
  \item{x}{output from \code{\link{slab.SPC}}}
  \item{hz.names}{vector of horizon names}
}

\details{This function is expecting that \code{x} is a data.frame containing columns identified in \code{nz.names}.}

\value{
A dataframe with the following columns:
	\item{hz}{horizon names}
	\item{top}{top boundary}
	\item{bottom}{bottom boundary}
	}

\author{D.E. Beaudette}
% \note{}

\seealso{\code{\link{slab.SPC}}}
\examples{
data(sp1)

# normalize horizon names: result is a factor
sp1$name <- generalize.hz(sp1$name, 
  new=c('O','A','B','C'), 
  pat=c('O', '^A','^B','C'))

# compute slice-wise probability so that it sums to contributing fraction, from 0-150
a <- slab(sp1, fm= ~ name, class_prob_mode=1, seg_vect=0:150)

# generate table of ML horizonation
get.ml.hz(a, c('O','A','B','C'))
}

\keyword{manip}
