\name{fixOverlap}
\alias{fixOverlap}
\alias{findOverlap}

\title{Attempt to fix overlapping sketches when using relative horizontal spacing.}

\description{This is a very simple optimization algorithm for adjusting horizontal sketch positions until affected profiles are farter apart than a given threshold. Rank-ordering and boundary conditions are enforced on the adjustments. Failure to converge within \code{maxIter} results in an integer sequence.}

\usage{
fixOverlap(x, thresh = 0.6, adj = 0.2, 
min.x = 0.8, max.x = length(x) + 0.2, 
maxIter = 1000, trace = FALSE)
}

\arguments{
  \item{x}{vector of relative horizontal positions, one for each profile}
  \item{thresh}{horizontal threshold defining "overlap", must be < 1, ideal values likely in [0.3, 0.8]}
  \item{adj}{adjustments are tested within \code{runif(min=adj * -1, max=adj)}}
  \item{min.x}{left-side boundary condition}
  \item{max.x}{right-side boundary condition}
  \item{maxIter}{maximum number of iterations to attempt before giving up and returning integer sequence}
  \item{trace}{print diagnostics}
}


\value{a vector of the same length as \code{x}, preserving rank-ordering and boundary conditions.}

\author{D.E. Beaudette}

\note{This is a very naeve function and may fail to converge on a reasonable solution. SANN would be a much more robust framework.}


\examples{
# sample data
data(sp4)
depths(sp4) <- id ~ top + bottom

# proposed vector of relative positions, overlap likely
pos <- c(1, 1.1, 3, 4, 5, 5.2, 7, 8, 9, 10)

# try it
explainPlotSPC(sp4, name='name', relative.pos=pos)

# attempt to fix using an integer sequence, short-circut will prevent adjustments
explainPlotSPC(sp4, name='name', relative.pos=fixOverlap(1:10, trace=TRUE))

# attempt to adjust using defaults
explainPlotSPC(sp4, name='name', relative.pos=fixOverlap(pos, trace=TRUE))

# attempt to adjust and tinker with defaults
explainPlotSPC(sp4, name='name', relative.pos=fixOverlap(pos, adj = 0.2, trace=TRUE))

# repeatable adjustments
set.seed(10101)
explainPlotSPC(sp4, name='name', relative.pos=fixOverlap(pos, thresh = 0.7, trace=TRUE))

# more complex adjustments required
pos <- c(1, 2, 3, 3.3, 5, 5.1, 5.5, 8, 9, 9.1)

# tinker
explainPlotSPC(sp4, name='name', relative.pos=pos)
explainPlotSPC(sp4, name='name', relative.pos=fixOverlap(pos, trace=TRUE))
explainPlotSPC(sp4, name='name', relative.pos=fixOverlap(pos, thresh = 0.7, trace=TRUE))
explainPlotSPC(sp4, name='name', relative.pos=fixOverlap(pos, thres=0.7, adj = 0.2, trace=TRUE))

# no solution possible given these constraints
explainPlotSPC(sp4, name='name', relative.pos=fixOverlap(pos, thres=1, adj = 0.2, trace=TRUE))
}


\keyword{ manip }

