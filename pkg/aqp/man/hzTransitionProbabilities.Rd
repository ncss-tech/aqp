\name{hzTransitionProbabilities}
\alias{hzTransitionProbabilities}
\alias{genhzTableToAdjMat}
\alias{mostLikelyHzSequence}

\title{Horizon Transition Probabilities}
\description{Functions for creating and working with horizon (sequence) transition probability matrices.}
\usage{
hzTransitionProbabilities(x, name, loopTerminalStates = FALSE)

genhzTableToAdjMat(tab)

mostLikelyHzSequence(mc, t0)
}

\arguments{
  \item{x}{A \code{SoilProfileCollection} object.}
  \item{name}{A horizon level attribute in \code{x} that names horizons.}
  \item{loopTerminalStates}{should terminal states loop back to themselves? This is useful when the transition probability matrix will be used to initialize a \code{markovchain} object. See examples below.}
  \item{tab}{A cross-tabulation relating original horizon designations to new, generalized horizon designations.}
  \item{mc}{A \code{markovchain} object, initialized from a horizon sequence transition probability matrix with looped terminal states.}
  \item{t0}{Time-zero: a label describing an initial state within a \code{markovchain} object.}
}

\details{Details and related tutorial pending...}

\value{The function \code{hzTransitionProbabilities} returns a square matrix of transition probabilities. See examples.

The function \code{genhzTableToAdjMat} returns a square adjacency matrix. See examples.

The function \code{mostLikelyHzSequence} returns the most likely sequence of horizons, given a \code{markovchain} object initialized from horizon transition probabilities and an initial state, \code{t0}. See examples.}

\author{D.E. Beaudette}

\note{These functions are still experimental and subject to change.}


\seealso{\code{\link{generalize.hz}}}
\examples{
data(sp4)
depths(sp4) <- id ~ top + bottom

# horizon transition probabilities: row -> col transitions
(tp <- hzTransitionProbabilities(sp4, 'name'))


\dontrun{
## plot TP matrix with functions from sharpshootR package
library(sharpshootR)
par(mar=c(0,0,0,0), mfcol=c(1,2))
plot(sp4)
plotSoilRelationGraph(tp, graph.mode = 'directed', edge.arrow.size=0.5)

## demonstrate genhzTableToAdjMat usage
data(loafercreek, package='soilDB')

# convert contingency table -> adj matrix / TP matrix
tab <- table(loafercreek$hzname, loafercreek$genhz)
m <- genhzTableToAdjMat(tab)

# plot 
par(mar=c(0,0,0,0), mfcol=c(1,1))
plotSoilRelationGraph(m, graph.mode = 'directed', edge.arrow.size=0.5)


## demonstrate markovchain integration
library(markovchain)
tp.loops <- hzTransitionProbabilities(sp4, 'name', loopTerminalStates = TRUE)

# init new markovchain from TP matrix
mc <- new("markovchain", states=dimnames(tp.loops)[[1]], transitionMatrix = tp.loops)

# simple plot
plot(mc, edge.arrow.size=0.5)

# check absorbing states
absorbingStates(mc)

# steady-state:
steadyStates(mc)
}
}

\keyword{manip}

