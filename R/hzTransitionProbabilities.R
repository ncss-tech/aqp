# generate transition probability matrix from horizon designations
#' Horizon Transition Probabilities
#'
#' Functions for creating and working with horizon (sequence) transition
#' probability matrices.
#'
#' See the following tutorials for some ideas: \describe{ \item{horizon
#' designation
#' TP}{\url{http://ncss-tech.github.io/AQP/aqp/hz-transition-probabilities.html}}
#' \item{soil color
#' TP}{\url{http://ncss-tech.github.io/AQP/aqp/series-color-TP-graph.html}} }
#'
#' @aliases hzTransitionProbabilities genhzTableToAdjMat mostLikelyHzSequence
#' @param x A \code{SoilProfileCollection} object.
#' @param name A horizon level attribute in \code{x} that names horizons.
#' @param loopTerminalStates should terminal states loop back to themselves?
#' This is useful when the transition probability matrix will be used to
#' initialize a \code{markovchain} object. See examples below.
#' @return The function \code{hzTransitionProbabilities} returns a square
#' matrix of transition probabilities. See examples.
#'
#' The function \code{genhzTableToAdjMat} returns a square adjacency matrix.
#' See examples.
#'
#' The function \code{mostLikelyHzSequence} returns the most likely sequence of
#' horizons, given a \code{markovchain} object initialized from horizon
#' transition probabilities and an initial state, \code{t0}. See examples.
#' @note These functions are still experimental and subject to change.
#' @author D.E. Beaudette
#' @seealso \code{\link{generalize.hz}}
#' @keywords manip
#' @export
#' @examples
#'
#' data(sp4)
#' depths(sp4) <- id ~ top + bottom
#'
#' # horizon transition probabilities: row -> col transitions
#' (tp <- hzTransitionProbabilities(sp4, 'name'))
#'
#'
#' \dontrun{
#' ## plot TP matrix with functions from sharpshootR package
#' library(sharpshootR)
#' par(mar=c(0,0,0,0), mfcol=c(1,2))
#' plot(sp4)
#' plotSoilRelationGraph(tp, graph.mode = 'directed', edge.arrow.size=0.5)
#'
#' ## demonstrate genhzTableToAdjMat usage
#' data(loafercreek, package='soilDB')
#'
#' # convert contingency table -> adj matrix / TP matrix
#' tab <- table(loafercreek$hzname, loafercreek$genhz)
#' m <- genhzTableToAdjMat(tab)
#'
#' # plot
#' par(mar=c(0,0,0,0), mfcol=c(1,1))
#' plotSoilRelationGraph(m, graph.mode = 'directed', edge.arrow.size=0.5)
#'
#'
#' ## demonstrate markovchain integration
#' library(markovchain)
#' tp.loops <- hzTransitionProbabilities(sp4, 'name', loopTerminalStates = TRUE)
#'
#' # init new markovchain from TP matrix
#' mc <- new("markovchain", states=dimnames(tp.loops)[[1]], transitionMatrix = tp.loops)
#'
#' # simple plot
#' plot(mc, edge.arrow.size=0.5)
#'
#' # check absorbing states
#' absorbingStates(mc)
#'
#' # steady-state:
#' steadyStates(mc)
#' }
#'
hzTransitionProbabilities <- function(x, name = GHL(x, required = TRUE), loopTerminalStates = FALSE) {

  # get all horizons
  h <- horizons(x)

  # sanit checks: no missing or NA horizon designation allowed
  idx <- which(h[[name]] == '' | is.na(h[[name]]) )
  if(length(idx) > 0) {
    message('NA horizon names have been removed from TP matrix')
    h <- h[-idx, ]
  }

  # get all hz names
  hz.names <- sort(unique(h[[name]]))
  n.names <- length(hz.names)
  # get profile IDs and depth column names
  # note that we cannot use the full set of IDs, as NA and "" hz names have been filtered
  # convert to character in case all IDs are integers, this makes it possible to address our list by ID
  id.name <- idname(x)
  pIDs <- as.character(unique(h[[id.name]]))
  dc <- horizonDepths(x)
  # init TP matrix with 0's
  m <- matrix(ncol=n.names, nrow=n.names, data = 0)
  # row / col names are entire set of hz names
  dimnames(m) <- list(hz.names, hz.names)

  # split by profile
  h.l <- split(h, h[[id.name]])

  # iterate over profiles
  for(i in pIDs) {
    # the current profile: hz names and top depths
    this.profile <- h.l[[i]][, c(name, dc[1])]
    # sort names by top depth, ascending order
    z <- this.profile[[name]][order(this.profile[, 2])]

    # transition probabilities require at least 2 horizons
    if(length(z) > 1) {
      # iterate over names
      for(j in 1:(length(z)-1)){
        # increment the current transition by 1
        m[z[j], z[j+1]] <- m[z[j], z[j+1]] + 1
      }
    }
  }

  # convert to probabilities by row
  m <- sweep(m, 1, rowSums(m), '/')

  # rows wih all NaN are terminal states: they transition to nothing
  # optionally, create a loop (A -> A)
  # this ensures compatibility with markovchain package
  if(loopTerminalStates) {
    loop.hz <- names(which(apply(m, 1, function(i) all(is.nan(i)))))
    # if there are some loops, then set the diagonal for these states to 1
    if(length(loop.hz) > 0) {
      if(length(loop.hz) < 2)
        m[loop.hz, loop.hz] <- 1 # single replacement, no diagonal
      else
        diag(m[loop.hz, loop.hz]) <- 1 # multiple replacements, use diagonal
    }

  }

  # replace NaN with 0
  m[which(is.nan(m))] <- 0

  # test for ties
  f.ties <- function(i) {
    # 0 ties aren't important
    not.zero <- which(! zapsmall(i) == 0)
    # tabulate ties after rounding to 8 decimal places
    res <- table(round(i[not.zero], 8))
    if(any(res > 1))
      return(TRUE)
    else
      return(FALSE)
  }

  if(any(apply(m, 1, f.ties))) {
    warning('ties in transition probability matrix', call. = FALSE)
    attr(m, 'ties') <- TRUE
  }
  else
    attr(m, 'ties') <- FALSE


  return(m)
}
