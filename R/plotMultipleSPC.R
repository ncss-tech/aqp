
#' Soil Profile Group Labels
#'
#' Labels groups of soil profiles within soil profile sketches.
#'
#' See examples below for ideas.
#'
#' @param x0 integer indices to the first profile within each group
#' @param x1 integer indices to the last profile within each group
#' @param labels vector of group labels
#' @param y0 baseline depth used for group brackets
#' @param y1 depth used for start and end markers for group brackets (see
#' examples)
#' @param label.offset vertical offset of group labels from baseline
#' @param label.cex label size
#' @note This function is typically called by some other convenience function
#' such as \code{\link{plotMultipleSPC}}.
#' @author D.E. Beaudette
#' @seealso \code{\link{plotMultipleSPC}}
#' @examples
#'
#' # load sample data
#' data(sp3)
#' data(sp4)
#'
#' # convert soil colors
#' sp3$h <- NA ; sp3$s <- NA ; sp3$v <- NA
#' sp3.rgb <- with(sp3, munsell2rgb(hue, value, chroma, return_triplets=TRUE))
#' sp3[, c('h','s','v')] <- t(with(sp3.rgb, rgb2hsv(r, g, b, maxColorValue=1)))
#'
#' # promote to SoilProfileCollection
#' depths(sp3) <- id ~ top + bottom
#' depths(sp4) <- id ~ top + bottom
#'
#' # combine into a list
#' spc.list <- list(sp3, sp4)
#'
#' # compute group lengths and start/stop locations
#' n.groups <- length(spc.list)
#' spc.lengths <- sapply(spc.list, length)
#' n.pedons <- sum(spc.lengths)
#' group.starts <- c(1, 1 + cumsum(spc.lengths[-n.groups]))
#' group.ends <- cumsum(spc.lengths)
#'
#' # determine depths of first / last profile in each group
#' yy <- unlist(sapply(spc.list, function(i) profileApply(i, max)))
#' tick.heights <- yy[c(group.starts, group.ends)] + 2
#'
#' # plot 2 SoilProfileCollection objects on the same axis
#' par(mar=c(1,1,1,1))
#' plot(sp3, n=n.pedons)
#' plot(sp4, add=TRUE, x.idx.offset=group.ends[1], plot.depth.axis=FALSE, id.style='side')
#' # annotate groups
#' profileGroupLabels(x0=group.starts, x1=group.ends,
#' labels=c('Collection 1', 'Collection 2'), y0=120, y1=tick.heights)
#'
profileGroupLabels <- function(x0, x1, labels, y0=100, y1=98, label.offset=2, label.cex=0.75) {

  # sanity check: start / stop / label lengths should be equal
  if(! all.equal(length(x0), length(x1), length(labels)) )
    stop('start positions, stop positions, and number of labels must be equal', call. = FALSE)

  # pre-compute some elements
  n.groups <- length(x0)
  label.centers <- (x0 + x1) / 2

  # add group base lines
  segments(x0=x0, x1=x1, y0=y0, y1=y0)
  # add arrows to first / last group members
  arrows(x0=c(x0, x1), x1=c(x0, x1), y0=c(y0, y0), y1=y1, length=0.1)

  # annotate with group names
  text(x=label.centers, y=y0 + label.offset, labels=labels, cex=label.cex)
}

## TODO: labeling is not very helpful
## TODO: figure out intellegent recycling of arguments
## TODO: no mechanism for merged legends
## TODO: this doesn't take into account non-default figure geometry
# annotate profile plots with group labels, usually below
#' Plot Multiple SoilProfileCollection Objects
#'
#' Combine multiple SoilProfileCollection objects into a single profile sketch,
#' with annotated groups.
#'
#' See examples below for usage.
#'
#' @param spc.list a list of \code{SoilProfileCollection} objects
#' @param group.labels a vector of group labels, one for each
#' \code{SoilProfileCollection} object
#' @param args a list of arguments passed to \code{plotSPC}, one for each
#' \code{SoilProfileCollection} object
#' @param arrow.offset vertical offset in depth from base of start / end
#' profiles and group bracket arrows
#' @param bracket.base.depth baseline depth used for group brackets
#' @param \dots additional arguments to the first call to \code{plotSPC}
#' @note Multiple color legends for thematic profile sketches are not currently
#' supported, use with caution.
#' @author D.E. Beaudette and Ben Marshall
#' @seealso \code{\link{profileGroupLabels}}
#' @keywords hplots
#' @examples
#'
#' # load sample data
#' data(sp3)
#' data(sp4)
#'
#' # convert soil colors
#' sp3$h <- NA ; sp3$s <- NA ; sp3$v <- NA
#' sp3.rgb <- with(sp3, munsell2rgb(hue, value, chroma, return_triplets=TRUE))
#' sp3[, c('h','s','v')] <- t(with(sp3.rgb, rgb2hsv(r, g, b, maxColorValue=1)))
#'
#' # promote to SoilProfileCollection
#' depths(sp3) <- id ~ top + bottom
#' depths(sp4) <- id ~ top + bottom
#'
#' # combine into a list
#' spc.list <- list(sp3, sp4)
#'
#' # plot multiple SPC objects, with list of named arguments for each call to plotSPC
#' par(mar=c(1,1,3,3))
#' plotMultipleSPC(spc.list, group.labels=c('Collection 1', 'Collection 2'),
#' args=list(list(name='name', id.style='top'),
#' list(name='name', id.style='side')), bracket.base.depth=120)
#'
plotMultipleSPC <- function(spc.list, group.labels, args=rep(list(NA), times=length(spc.list)), arrow.offset=2, bracket.base.depth=95, ...) {

  # compute group stats
  n.groups <- length(spc.list)
  spc.lengths <- sapply(spc.list, length)
  n.pedons <- sum(spc.lengths)
  group.starts <- c(1, 1 + cumsum(spc.lengths[-n.groups]))
  group.ends <- cumsum(spc.lengths)

  # get depths + offset to start / end profiles
  yy <- unlist(sapply(spc.list, function(i) profileApply(i, max)))
  tick.heights <- yy[c(group.starts, group.ends)] + arrow.offset

  # setup plot with first SPC in list
  do.call(plotSPC, c(x=spc.list[[1]], n=n.pedons, na.omit(args[[1]]), ...))

  # iterate over remaining SPC objs
  if(n.groups > 1) {
    for(i in 2:n.groups) {
      this.obj <- spc.list[[i]]
      this.args <- na.omit(args[[i]])
      suppressMessages(
        do.call(
          plotSPC, c(x=this.obj, x.idx.offset=group.ends[i-1], add=TRUE, plot.depth.axis=FALSE, this.args)
        )
      )
    }
  }

  # annotate with group brackets
  profileGroupLabels(x0=group.starts, x1=group.ends, labels=group.labels, y0=bracket.base.depth, y1=tick.heights)
}




