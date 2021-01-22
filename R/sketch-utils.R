
.interpretHorizonColor <- function(h, color, default.color, col.palette, col.palette.bias, n.legend) {

  # this is optionally replaced with real data when using thematic colors
  color.legend.data <- NULL

  # toggle as needed for more room
  multi.row.legend <- FALSE
  # multi-row legend indices
  leg.row.indices <- NULL

  ## TODO: manually setting color=NULL will throw an error
  # think about how to best handle this
  # if(is.null(color)) {
  #
  # }

  # short-circuit: if all h[[color]] are NA the following logic will not reliably work
  # this is because sometimes all NA are interpreted as logical vectors
  if(all(is.na(h[[color]]))) {
    h[[".color"]] <- NA
  } else {

    # there is at least 1 non-NA color to work with

    # 1. numeric vector, rescale and apply color ramp
    if(is.numeric(h[[color]])) {

      # generate color ramp function
      cr <- colorRamp(col.palette, bias = col.palette.bias)

      if(!requireNamespace("scales", quietly = TRUE))
        stop("package `scales` is required", call.=FALSE)

      # note that this may contain NAs
      c.rgb <- cr(scales::rescale(h[[color]]))
      cc <- which(complete.cases(c.rgb))
      h$.color <- NA

      # convert non-NA values into colors
      h$.color[cc] <- rgb(c.rgb[cc, , drop = FALSE], maxColorValue=255)

      # generate range / colors for legend
      pretty.vals <- pretty(h[[color]], n = n.legend)

      # truncate to 3 signif vals and convert to character for correct interpretation of floating point values
      leg.pretty.vals <- as.character(signif(pretty.vals, 3))

      # special case: there are < 3 unique values -> convert to factor
      # previous calculations are ignored
      low.n.test.vals <- as.character(signif(h[[color]], digits = 3))
      if(length(unique(na.omit(low.n.test.vals))) < 3) {
        # replace with character representation with 3 significant digits
        h[[color]] <- low.n.test.vals
        message('less than 3 unique values, converting to factor')
      }

      # put into a list for later
      color.legend.data <- list(
        legend = leg.pretty.vals,
        col = rgb(cr(scales::rescale(pretty.vals)), maxColorValue=255),
        multi.row.legend = multi.row.legend,
        leg.row.indices = leg.row.indices
      )
    }

    # 2. vector of categorical data
    if(is.character(h[[color]]) | is.factor(h[[color]])) {

      # testing if ALL valid colors
      if( all(.isColorValid(na.omit(h[[color]])))) {
        # YES: interpret values directly as colors
        h$.color <- h[[color]]
      } else {
        # NO: this is or can be converted into a factor
        if(!is.factor(h[[color]]))
          h[[color]] <- factor(h[[color]])

        # get color mapping levels after dropping missing levels
        h[[color]] <- droplevels(h[[color]])
        color.levels <- levels(h[[color]])

        # make a color mapping function
        if(!requireNamespace("scales", quietly = TRUE))
          stop("package `scales` is required", call.=FALSE)

        color.mapper <- scales::col_factor(
          palette = colorRampPalette(col.palette, bias = col.palette.bias)(length(color.levels)),
          domain = color.levels,
          na.color = default.color,
          ordered = TRUE
        )

        # apply color mapping
        h$.color <- color.mapper(h[[color]])

        # generate colors and labels for legend
        pretty.vals <- color.levels

        # interpret n.legend as max(items) / row
        n.leg.classes <- length(pretty.vals)

        # create more room via multiple calls to legend
        if(n.legend < n.leg.classes) {

          # make indices to two rows of legends
          # safely accounts for even / odd n.leg.classes
          leg.row.indices <- .splitLegend(n.leg.classes)

          # set flag for later
          multi.row.legend <- TRUE
        }

        # pack into a list for later use
        color.legend.data <- list(
          legend = pretty.vals,
          col = color.mapper(pretty.vals),
          multi.row.legend = multi.row.legend,
          leg.row.indices = leg.row.indices
        )

      }
    }

  }


  # if the color column doesn't exist, fill with NA
  if(is.null(h[[color]]))
    h[[".color"]] <- NA

  # fill missing colors with a reasonable default
  h[['.color']] <- ifelse(is.na(h[['.color']]), default.color, h[['.color']])

  # assemble results
  res <- list(
    colors = h[['.color']],
    color.legend.data = color.legend.data
    )

  return(res)
}





# split legend into two rows, and create indices
# any more classes than that and things become impossible to read
# n: total number of classes
.splitLegend <- function(n) {

  #  make enough room for even division of odd numbers
  n.per.row <- ceiling(n / 2)

  # make indices for first row
  row.1.idx <- seq(from=1, to=n.per.row)
  row.2.idx <- seq(from=n.per.row + 1, to=n)

  res <- list(
    row.1=row.1.idx,
    row.2=row.2.idx
  )

  return(res)
}


# Test for valid colors in vector `x`:
#   1. named colors from colors()
#   2. RGB / RGBA encoding of colors
.isColorValid <- function(x) {
  # check for named colors
  test.1 <- x %in% colors()

  # check for valid RGB
  test.2 <- grepl('^#[a-f0-9]{6}', x, ignore.case = TRUE)

  # check for valid RGBA colors
  test.3 <- grepl('^#[a-f0-9]{8}', x, ignore.case = TRUE)

  # must pass at least 1 test
  res <- test.1 | test.2 | test.3
  return(res)
}




#' @title Visual Explanation for \code{plotSPC}
#' @description Create a visual explanation for the many arguments to \code{plotSPC}. Call this function instead of \code{plotSPC}, all objects after \code{x} are passed on to \code{plotSPC}. Nearly all of the figures in the \href{https://ncss-tech.github.io/AQP/aqp/aqp-intro.html}{Introduction to SoilProfileCollection Objects tutorial} are created with this function.
#'
#' @author D.E. Beaudette
#' @seealso \code{\link{plotSPC}}
#' @keywords manip
#'
#' @param x a \code{SoilProfileCollection} object
#' @param \dots arguments passed to \code{\link{plotSPC}}
#'
#' @return a list of internally-used ordering vectors and graphical offsets / scaling factors
#'
#' Attempt to fix overlapping sketches when using relative horizontal spacing.
#'
#' This is a very simple optimization algorithm for adjusting horizontal sketch
#' positions until affected profiles are farther apart than a given threshold.
#' Rank-ordering and boundary conditions are enforced on the adjustments.
#' Failure to converge within \code{maxIter} results in an integer sequence.
#'
#' @keywords manip
#' @examples
#'
#' # sample data
#' data(sp4)
#' depths(sp4) <- id ~ top + bottom
#'
#' # proposed vector of relative positions, overlap likely
#' pos <- c(1, 1.1, 3, 4, 5, 5.2, 7, 8, 9, 10)
#'
#' # try it
#' explainPlotSPC(sp4, name='name', relative.pos=pos)
#'
#' # attempt to fix using an integer sequence, short-circut will prevent adjustments
#' explainPlotSPC(sp4, name='name', relative.pos=fixOverlap(1:10, trace=TRUE))
#'
#' # attempt to adjust using defaults
#' explainPlotSPC(sp4, name='name', relative.pos=fixOverlap(pos, trace=TRUE))
#'
#' # attempt to adjust and tinker with defaults
#' explainPlotSPC(sp4, name='name', relative.pos=fixOverlap(pos, adj = 0.2, trace=TRUE))
#'
#' # repeatable adjustments
#' set.seed(10101)
#' explainPlotSPC(sp4, name='name', relative.pos=fixOverlap(pos, thresh = 0.7, trace=TRUE))
#'
#' # more complex adjustments required
#' pos <- c(1, 2, 3, 3.3, 5, 5.1, 5.5, 8, 9, 9.1)
#'
#' # tinker
#' explainPlotSPC(sp4, name='name', relative.pos=pos)
#' explainPlotSPC(sp4, name='name', relative.pos=fixOverlap(pos, trace=TRUE))
#' explainPlotSPC(sp4, name='name', relative.pos=fixOverlap(pos, thresh = 0.7, trace=TRUE))
#' explainPlotSPC(sp4, name='name', relative.pos=fixOverlap(pos, thres=0.7, adj = 0.2, trace=TRUE))
#'
#' # no solution possible given these constraints
#' explainPlotSPC(sp4, name='name', relative.pos=fixOverlap(pos, thres=1, adj = 0.2, trace=TRUE))
#'
#'
explainPlotSPC <- function(x, ...) {
  plotSPC(x, id.style='side', ...)
  box()

  # get last plot parameters
  lsp <- get('last_spc_plot', envir=aqp.env)

  # get max depth by profile
  max.depths <- profileApply(x, max)

  # re-order max depths
  max.depths <- max.depths[lsp$plot.order]

  # apply y-offset and scaling factor
  scaled.max.depths <- lsp$y.offset + (lsp$scaling.factor * max.depths)
  scaled.depth.axis <- lsp$y.offset + (lsp$scaling.factor * pretty(1:max(x)))

  # suitable location for y-space annotation
  y.space.x <- 2.5

  # suitable location for x-space annotation
  # index of last profile + some
  x.space.x <- lsp$n + (length(x) * 0.05)
  # 95% of total scaled depths
  x.space.y <- max(scaled.max.depths) * 0.95

  # original profile index text y-coordinate
  # roughly 10% of the max(transformed depths)
  original.profile.idx.y <- lsp$y.offset + (-max(scaled.max.depths) * 0.08)

  # inspect plotting area, very simple to overlay graphical elements
  segments(x0 = lsp$x0, x1=lsp$x0, y0=lsp$max.depth, y1=scaled.max.depths, lty=3, lwd=2, col='darkgreen')

  # profiles are centered at integers, from 1 to length(obj)
  axis(1, line=0.25, at=round(lsp$x0, 2), cex.axis=0.75, font=4, col='darkgreen', col.axis='darkgreen', lwd=2)
  mtext('canvas x-coordinate', side=1, line=2.25, font=4, col='darkgreen')

  # y-axis is based on profile depths
  axis(2, line=0.25, at=scaled.depth.axis, cex.axis=0.75, font=4, las=1, col='blue', col.axis='blue', lwd=2)
  mtext('canvas y-coordinate', side=2, line=2.25, font=4, col='blue')

  # show extra y-space
  arrows(x0=y.space.x, x1=y.space.x, y0=0, y1=-lsp$extra_y_space, length = 0.08, code = 3, col='orange', lwd=1)
  text(x=y.space.x, y=-lsp$extra_y_space/2, labels = 'extra y-space', cex=0.65, pos=2, font=3, col='orange')
  text(x=y.space.x, y=-lsp$extra_y_space/2, labels = lsp$extra_y_space, cex=0.85, pos=4, font=2, col='orange')

  # show extra x-space
  arrows(x0=lsp$n, x1=lsp$n + lsp$extra_x_space, y0=x.space.y, y1=x.space.y, length = 0.08, code = 3, col='orange', lwd=1)
  text(x=x.space.x, y=x.space.y, labels = 'extra x-space', cex=0.65, pos=3, font=3, col='orange')
  text(x=x.space.x, y=x.space.y, labels = lsp$extra_x_space, cex=0.85, pos=1, font=2, col='orange')

  # demonstrate width on first profile
  arrows(x0=lsp$x0[1] - lsp$width, x1=lsp$x0[1] + lsp$width, y0=x.space.y, y1=x.space.y, length = 0.08, code = 3, col='orange', lwd=1)
  text(x=lsp$x0[1], y=x.space.y, labels = 'width', cex=0.65, pos=3, font=3, col='orange')
  text(x=lsp$x0[1], y=x.space.y, labels = lsp$width, cex=0.85, pos=1, font=2, col='orange')

  # plotting order
  text(x=lsp$x0, y=original.profile.idx.y, labels=lsp$plot.order, col='darkred', font=4, cex=0.75)
  mtext('original profile index', side=3, line=0, font=4, col='darkred')

  invisible(lsp)
}

#' Establish which elements within a vector of horizontal positions overlap beyond a given threshold
#'
#' @param x vector of relative horizontal positions, one for each profile
#' @return - `findOverlap` a vector of the same length as `x`, preserving rank-ordering and boundary conditions.
#' @note This is a very naive function and may fail to converge on a reasonable
#' solution. SANN would be a much more robust framework.
#' 
#' @rdname explainPlotSPC
#' 
#' @export
#'
findOverlap <- function(x, thresh) {
  # all pair-wise distance
  d <- dist(x)
  m <- as.matrix(d)
  # diagonal isn't used here
  diag(m) <- NA
  # find matrix elements
  idx <- which(m < thresh)
  # use upper-triangle indexes to find elements in original vector
  col.idx <- col(m)[idx]
  # done
  return(col.idx)
}

## 2019-07-16 | DEB
## fix overlap via random perterbation of affected elements
## this function is actually quite stupid as it can converge on bogus results
## scaled adjustments based on deviation from threshold distances would be better
## or, SANN style adjustments
##
## ideas:
## * cooling ala SANN -> smaller adjustments through time
## * perturbations should not increase overlap
## * debugging output
##
# x: vector of horizontal positions
# thresh: threshold at which overlap is a problem
# adj: adjustments are tested from runif(min=adj * -1, max=adj)
# min.x: left boundary condition
# max.x: right boundary condition
# maxIter: maximum number of iterations to attempt before collapsing to integer sequence
# trace: print diagnostics

#' Fix overlap via random perterbation of affected elements
#' @description This function is actually quite stupid as it can converge on bogus results
#' scaled adjustments based on deviation from threshold distances would be better
#  or, SANN style adjustments
#' @param x vector of horizontal positions
#' @param thresh horizontal threshold defining "overlap", must be < 1, ideal
#' values likely in \[0.3, 0.8]
#' @param adj adjustments are tested within \code{runif(min=adj * -1, max=adj)}
#' @param min.x left-side boundary condition
#' @param max.x right-side boundary condition
#' @param maxIter maximum number of iterations to attempt before giving up and
#' returning integer sequence
#' @param trace print diagnostics
#' @rdname explainPlotSPC
#' @export
#'
fixOverlap <- function(x, thresh=0.6, adj=0.2, min.x=0.8, max.x=length(x)+0.2, maxIter=1000, trace=FALSE) {

  # initial configuration
  ov <- findOverlap(x, thresh)

  # save original
  x.orig <- x

  # counter to prevent run-away while-loop
  i <- 1

  # short-circuit: only proceed if there is overlap
  if(length(ov) > 0) {

    # iterate...
    while(length(ov) > 0) {

      # fail-safe
      if(i > maxIter) {
        message('maximum number of iterations reached, using integer sequence')
        return(1:length(x))
      }

      # generate random perturbations to affected indices
      perturb <- runif(n = length(ov), min = adj * -1, max = adj)

      # attempt perturbation
      x.test <- x
      x.test[ov] <- x.test[ov] + perturb

      # enforce boundary conditions
      if(any(x.test < min.x) | any(x.test > max.x)) {
        # print('boundary condition')
        i <- i + 1
        next
      }

      # enforce rank ordering
      if(any(rank(x.orig) != rank(x.test))) {
        # print('rank violation')
        i <- i + 1
        next
      }

      ## this may be too strict: 85% -> 75% success rate if enabled
      # # perturbations should not increase number of affected positions
      # # check to be sure
      # len <- length(findOverlap(x.test, thresh))
      # # stats[[i]] <- len
      # if(len > length(ov)) {
      #   # print('wrong turn')
      #   i <- i + 1
      #   next
      # }

      ## alternative idea: perturbations should minimize overlap
      ## how to quantify?

      # apply perturbation to working copy
      x <- x.test

      # eval overlap and try again
      ov <- findOverlap(x, thresh)
      i <- i + 1
    }
  }

  if(trace) {
    message(sprintf("%s iterations", i))
  }


  return(x)
}

