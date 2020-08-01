## TODO: still not completely generalized
# annotate elements from @diagnostic with brackets
# mostly a helper function for addBracket()
addDiagnosticBracket <- function(s, kind, feature='featkind', top='featdept', bottom='featdepb', ...) {

  # extract diagnostic horizon information
  # note: the idname is already present in `d`
  d <- diagnostic_hz(s)
  d <- d[which(d[[feature]] == kind), ]

  # rename columns so that addBracket() can find top/bottom depths
  nm <- names(d)
  nm[which(nm == top)] <- 'top'
  nm[which(nm == bottom)] <- 'bottom'
  names(d) <- nm

  # there may be no matching features, in that case issue a message and do nothing
  if(nrow(d) < 1) {
    message('no matching features found')
  } else {
    # add backets
    # sorting is done via matching idname to plot order of idname
    addBracket(d, ...)
  }

}


## TODO: more testing!
## TODO: add proper documentation
## NOTE: this function is vectorized
# internal function for plotting a bracket (usually defines a diagnostic feature or similar)
# x: data.frame with lsp$idname, top, bottom, label (optional)
# tick.length: bracket tick length
# offset: left-hand offset from profile center
addBracket <- function(x, label.cex=0.75, tick.length=0.05, arrow.length=0.05, offset=-0.3, missing.bottom.depth=NULL, ...) {

  # get plotting details from aqp environment
  lsp <- get('last_spc_plot', envir=aqp.env)
  depth.offset <- lsp$y.offset
  sf <- lsp$scaling.factor

  # test for required columns:
  # lsp$idname
  # top
  # bottom
  if(is.null(x$top) | is.null(x$bottom) | is.null(x[[lsp$idname]]))
    stop('required columns missing', call. = FALSE)

  # test for > 0 rows
  if(nrow(x) < 1) {
    warning('no rows')
    return(FALSE)
  }

  # test for all missing top depths
  if(all(is.na(x$top)))
    stop('no top depths supplied')

  # test for missing label column
  do.label <- FALSE
  if(! is.null(x$label))
    do.label <- TRUE

  # test for all bottom depths missing
  no.bottom <- FALSE
  if(all(is.na(x$bottom)))
    no.bottom <- TRUE

  # if not specified, use the max depth in the last plot
  if(is.null(missing.bottom.depth)) {
    missing.bottom.depth <- lsp$max.depth
  }

  # apply scale and offset to missing bottom depth
  missing.bottom.depth <- (missing.bottom.depth * sf) + depth.offset

  # re-order rows of x, according to IDs
  idx <- match(lsp$pIDs, x[[lsp$idname]])
  x <- x[idx, ]

  # determine horizon depths in current setting
  # depth_prime = (depth * scaling factor) + y.offset
  top <- (x$top * sf) + depth.offset
  bottom <- (x$bottom * sf) + depth.offset

  ## x-coordinates
  # 2019-07-15: using relative position
  x.base <- lsp$x0

  # normal case: both top and bottom defined
  if(!missing(top) & !missing(bottom)) {
    # x-positions
    x.1 <- x.base + offset
    x.2 <- x.1 + tick.length
    # top tick
    segments(x.1, top, x.2, top, lend=2, ...)
    # bottom tick
    segments(x.1, bottom, x.2, bottom, lend=2, ...)
    # vertical bar
    segments(x.1, top, x.1, bottom, lend=2, ...)
  }

  # missing bottom: replace bottom tick with arrow head
  if(no.bottom) {
    # x-positions
    x.1 <- x.base + offset
    x.2 <- x.1 + tick.length
    # top tick
    segments(x.1, top, x.2, top, lend=2, ...)
    # vertical bar is now an arrow
    arrows(x.1, top, x.1, top + missing.bottom.depth, length=arrow.length, lend=2, ...)
  }

  # optionally plot label
  if (do.label) {
    if (no.bottom)
      bottom <- rep(missing.bottom.depth, times=length(bottom))
    # add labels
    text(x.1 - 0.05, (top + bottom)/2, x$label, srt=90, cex=label.cex, pos=3)
  }

}


