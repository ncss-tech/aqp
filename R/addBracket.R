## TODO: still not completely generalized
# annotate elements from @diagnostic with brackets 
# mostly a helper function for addBracket()
addDiagnosticBracket <- function(s, kind, id=idname(s), feature='featkind', top='featdept', bottom='featdepb', ...) {
  
  ## note: plot offset / scaling details are applied by addBracket()
  ## note: we still have to re-order depths based on the plotting order
  lsp <- get('last_spc_plot', envir=aqp.env)
  plot.order <- lsp$plot.order
  
  # extract diagnostic horizon information
  d <- diagnostic_hz(s)
  d <- d[which(d[[feature]] == kind), ]
  
  # there may be no matching features, in that case issue a message and do nothing
  if(nrow(d) < 1) {
    message('no matching features found')
  } else {
    # generate index linking our top/bottom depths with the plotting order
    # profile_id() returns original order
    # plot.order re-orders according to last plot
    key <- match(d[[id]], profile_id(s)[plot.order])
    
    # add backets
    # depths are in the same order as the key
    addBracket(top=d[[top]], bottom=d[[bottom]], idx=key, ...)
  }
  
}


###
### this is currently broken for all cases where plotting order != SPC order
###

## TODO: more testing!
## TODO: add proper documentation
## NOTE: this function is vectorized
# internal function for plotting a bracket (usually defines a diagnostic feature or similar)
# idx: integer index to profile, adjusted to correct plotting order
# top: top depth, adjusted to correct plotting order
# bottom: bottom depth, adjusted to correct plotting order
# label: optional label, adjusted to correct plotting order
# tick.length: bracket tick length
# offset: left-hand offset from profile center
addBracket <- function(idx, top, bottom=NULL, label=NULL, label.cex=0.75, tick.length=0.05, arrow.length=0.05, offset=-0.3, missing.bottom.depth=25, ...) {
  
  # get plotting details from aqp environment
  lsp <- get('last_spc_plot', envir=aqp.env)
  depth.offset <- lsp$y.offset
  sf <- lsp$scaling.factor
  w <- lsp$width
  
  ## TODO
  # sanity check: at the very least, length(top) = length(idx)
  
  
  # determine horizon depths in current setting
  # depth_prime = (depth * scaling factor) + y.offset
  top <- (top * sf) + depth.offset
  bottom <- (bottom * sf) + depth.offset

  # normal case: both top and bottom defined
  if(!missing(top) & !missing(bottom)) {
    # x-positions
    x.1 <- idx + offset
    x.2 <- x.1 + tick.length
    # top tick
    segments(x.1, top, x.2, top, lend=2, ...)
    # bottom tick
    segments(x.1, bottom, x.2, bottom, lend=2, ...)
    # vertical bar
    segments(x.1, top, x.1, bottom, lend=2, ...)
  }
  
  # missing bottom: replace bottom tick with arrow head
  if(!missing(top) & missing(bottom)) {
    # x-positions
    x.1 <- idx + offset
    x.2 <- x.1 + tick.length
    # top tick
    segments(x.1, top, x.2, top, lend=2, ...)
    # vertical bar is now an arrow
    arrows(x.1, top, x.1, top + (missing.bottom.depth * sf), length=arrow.length, lend=2, ...)
  }
  
  # optionally plot label
  if(!missing(top) & !missing(label)){
    text(x.1 - 0.05, (top + bottom)/2, label, srt=90, cex=label.cex, pos=3)
  }
  
}


