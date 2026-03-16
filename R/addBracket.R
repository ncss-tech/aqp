

#' @title Annotate Diagnostic Features
#' @description Annotate diagnostic features within a sketch of soil profiles.
#' 
#' @param s `SoilProfileCollection` object
#' @param kind character, filter applied to `feature` column of diagnostic horizons registered within `s`
#' @param feature column name containing feature kind
#' @param top column name containing feature top depth
#' @param bottom column name containing feature top depth
#' @param ... additional arguments passed to [addBracket()]
#'
#' @details Additional examples can be found in [this tutorial](http://ncss-tech.github.io/AQP/aqp/SPC-plotting-ideas.html).
#' 
#' @note This is a `low-level` plotting function: you must first plot a `SoilProfileCollection` object before using this function.
#' 
#' @author D.E. Beaudette
#' 
#' @seealso [addBracket()], [plotSPC()]
#'
#' @export
#'
#' @examples
#' 
#'  # example data
#' x <- c(
#'   'P1:AAA|BwBwBwBw|CCCCCCC|CdCdCdCd',
#'   'P2:Ap|AA|E|BhsBhs|Bw1Bw1|CCCCC',
#'   'P3:A|Bt1Bt1Bt1|Bt2Bt2Bt2|Bt3|Cr|RRRRR',
#'   'P4:AA|EEE|BhsBhsBhsBhs|BwBw|CCCCC',
#'   'P5:AAAA|ACACACACAC|CCCCCCCCCCC|CdCdCd'
#' )
#' 
#' s <- quickSPC(x)
#' 
#' diagnostic_hz(s) <- data.frame(
#'   id = c('P1', 'P4'),
#'   t = c(12, 25), 
#'   b = c(70, 100),
#'   kind = c('Best', 'Best')
#' )
#' 
#' op <- par(no.readonly = TRUE)
#' par(mar = c(0, 0, 3, 2))
#' 
#' # sketches
#' plotSPC(
#'   s, name = 'name', name.style = 'center-center', cex.names = 0.75, max.depth = 210
#' )
#' 
#' # note that custom top/bottom depths must be supplied
#' addDiagnosticBracket(
#'   s, feature = 'kind', kind = 'Best', top = 't', bottom = 'b',
#'   labcol = 'kind',
#'   offset = -0.35, col = 'firebrick', tick.length = 0.04, lwd = 2
#' )
#' 
#' par(op)
#' 
#'
addDiagnosticBracket <- function(s, kind, feature = 'featkind', top = 'featdept', bottom = 'featdepb', ...) {

  # extract diagnostic horizon information
  # note: the idname is already present in `d`
  d <- diagnostic_hz(s)
  d <- d[which(d[[feature]] == kind), ]

  # there may be no matching features, in that case issue a message and do nothing
  if(nrow(d) < 1) {
    message('no matching features found')
  } else {
    # add backets
    # sorting is done via matching idname to plot order of idname
    addBracket(d, hzDepths = c(top, bottom), ...)
  }

}



#' @title Add Depth Brackets
#' 
#' @description Add depth brackets to soil profile sketches.
#'
#' @param x `data.frame` containing at least: `idname(x)` (profile ID) and `horizonDepths(x)` (horizon top and bottom depths)
#' 
#' @param labcol character, optional name of a column in `x` used for labeling
#' 
#' @param agg logical, aggregate multiple brackets per profile into a single depth range?
#' 
#' @param hzDepths character vector of length 2, optional column names in `x` that define bracket top and bottom depths. When `NULL`, use horizon top/bottom column names from the `SoilProfileCollection` object used by the last call to `plotSPC()`.
#' 
#' @param label.cex numeric, scaling factor for label font
#' 
#' @param tick.length numeric, length of bracket "tick" mark
#' 
#' @param arrow.length numeric, length of arrowhead (see `arrows()`)
#' 
#' @param offset numeric, left-hand offset from each profile
#' 
#' @param missing.bottom.depth numeric, distance (in depth units) to extend brackets that are missing a lower depth (defaults to max depth of collection)
#' 
#' @param ... further arguments passed on to `segments()` or `arrows()`
#' 
#' @details When `x` contains multiple records per profile a bracket will be created for each record. Setting `agg = TRUE` will first aggregate all records per profile, then add a single bracket spanning the depth range of those records. Additional examples can be found in \href{http://ncss-tech.github.io/AQP/aqp/SPC-plotting-ideas.html}{this tutorial}.
#' 
#' @note This is a `low-level` plotting function: you must first plot a `SoilProfileCollection` object before using this function. Details about the last plotted `SoilProfileCollection` are available using `get('last_spc_plot', envir = aqp.env)`. 
#' 
#' @author D.E. Beaudette
#' 
#' @export
#' 
#' @seealso [addDiagnosticBracket()], [plotSPC()]
#' 
#' @examples
#' 
#' # example data
#' x <- c(
#'   'P1:AAA|BwBwBwBw|CCCCCCC|CdCdCdCd',
#'   'P2:Ap|AA|E|BhsBhs|Bw1Bw1|CCCCC',
#'   'P3:A|Bt1Bt1Bt1|Bt2Bt2Bt2|Bt3|Cr|RRRRR',
#'   'P4:AA|EEE|BhsBhsBhsBhs|BwBw|CCCCC',
#'   'P5:AAAA|ACACACACAC|CCCCCCC|CdCdCd'
#' )
#' 
#' s <- quickSPC(x)
#' 
#' # change horizon depth names
#' # ensure that plotSPC() -> addBracket() can find them
#' horizonDepths(s) <- c('tt', 'bb')
#' 
#' # expression defines a single reference horizon in most profiles
#' .ex <- grepl('Bt3|Bw', s$name)
#' # encode for thematic profile sketches
#' s$e <- factor(as.character(.ex), levels = c('FALSE', 'TRUE'), labels = c('Horizons', 'Reference'))
#' 
#' # get horizon row indices to horizons above reference
#' a <- hzAbove(s, .ex, SPC = FALSE, simplify = TRUE)
#' 
#' # create bracket data.frame
#' b <- depths(s, hzID = FALSE)[a, ]
#' 
#' # add labels
#' b$label <- c('S')
#' 
#' op <- par(no.readonly = TRUE)
#' par(mar = c(0, 0, 3, 2), mfcol = c(1, 2))
#' 
#' # sketches
#' plotSPC(
#'   s, color = 'e', col.label = 'Original', col.palette = c('grey', 'royalblue'), 
#'   name = 'name', name.style = 'center-center', cex.names = 0.75,
#'   max.depth = 180
#' )
#' 
#' # plot individual brackets, no labels
#' addBracket(
#'   b, 
#'   agg = FALSE, labcol = 'label',
#'   offset = -0.35, col = 'black', tick.length = 0.04, lwd = 1
#' )
#' 
#' # sketches
#' plotSPC(
#'   s, color = 'e', col.label = 'Aggregate', col.palette = c('grey', 'royalblue'), 
#'   name = 'name', name.style = 'center-center', cex.names = 0.75,
#'   max.depth = 180
#' )
#' 
#' # aggregate multiple brackets into single depth span
#' # include first label from each group
#' addBracket(
#'   b, 
#'   agg = TRUE, labcol = 'label',
#'   offset = -0.35, col = 'firebrick', tick.length = 0.04, lwd = 2
#' ) 
#' 
#' par(op)
#' 
addBracket <- function(x, labcol = NULL, agg = FALSE, hzDepths = NULL, label.cex = 0.75, tick.length = 0.05, arrow.length = 0.05, offset = -0.3, missing.bottom.depth = NULL, ...) {

  # get plotting details from aqp environment
  lsp <- get('last_spc_plot', envir = aqp.env)
  
  # y.offset is a vector length(x)
  depth.offset <- lsp$y.offset
  sf <- lsp$scaling.factor
  
  
  # setup short names for horizon top and bottom depths
  
  # bracket depth column names inherited from the last plotSPC()
  if(is.null(hzDepths)) {
    .top <-lsp$horizonDepths[1]
    .bottom <- lsp$horizonDepths[2]
  } else {
    # bracket depth column names provided
    .top <- hzDepths[1]
    .bottom <- hzDepths[2]
  }

  
  # line ending "2" is usually what we want
  # however, when using very large line widths and tick.length = 0, the interval will extend
  # beyond the top/bottom depths
  if(tick.length == 0) {
    seg.lend <- 1
  } else {
    seg.lend <- 2
  }
  
  
  # test for required columns:
  # profile ID, top, bottom <-- these are defined in lsp
  if(is.null(x[[.top]]) | is.null(x[[.bottom]]) | is.null(x[[lsp$idname]])) {
    stop('required columns missing', call. = FALSE)
  }
  
  # test for < 0 rows
  if(nrow(x) < 1) {
    warning('no rows')
    return(FALSE)
  }

  # test for all missing top depths
  if(all(is.na(x[[.top]]))) {
    stop('no top depths supplied')
  }
    

  # test for missing label column
  do.label <- TRUE
  if(is.null(labcol)) {
    do.label <- FALSE
  }
  
  ## TODO: this must also be done a profile at a time
  # test for all bottom depths missing
  no.bottom <- FALSE
  if(all(is.na(x[[.bottom]]))) {
    no.bottom <- TRUE
  }
  
  # if not specified, use the max depth in the last plot
  if(is.null(missing.bottom.depth)) {
    missing.bottom.depth <- lsp$max.depth
  }

  # apply scale and offset to missing bottom depth
  # depth.offset is a vector length(x)
  missing.bottom.depth <- (missing.bottom.depth * sf) + depth.offset

  # apply scaling factor and offset
  # depth_prime = (depth * scaling factor) + y.offset
  # in case x is a subset of the last plotSPC call, must index
  idx <- match(x[[lsp$idname]], lsp$pIDs)
  x[[.top]] <- (x[[.top]] * sf) + depth.offset[idx]
  x[[.bottom]] <- (x[[.bottom]] * sf) + depth.offset[idx]
  
  ## x-coordinates
  # 2019-07-15: using relative position
  x.base <- lsp$x0
  
  # there may be more than 1 bracket per ID
  x.list <- split(x, x[[lsp$idname]])
  
  ## TODO: match logic seems backwards
  # re-order list elements, according to plot order
  # there may be some profiles without brackets to add
  # resulting in NA in re-ordering index
  idx <- match(lsp$pIDs, names(x.list))
  # re-order in the presence of NA
  x.list <- x.list[idx]
  
  # iterate over list of profile IDs
  for(i in seq_along(x.list)) {
    
    # current set of brackets (single profile)
    brackets <- x.list[[i]]
    
    # skipping profiles with missing brackets
    if(! is.null(brackets)) {
      
      
      # optional aggregation over multiple spans / profile
      # resulting label will be the first row
      if(agg) {
        .atop <- aggregate(brackets[[.top]], by = list(brackets[[lsp$idname]]), FUN = min)
        .abottom <- aggregate(brackets[[.bottom]], by = list(brackets[[lsp$idname]]), FUN = max)
        
        if(do.label) {
          .res <- cbind(.atop, .abottom[, 2], brackets[[labcol]][1])
          names(.res) <- c(lsp$idname, lsp$horizonDepths, labcol)
        } else {
          .res <- cbind(.atop, .abottom[, 2])
          names(.res) <- c(lsp$idname, lsp$horizonDepths)
        }
        
        brackets <- .res
      }
      
      # note: these may contain NA
      # variables recycled within loop
      top <- brackets[[.top]]
      bottom <- brackets[[.bottom]]
      
      # missing bottom: replace bottom tick with arrow head
      if(no.bottom) {
        
        # x-positions
        # x.base is a vector
        x.1 <- x.base[i] + offset
        x.2 <- x.1 + tick.length
        # top tick
        segments(x.1, top, x.2, top, lend = seg.lend, ...)
        # vertical bar is now an arrow
        arrows(x.1, top, x.1, top + missing.bottom.depth[i], length = arrow.length, lend = seg.lend, ...)
      } else {
        # normal usage
        
        # x-positions
        # x.base is a vector
        x.1 <- x.base[i] + offset
        x.2 <- x.1 + tick.length
        # top tick
        segments(x.1, top, x.2, top, lend = seg.lend, ...)
        # bottom tick
        segments(x.1, bottom, x.2, bottom, lend = seg.lend, ...)
        # vertical bar
        segments(x.1, top, x.1, bottom, lend = seg.lend, ...)
      }
      
      # optionally plot label
      if(do.label) {
        label <- brackets[[labcol]]
        if(no.bottom) {
          bottom <- rep(missing.bottom.depth[i], times = length(bottom))
        }
        # add labels at mid-points
        # requires a small horizontal nudge
        text(x.1 - 0.1, (top + bottom)/2, label, srt = 90, cex = label.cex, adj = c(0.5, 0.5))
      }
    }
  }
}


