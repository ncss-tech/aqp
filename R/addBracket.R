## TODO:
# * generalize fully
# * verbose output for testing

#' @title Annotate Diagnostic Features
#' @description Annotate diagnostic features within a sketch of soil profiles.
#' 
#' @param s \code{SoilProfileCollection} object
#' @param kind filter applied to \code{feature} column of diagnostic horizons registered within \code{s}
#' @param feature column name containing feature kind
#' @param top column name containing feature top depth
#' @param bottom column name containing feature top depth
#' @param ... additional arguments passed to \code{addBracket}
#'
#' @details Additional examples can be found in \href{http://ncss-tech.github.io/AQP/aqp/SPC-plotting-ideas.html}{this tutorial}.
#' 
#' @note This is a `low-level` plotting function: you must first plot a \code{SoilProfileCollection} object before using this function.
#' 
#' @author D.E. Beaudette
#' 
#' @seealso \code{\link{addBracket}, \link{plotSPC}}
#'
#' @export
#'
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
# x: data.frame with lsp$idname, top, bottom, label (optional), may contain multiple / profile
# tick.length: bracket tick length
# offset: left-hand offset from profile center

#' @title Add Depth Brackets
#' @description Add depth brackets to soil profile sketches.
#'
#' @param x \code{data.frame} object containing `idname(x)`, `top`, `bottom`, and optionally `label` columns
#' @param label.cex scaling factor for label font
#' @param tick.length length of bracket "tick" mark
#' @param arrow.length length of arrowhead
#' @param offset left-hand offset from each profile
#' @param missing.bottom.depth distance (in depth units) to extend brackets that are missing a lower depth (defaults to max depth of collection)
#' @param ... further arguments passed on to \code{segments} or \code{arrows}
#' 
#' @details \code{x} may contain multiple records per profile. Additional examples can be found in \href{http://ncss-tech.github.io/AQP/aqp/SPC-plotting-ideas.html}{this tutorial}.
#' 
#' @note This is a `low-level` plotting function: you must first plot a \code{SoilProfileCollection} object before using this function.
#' 
#' @author D.E. Beaudette
#' 
#' @export
#' 
#' @seealso \code{\link{addDiagnosticBracket}, \link{plotSPC}}
#' 
#' @examples
#' 
#' # sample data
#' data(sp1)
#' 
#' # add color vector
#' sp1$soil_color <- with(sp1, munsell2rgb(hue, value, chroma))
#' 
#' # promote to SoilProfileCollection
#' depths(sp1) <- id ~ top + bottom
#' 
#' # plot profiles
#' par(mar = c(0, 0, 0, 1))
#' plotSPC(sp1, width = 0.3)
#' 
#' # extract min--max depths associated with all A horizons
#' # result is a single-row data.frame / profile
#' combinedBracket <- function(i) {
#'   h <- horizons(i)
#'   idn <- idname(i)
#'   this.id <- h[[idn]][1]
#'   
#'   idx <- grep('^A', h$name)
#'   
#'   res <- data.frame(
#'     id = this.id,
#'     top = min(h$top[idx]), 
#'     bottom = max(h$bottom[idx], na.rm=TRUE)
#'   )
#'   names(res)[1] <- idn
#'   
#'   return(res)
#' }
#' 
#' # return matching horizon top / bottom depths for A or C horizons
#' # result is a 0 or more row data.frame / profile
#' individualBrackets <- function(i) {
#'   h <- horizons(i)
#'   idn <- idname(i)
#'   this.id <- h[[idn]][1]
#'   
#'   idx <- grep('^A|^C', h$name)
#'   
#'   res <- data.frame(
#'     id = this.id,
#'     top = h$top[idx], 
#'     bottom = h$bottom[idx]
#'   )
#'   names(res)[1] <- idn
#'   
#'   return(res)
#' }
#' 
#' # combined brackets
#' b1 <- profileApply(sp1, combinedBracket, frameify = TRUE)
#' 
#' # individual brackets
#' b2 <- profileApply(sp1, individualBrackets, frameify = TRUE)
#' 
#' # plot in reverse order
#' plotSPC(sp1, plot.order = rev(1:length(sp1)), width = 0.25)
#' 
#' # note that plotting order is derived from the call to `plotSPC(sp1)`
#' addBracket(b1, col='red', offset = -0.35)
#' 
#' # plot in reverse order
#' plotSPC(sp1, plot.order = rev(1:length(sp1)), width = 0.25)
#' 
#' # note that plotting order is derived from the call to `plotSPC(sp1)`
#' addBracket(b2, col='red', offset = -0.35)
#' 
#' 
addBracket <- function(x, label.cex=0.75, tick.length=0.05, arrow.length=0.05, offset=-0.3, missing.bottom.depth=NULL, ...) {

  # get plotting details from aqp environment
  lsp <- get('last_spc_plot', envir = aqp.env)
  
  # y.offset is a vector length(x)
  depth.offset <- lsp$y.offset
  sf <- lsp$scaling.factor
  
  # line ending "2" is usually what we want
  # however, when using very large line widths and tick.length = 0, the interval will extend
  # beyond the top/bottom depths
  if(tick.length == 0) {
    seg.lend <- 1
  } else {
    seg.lend <- 2
  }
  
  
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

  ## TODO: this must also be done a profile at a time
  # test for all bottom depths missing
  no.bottom <- FALSE
  if(all(is.na(x$bottom)))
    no.bottom <- TRUE

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
  x$top <- (x$top * sf) + depth.offset[idx]
  x$bottom <- (x$bottom * sf) + depth.offset[idx]
  
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
      # note: these may contain NA
      # variables recycled within loop
      top <- brackets[['top']]
      bottom <- brackets[['bottom']]
      label <- brackets[['label']]
      
      # missing bottom: replace bottom tick with arrow head
      if(no.bottom) {
        
        # x-positions
        # x.base is a vector
        x.1 <- x.base[i] + offset
        x.2 <- x.1 + tick.length
        # top tick
        segments(x.1, top, x.2, top, lend = seg.lend, ...)
        # vertical bar is now an arrow
        arrows(x.1, top, x.1, top + missing.bottom.depth[i], length=arrow.length, lend = seg.lend, ...)
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
        if(no.bottom) {
          bottom <- rep(missing.bottom.depth[i], times = length(bottom))
        }
        # add labels at mid-points
        text(x.1 - 0.05, (top + bottom)/2, label, srt=90, cex=label.cex, pos=3)
      }
    }
    
    
  }

  

}


