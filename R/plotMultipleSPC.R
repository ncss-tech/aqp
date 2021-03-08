
#' @title Soil Profile Group Labels
#'
#' @description Labels groups of soil profiles within soil profile sketches.
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

## TODO: figure out intelligent recycling of arguments
## TODO: no mechanism for merged legends
## TODO: this doesn't take into account non-default figure geometry


#' @title Plot Multiple `SoilProfileCollection` Objects
#'
#' @details Combine multiple `SoilProfileCollection` objects into a single profile sketch,
#' with annotated groups.
#'
#' See examples below for usage.
#'
#' @param spc.list a list of \code{SoilProfileCollection} objects
#' 
#' @param group.labels a vector of group labels, one for each
#' \code{SoilProfileCollection} object
#' 
#' @param args a list of arguments passed to \code{plotSPC}, one for each
#' \code{SoilProfileCollection} object
#' 
#' @param merged.legend name of a horizon level attribute from which to create thematic sketches and merged legend
#' 
#' @param merged.colors vector of colors used to create thematic sketches from a shared horizon level attribute
#' 
#' @param merged.legend.title legend title
#' 
#' @param arrow.offset vertical offset in depth from base of start / end
#' profiles and group bracket arrows
#' 
#' @param bracket.base.depth baseline depth used for group brackets
#' 
#' @param label.offset vertical offset of group labels from baseline
#' 
#' @param label.cex label size
#' 
#' @param \dots additional arguments to the first call to \code{plotSPC}
#' 
#' @note For thematic sketches, use the `merged.legend` argument instead of `color` argument to `plotSPC`
#' 
#' @author D.E. Beaudette and Ben Marshall
#' 
#' @seealso \code{\link{profileGroupLabels}}
#' 
#' @keywords hplots
#' 
#' @examples
#'
#' ##
#' ## Simple Example
#' ##
#' 
#' # using default arguments to plotSPC()
#' 
#' # load sample data
#' data(sp3)
#' data(sp4)
#' 
#' # promote to SoilProfileCollection
#' depths(sp3) <- id ~ top + bottom
#' depths(sp4) <- id ~ top + bottom
#' 
#' # combine into a list
#' spc.list <- list(sp3, sp4)
#' 
#' # argument list
#' arg.list <- list(
#'   list(name='name', id.style='top'),
#'   list(name='name', id.style='side')
#' )
#' 
#' # plot multiple SPC objects, 
#' # with list of named arguments for each call to plotSPC
#' par(mar=c(1,1,3,3))
#' plotMultipleSPC(
#'   spc.list, 
#'   group.labels = c('Collection 1', 'Collection 2'),
#'   args = arg.list, 
#'   bracket.base.depth = 120, label.cex = 1
#' )
#' 
#' # specify a different max.depth
#' plotMultipleSPC(
#'   spc.list, 
#'   group.labels = c('Collection 1', 'Collection 2'),
#'   args = arg.list, 
#'   bracket.base.depth = 120, label.cex = 1,
#'   max.depth = 250
#' )
#' 
#' 
#' 
#' ##
#' ## Merged Legend Example
#' ##
#' 
#' # merged legend based on hz attribute 'clay'
#' 
#' # reset sample data
#' data(sp3)
#' data(sp4)
#' 
#' # promote to SoilProfileCollection
#' depths(sp3) <- id ~ top + bottom
#' depths(sp4) <- id ~ top + bottom
#' 
#' # combine into a list
#' spc.list <- list(sp3, sp4)
#' 
#' # argument list
#' arg.list <- list(
#'   list(name='name', id.style='top'),
#'   list(name='name', id.style='side')
#' )
#' 
#' 
#' par(mar=c(1,1,3,3))
#' plotMultipleSPC(
#'   spc.list, 
#'   group.labels = c('Collection 1', 'Collection 2'),
#'   args = arg.list, 
#'   label.cex = 1, 
#'   merged.legend = 'clay', merged.legend.title = 'Clay (%)'
#' )
#' 
#' 
#' ##
#' ## Complex Merged Legend Example
#' ##
#' 
#' # create a merged legend from "clay" in sp4 and jacobs2000
#' # use "soil_color" from sp3
#' 
#' # reset sample data
#' data(sp3)
#' data(sp4)
#' data(jacobs2000)
#' 
#' # promote to SoilProfileCollection
#' depths(sp3) <- id ~ top + bottom
#' depths(sp4) <- id ~ top + bottom
#' 
#' # remove 'clay' column from sp3
#' sp3$clay <- NULL
#' 
#' # combine into a list
#' spc.list <- list(sp3, sp4, jacobs2000)
#' 
#' # try some variations on the default arguments
#' # `clay` is missing in the first SPC, safe to specify another column for colors
#' arg.list <- list(
#'   list(color = 'soil_color', id.style='top', name = NA, width = 0.3, hz.depths = TRUE),
#'   list(name='name', id.style='side', name.style = 'center-center'),
#'   list(name='name', id.style='side', name.style = 'left-center', hz.depths = TRUE)
#' )
#' 
#' par(mar=c(1,1,3,3))
#' plotMultipleSPC(
#'   spc.list, 
#'   group.labels = c('sp3', 'sp4', 'jacobs2000'),
#'   label.offset = 3,
#'   args = arg.list, 
#'   merged.legend = 'clay', merged.legend.title = 'Clay (%)',
#'   axis.line.offset = 0
#' )


plotMultipleSPC <- function(spc.list, group.labels, args = rep(list(NA), times = length(spc.list)), merged.legend = NULL, merged.colors = c("#5E4FA2", "#3288BD", "#66C2A5","#ABDDA4", "#E6F598", "#FEE08B","#FDAE61", "#F46D43", "#D53E4F","#9E0142"), merged.legend.title = merged.legend, arrow.offset = 2, bracket.base.depth = 95, label.offset = 2, label.cex = 0.75, ...) {

  # compute group stats
  n.groups <- length(spc.list)
  spc.lengths <- sapply(spc.list, length)
  n.pedons <- sum(spc.lengths)
  group.starts <- c(1, 1 + cumsum(spc.lengths[-n.groups]))
  group.ends <- cumsum(spc.lengths)

  # get depths + offset to start / end profiles
  yy <- unlist(sapply(spc.list, function(i) profileApply(i, max)))
  tick.heights <- yy[c(group.starts, group.ends)] + arrow.offset
  
  
  # unique set of arguments specified in args and ...
  unique.args <- unique(
    c(
      names(unlist(arg.list)),
      names(list(...))
    )
  )
  
  # estimate a reasonable max depth (over all SPCs)
  # but only when not specified in any arguments
  if(! 'max.depth' %in% unique.args){
    # max over collections
    max.depth <- max(sapply(spc.list, max), na.rm = TRUE)
    
    # note: adding an extra 5% of max.depth for labels
    max.depth + (max.depth / 5)
    
    # insert into first set of arguments to plotSPC
    arg.list[[1]]$max.depth <- max.depth
  }
  
  
  # extend base depth if not supplied
  if(missing(bracket.base.depth)) {
    bracket.base.depth <- max(sapply(spc.list, max), na.rm = TRUE) + 10
  }
  
  # optionally create a merged set of thematic colors and legend
  if(! is.null(merged.legend)) {
    # color ramp function
    cr <- colorRamp(merged.colors)
    
    ## TODO: abstract for use in plotSPC
    
    # NA-padded value -> color mapping for full range of some horizon attribute
    .mapColor <- function(x, r, col.ramp) {
      # rescale to {0,1}
      c.rgb <- cr(.rescaleRange(x, x0 = 0, x1 = 1))
      cc <- which(complete.cases(c.rgb))
      cols <- rep(NA, times = nrow(c.rgb))
      cols[cc] <- rgb(c.rgb[cc, ], maxColorValue=255)
      return(cols)
    }
    
    # collect values over list of SPCs
    combined.data <- na.omit(
      unlist(
        lapply(spc.list, function(i) i[[merged.legend]])
      )
    )
    # get the full range
    combined.range <- range(combined.data, na.rm = TRUE)
    
    
    # iterate over list of profiles and arguments
    for(i in 1:length(spc.list)) {
      
      # current SPC
      spc_i <- spc.list[[i]]
      arg_i <- arg.list[[i]]
      
      # map colors if column is present
      if(!is.null(spc_i[[merged.legend]])) {
        
        # convert non-NA values into colors
        horizons(spc_i)[['.color']] <- .mapColor(spc_i[[merged.legend]], combined.range, cr)
        
        # add arguments
        
        # thematic flag
        arg_i$color = '.color'
        # suppress legend
        arg_i$show.legend = FALSE
        
        # modify in place
        spc.list[[i]] <- spc_i
        arg.list[[i]] <- arg_i
        
      } else {
        # do nothing
        
      }
      
    } # done iteration over lists of SPCs and arguments
    
    
    ## TODO: this will not work with categorical variables
    ##       -> abstract code from plotSPC into more general purpose functions
    
    # generate combined range / colors for legend
    pretty.vals <- pretty(combined.data, n = 8)
    
    # create legend object
    legend.data <- list(
      legend = pretty.vals, 
      col = rgb(
        cr(
          # rescale to {0,1}
          .rescaleRange(pretty.vals, x0 = 0, x1 = 1)
        ), 
        maxColorValue=255)
    )
    
    
  }

  # setup plot with first SPC in list
  do.call(
    what = plotSPC, 
    args = c(
      x = spc.list[[1]], 
      n = n.pedons, 
      na.omit(arg.list[[1]]),
      ...)
    )

  # iterate over remaining SPC objs
  if(n.groups > 1) {
    for(i in 2:n.groups) {
      this.obj <- spc.list[[i]]
      this.args <- na.omit(arg.list[[i]])
      suppressMessages(
        do.call(
          what = plotSPC, 
          args = c(x=this.obj, x.idx.offset=group.ends[i-1], add=TRUE, plot.depth.axis=FALSE, this.args)
        )
      )
    }
  }

  # annotate groups with brackets
  profileGroupLabels(
    x0 = group.starts, 
    x1 = group.ends, 
    labels = group.labels, 
    y0 = bracket.base.depth, 
    y1 = tick.heights, 
    label.offset = label.offset, 
    label.cex = label.cex
    )
  
  # add merged legend
  if(! is.null(merged.legend)) {
    mtext(side=3, text = merged.legend.title, font=2, line=1.6)
    legend('bottom', legend=legend.data$legend, col=legend.data$col, bty='n', pch=15, horiz=TRUE, xpd=TRUE, inset=c(0, 0.99))   
  }
  
}




