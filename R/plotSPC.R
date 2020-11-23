## 2019-07-16: moved util functions to `sketch-utils.R`



# TODO: behavior not defined for horizons with an indefinate lower boundary
# TODO: move some of the processing outside of the main loop: column names, etc.

#' Create Soil Profile Sketches
#' @name plotSPC
#' @rdname SoilProfileCollection-plotting-methods
#' @docType methods
#' @aliases plot,SoilProfileCollection,ANY-method,plot.SoilProfileCollection
#'
#' @description Generate a diagram of soil profile sketches from a \code{SoilProfileCollection} object. The \href{https://ncss-tech.github.io/AQP/aqp/aqp-intro.html}{Introduction to SoilProfileCollection Objects tutorial} contains many examples and discussion of the large number of arguments to this function.
#'
#' @param x a \code{SoilProfileCollection} object
#'
#' @param color quoted column name containing R-compatible color descriptions, or numeric / categorical data to be displayed thematically; see details
#'
#' @param width scaling of profile widths (typically 0.1 - 0.4)
#'
#' @param name quoted column name of the (horizon-level) attribute containing horizon designations, can be left as \code{NULL} and horizon designation column will be selected via \code{hzdesgnname(x)}. Suppress horizon name printing by setting \code{name=NA}.
#'
#' @param name.style one of several possible horizon designations labeling styles: 'right-center' (aqp default), 'left-top', 'left-center'
#'
#' @param label quoted column name of the (site-level) attribute used to identify profile sketches
#'
#' @param hz.depths logical, annotate horizon top depths to the right of each sketch (FALSE)
#'
#' @param alt.label quoted column name of the (site-level) attribute used for secondary annotation
#'
#' @param alt.label.col color used for secondary annotation text
#'
#' @param cex.names baseline character scaling applied to all text labels
#'
#' @param cex.depth.axis character scaling applied to depth scale
#'
#' @param cex.id character scaling applied to \code{label}
#'
#' @param font.id font style applied to \code{label}, default is 2 (bold)
#'
#' @param print.id logical, print \code{label} above/beside each profile? (TRUE)
#'
#' @param id.style \code{label} printing style: 'auto' (default) = simple heuristic used to select from: 'top' = centered above each profile, 'side' = 'along the top-left edge of profiles'
#'
#' @param plot.order integer vector describing the order in which individual soil profiles should be plotted
#'
#' @param relative.pos vector of relative positions along the x-axis, within \{1, n\}, ignores \code{plot.order} see details
#'
#' @param add logical, add to an existing figure
#'
#' @param scaling.factor vertical scaling of profile depths, useful for adding profiles to an existing figure
#'
#' @param y.offset vertical offset for top of profiles, useful for adding profiles to an existing figure
#'
#' @param x.idx.offset integer specifying horizontal offset from 0 (left-hand edge)
#'
#' @param n integer describing amount of space along x-axis to allocate, defaults to \code{length(x)}
#'
#' @param max.depth suggested lower depth boundary of plot
#'
#' @param n.depth.ticks suggested number of ticks in depth scale
#'
#' @param shrink logical, reduce character scaling for 'long' horizon by 80\% ?
#'
#' @param shrink.cutoff character length defining 'long' horizon names
#'
#' @param abbr logical, abbreviate \code{label}?
#'
#' @param abbr.cutoff suggested minimum length for abbreviated \code{label}
#'
#' @param divide.hz logical, divide horizons with line segment? (TRUE), see details
#'
#' @param hz.distinctness.offset NULL, or quoted column name (horizon-level attribute) containing vertical offsets used to depict horizon boundary distinctness (same units as profiles), see details and code{\link{hzDistinctnessCodeToOffset}}
#' 
#' @param hz.topography.offset NULL, or quoted column name (horizon-level attribute) containing offsets used to depict horizon boundary topography (same units as profiles), see details and code{\link{hzTopographyCodeToOffset}}
#'
#' @param hz.boundary.lty quoted column name (horizon-level attribute) containing line style (integers) used to encode horizon topography
#'
#' @param axis.line.offset horizontal offset applied to depth axis (default is -2.5, larger numbers move the axis to the right)
#'
#' @param plot.depth.axis logical, plot depth axis? (default is TRUE)
#'
#' @param density fill density used for horizon color shading, either a single integer or a quoted column name (horizon-level attribute) containing integer values (default is NULL, no shading)
#'
#' @param col.label thematic legend title
#'
#' @param col.palette color palette used for thematic sketches (default is \code{rev(brewer.pal(10, 'Spectral'))})
#'
#' @param col.palette.bias color ramp bias (skew), see \code{\link{colorRamp}}
#'
#' @param col.legend.cex scaling of thematic legend
#'
#' @param n.legend approximate number of classes used in numeric legend, max number of items per row in categorical legend
#'
#' @param lwd line width multiplier used for sketches
#'
#' @param lty line style used for sketches
#'
#' @param default.color default horizon fill color used when \code{color} attribute is \code{NA}
#'
#' @param \dots other arguments passed into lower level plotting functions
#'
#'
#' @details
#' Depth limits (\code{max.depth}) and number of depth ticks (\code{n.depth.ticks}) are *suggestions* to the \code{\link{pretty}} function. You may have to tinker with both parameters to get what you want.
#'
#' The 'side' \code{id.style} is useful when plotting a large collection of profiles, and/or, when profile IDs are long.
#'
#' If the column containing horizon designations is not specified (the \code{name} argument), a column (presumed to contain horizon designation labels) is guessed based on regular expression matching of the pattern 'name'-- this usually works, but it is best to manual specify the name of the column containing horizon designations.
#'
#' The \code{color} argument can either name a column containing R-compatible colors, possibly created via \code{\link{munsell2rgb}}, or column containing either numeric or categorical (either factor or character) values. In the second case, values are converted into colors and displayed along with a simple legend above the plot. Note that this functionality makes several assumptions about plot geometry and is most useful in an interactive setting.
#'
#' Adjustments to the legend can be specified via \code{col.label} (legend title), \code{col.palette} (palette of colors, automatically expanded), \code{col.legend.cex} (legend scaling), and \code{n.legend} (approximate number of classes for numeric variables, or, maximum number of legend items per row for categorical variables). Currently, \code{plotSPC} will only generate two rows of legend items. Consider reducing the number of classes if two rows isn't enough room.
#'
#' Profile sketches can be added according to relative positions along the x-axis (vs. integer sequence) via \code{relative.pos} argument. This should be a vector of positions within \{1,n\} that are used for horizontal placement. Default values are \code{1:length(x)}. Care must be taken when both \code{plot.order} and \code{relative.pos} are used simultaneously: \code{relative.pos} specifies horizontal placement after sorting. \code{addDiagnosticBracket} and \code{addVolumeFraction} use the \code{relative.pos} values for subsequent annotation.
#'
#' Relative positions that are too close will result in overplotting of sketches. Adjustments to relative positions such that overlap is minimized can be performed with \code{fixOverlap(pos)}, where \code{pos} is the original vector of relative positions.
#'
#' The \code{x.idx.offset} argument can be used to shift a collection of pedons from left to right in the figure. This can be useful when plotting several different \code{SoilProfileCollection} objects within the same figure. Space must be pre-allocated in the first plotting call, with an offset specified in the second call. See examples below.
#'
#'
#'
#' @note A new plot of soil profiles is generated, or optionally added to an existing plot.
#'
#' @author D.E. Beaudette
#'
#' @references Beaudette, D.E., Roudier P., and A.T. O'Geen. 2013. Algorithms for Quantitative Pedology: A Toolkit for
#' Soil Scientists. Computers & Geosciences. 52:258 - 268.
#'
#' @keywords hplots
#'
#' @seealso \code{\link{fixOverlap}, \link{explainPlotSPC}, \link{SoilProfileCollection-class}, \link{pretty}, \link{hzDistinctnessCodeToOffset}, \link{addBracket}, \link{profileGroupLabels}}
#'
#' @examples
#'
#' # example data
#' data(sp1)
#' # usually best to adjust margins
#' par(mar=c(0,0,3,0))
#'
#' # add color vector
#' sp1$soil_color <- with(sp1, munsell2rgb(hue, value, chroma))
#'
#' # promote to SoilProfileCollection
#' depths(sp1) <- id ~ top + bottom
#'
#' # plot profiles
#' plot(sp1, id.style='side')
#'
#' # title, note line argument:
#' title('Sample Data 1', line=1, cex.main=0.75)
#'
#' # plot profiles without horizon-line divisions
#' plot(sp1, divide.hz=FALSE)
#'
#' # add dashed lines illustrating horizon boundary distinctness
#' sp1$hzD <- hzDistinctnessCodeToOffset(sp1$bound_distinct)
#' plot(sp1, hz.distinctness.offset='hzD')
#'
#' # plot horizon color according to some property
#' data(sp4)
#' depths(sp4) <- id ~ top + bottom
#' plot(sp4, color='clay')
#'
#' # another example
#' data(sp2)
#' depths(sp2) <- id ~ top + bottom
#' site(sp2) <- ~ surface
#'
#' # label with site-level attribute: `surface`
#' plot(sp2, label='surface', plot.order=order(sp2$surface))
#'
#' # example using a categorical attribute
#' plot(sp2, color = "plasticity")
#'
#' # plot two SPC objects in the same figure
#' par(mar=c(1,1,1,1))
#' # plot the first SPC object and
#' # allocate space for the second SPC object
#' plot(sp1, n=length(sp1) + length(sp2))
#' # plot the second SPC, starting from the first empty space
#' plot(sp2, x.idx.offset=length(sp1), add=TRUE)
#'
#'
#' ##
#' ## demonstrate adaptive legend
#' ##
#'
#' data(sp3)
#' depths(sp3) <- id ~ top + bottom
#'
#' # make some fake categorical data
#' horizons(sp3)$fake.data <- sample(letters[1:15], size = nrow(sp3), replace=TRUE)
#'
#' # better margins
#' par(mar=c(0,0,3,1))
#'
#' # note that there are enough colors for 15 classes (vs. previous limit of 10)
#' # note that the legend is split into 2 rows when length(classes) > n.legend argument
#' plot(sp3, color='fake.data', name='fake.data', cex.names=0.8)
#'
#' # make enough room in a single legend row
#' plot(sp3, color='fake.data', name='fake.data', cex.names=0.8, n.legend=15)

plotSPC <- function(
  x,
  color='soil_color',
  width=0.2,
  name=NULL,
  name.style='right-center',
  label=idname(x),
  hz.depths=FALSE,
  alt.label=NULL,
  alt.label.col='black',
  cex.names=0.5,
  cex.depth.axis=cex.names,
  cex.id=cex.names+(0.2*cex.names),
  font.id=2,
  print.id=TRUE,
  id.style='auto',
  plot.order=1:length(x),
  relative.pos=1:length(x),
  add=FALSE,
  scaling.factor=1,
  y.offset=0,
  x.idx.offset=0,
  n=length(x),
  max.depth=ifelse(is.infinite(max(x)), 200, max(x)),
  n.depth.ticks=5,
  shrink=FALSE,
  shrink.cutoff=3,
  abbr=FALSE,
  abbr.cutoff=5,
  divide.hz=TRUE,
  hz.distinctness.offset=NULL,
  hz.topography.offset=NULL,
  hz.boundary.lty=NULL,
  axis.line.offset=-2.5,
  plot.depth.axis=TRUE,
  density=NULL,
  col.label=color,
  col.palette = c("#5E4FA2", "#3288BD", "#66C2A5","#ABDDA4", "#E6F598",
                  "#FEE08B","#FDAE61", "#F46D43", "#D53E4F","#9E0142"),
  col.palette.bias=1,
  col.legend.cex=1,
  n.legend=8,
  lwd=1,
  lty=1,
  default.color=grey(0.95),
  ...
) {

  ###################
  ## sanity checks ##
  ###################

  # horizon name style
  if(! name.style %in% c('right-center', 'left-center', 'left-top', 'center-center', 'center-top')) {
    warning('invalid `name.style`', call. = FALSE)
    name.style <- 'right-center'
  }

  # horizon distinctness offset column name must be valid
  if(!missing(hz.distinctness.offset)) {
    # valid name
    if(! hz.distinctness.offset %in% horizonNames(x))
      stop('invalid `hz.distinctness.offset` column name', call. = FALSE)

    # must be numeric
    if(! is.numeric(x[[hz.distinctness.offset]]))
      stop('`hz.distinctness.offset` must be numeric', call. = FALSE)
    
  } else {
    # no horizon distinctness offset specified, use 0
    hz.distinctness.offset <- '.hdo'
    horizons(x)[[hz.distinctness.offset]] <- 0
  }

  # horizon topography offset column name must be valid
  if(!missing(hz.topography.offset)) {
    # valid name
    if(! hz.topography.offset %in% horizonNames(x))
      stop('invalid `hz.topography.offset` column name', call. = FALSE)
    
    # must be numeric
    if(! is.numeric(x[[hz.topography.offset]]))
      stop('`hz.topography.offset` must be numeric', call. = FALSE)
    
  } else {
    # no horizon topography offset specified, use 0
    hz.topography.offset <- '.hto'
    horizons(x)[[hz.topography.offset]] <- 0
  }
  
  # horizon line type
  if(!missing(hz.boundary.lty)) {
    # valid name
    if(! hz.boundary.lty %in% horizonNames(x))
      stop('invalid `hz.boundary.lty` column name', call. = FALSE)

    # must be numeric
    if(! is.numeric(x[[hz.boundary.lty]]))
      stop('`hz.boundary.lty` must be numeric', call. = FALSE)
  }


  ###################
  ## fudge factors ##
  ###################

  # TODO: base calculations on strwidth() AFTER plot() has been called

  # padding along x-axis, prevents crowding
  # dynamic adjustment must also taking into account figure size
  # roughly 10% of length(x)
  extra_x_space <- length(x) * 0.1

  # multiplier (width * x_left_spac_mult) used to set left-side space along x-axis
  x_left_space_mult <- 2

  # add a little extra x-space when n <= 5
  if(length(x) <= 5 & length(x) > 2) {
    extra_x_space <- extra_x_space + 0.25
    x_left_space_mult <- 2

  }

  # special cases
  if(length(x) == 2) {
    extra_x_space <- extra_x_space + 0.5
    x_left_space_mult <- 2.75

  }

  # special cases
  if(length(x) == 1) {
    extra_x_space <- extra_x_space + 0.5
    x_left_space_mult <- 3.5
  }

  # padding above profiles, ~ 15 is about right for n in {1,25} and max depth near 150cm
  # a sketch of shalllow profiles could benefit from ~ 5
  if(max.depth <=50)
    extra_y_space <- 5
  if(max.depth > 50 & max.depth <= 100)
    extra_y_space <- 10
  if(max.depth > 100)
    extra_y_space <- 15

  # get profile IDs
  pIDs <- profile_id(x)

  # save arguments to aqp env
  lsp <- list('width'=width,
              'plot.order'=plot.order,
              'x0'=relative.pos + x.idx.offset,
              'pIDs'=pIDs[plot.order],
              'idname'=idname(x),
              'y.offset'=y.offset,
              'scaling.factor'=scaling.factor,
              'max.depth'=max.depth,
              'n'=n,
              'extra_x_space'=extra_x_space,
              'extra_y_space'=extra_y_space)

  assign('last_spc_plot', lsp, envir=aqp.env)

  # get horizons
  h <- horizons(x)

  # get column names from horizon dataframe
  nm <- names(h)
  
  ## TODO: there has to be a clear mechanism for suppressing labeling of horizon names
  
  # if the user has not specified a column containing horizon designations
  #   or, if the specified value is not in the horizon names vector
  #   try to make a guess
  if(missing(name)) {
    name <- guessHzDesgnName(x)
  }

  if(!is.na(name) & !all((name %in% horizonNames(x)))) {
    name <- guessHzDesgnName(x)
  }

  ####################
  ## horizon colors ##
  ####################
  
  # results contain:
  # vector of horizon colors, row-order preserved
  # legend data if relevant, otherwise NULL
  hz.color.interpretation <- .interpretHorizonColor(h, color, default.color, col.palette, col.palette.bias, n.legend)
  h[['.color']] <- hz.color.interpretation$colors
  
  
  ####################
  ## horizon depths ##
  ####################
  
  # get top/bottom column names
  IDcol <- idname(x)
  hzDepthCols <- horizonDepths(x)
  tcol <- hzDepthCols[1]
  bcol <- hzDepthCols[2]

  # get profile labels from @site
  pLabels <- site(x)[[label]]

  ## this should probably use strwidth() AFTER plot() has been called
  # if profile style is auto, determine style based on font metrics
  if(id.style == 'auto') {
  	sum.ID.str.width <- sum(sapply(pLabels, strwidth, units='inches', cex=cex.id, font=2))
  	plot.width <- par('pin')[1]
  	ID.width.ratio <- sum.ID.str.width  / plot.width
#   	print(ID.width.ratio)

  	if(ID.width.ratio > 0.7)
  		id.style <- 'side'
  	else
  		id.style <- 'top'
  	}


  # pre-compute nice range for depth axis, also used for plot init
  depth_axis_intervals <- pretty(seq(from=0, to = max.depth, by = 1), n = n.depth.ticks)

  # init plotting region, unless we are appending to an existing plot
  # note that we are using some fudge-factors to get the plotting region just right
  if(!add) {
    # margins are set outside of this function
	  plot(0, 0, type='n', xlim=c(width * x_left_space_mult, n+(extra_x_space)),
	       ylim=c(max(depth_axis_intervals), -extra_y_space),
	       axes=FALSE, xlab='', ylab='')
  }


  # calculate width of a single character on current plot device
  one.char.width <- strwidth('W')

  # TODO dynamically adjust `width` based on strwidth(longest.hz.name)
  # TODO abstract single profile sketch code into a single function
  # TODO skip sketch rendering when i=`n` outside of length(SPC) (depths are NA)
  
  ## iterate over profile index from 1 -> n
  ## note: there may not be `n` profiles
  for(i in 1:n) {
	  # convert linear sequence into plotting order
	  profile_i <- plot.order[i]

	  # extract the current profile's horizon data
    this_profile_label <- pLabels[profile_i]
	  this_profile_id <- pIDs[profile_i]

	  this_profile_data <- h[h[[IDcol]] == this_profile_id, ]

    # extract column names
    cn <- names(this_profile_data)

    # extract / generate horizon color
    # note: the ".color" horizon attribute is auto-generated above
    # missing and NA colors have already been dealt with above
    this_profile_colors <- this_profile_data$.color

	  # extract / generate horizon fill density
	  if(! missing(density)) {
	  	# if a single number was given, then recylce it over all horizons
	  	if(is.numeric(density))
	  		this_profile_density <- density
	  	# otherwise we have a column name
	  	else {
	  		m <- match(density, cn)
	  		if(! is.na(m))
		  		this_profile_density <- this_profile_data[[m]]
		  	else # user-defined column is missing
			  	this_profile_density <- NULL
	  	}
	  }
	  else # no user-defined density column
	  	this_profile_density <- NULL

    # extract / generate horizon name
    m <- match(name, cn)
    if(! is.na(m))
      this_profile_names <- this_profile_data[[m]]
      # otherwise use an empty string
    else
      this_profile_names <- ''


    ########################################
	  ## generate baseline horizon geometry ##
    ########################################

    ## center of each sketch
    # 2019-07-15: added relative position feature, could use some more testing
    # x0 <- x.idx.offset + i
    x0 <- x.idx.offset + relative.pos[i]

	  # get vectors of horizon boundaries, and scale
	  y0 <- (this_profile_data[[bcol]] * scaling.factor) + y.offset
	  y1 <- (this_profile_data[[tcol]] * scaling.factor) + y.offset


	  
	  ##############################
	  ## create horizons + colors ##
	  ##############################
	  
	  # horizons are parallelograms
	  # offset described by hz.distinctness.offset
	  # horizontal mid-points described by hz.topography.offset

	  # iterate over horizons
	  nh <- length(y0)
	  
	  # extract current distinctness offset
	  hdo <- this_profile_data[[hz.distinctness.offset]]
	  
	  # apply sketch-wide scaling factor
	  hdo <- hdo * scaling.factor
	  
	  # extract current topography offset
	  hto <- this_profile_data[[hz.topography.offset]]
	  
	  # apply sketch-wide scaling factor
	  hto <- hto * scaling.factor
	  
	  # extract current set of line types if provided
	  if(! is.null(hz.boundary.lty)) {
	    ht.lty <- this_profile_data[, hz.boundary.lty]
	  } else {
	    # use constant value
	    ht.lty <- rep(1, times=nh)
	  }
	  
	  
	  # empty list for storing y-coordinates
	  coords.list <- vector(mode = 'list', length = nh)
	  
	  for(j in 1:nh) {
	    
	    ## rectangle for reference
	    # xx <- c(x0 - width, x0 + width, x0 + width, x0 - width)
	    # yy <- c(y0[j], y0[j], y1[j], y1[j])
	    
	    # parallelogram geometry: x-coords are fixed, y-coords vary based on horizon sequence
	    # y0 are bottom depths
	    # y1 are top depths
	    #
	    # vertex order: ll, lc, lr, ur, uc, ul
	    x.ll <- x0 - width
	    x.lr <- x0 + width
	    x.ur <- x0 + width
	    x.ul <- x0 - width
	    # mid-points for horizon topography
	    x.lc <- x0
	    x.uc <- x0
	    xx <- c(x.ll, x.lc, x.lr, x.ur, x.uc, x.ul)
	    
	    # make polygons based on 1st, 2nd to j-1, last horizon
	    if(j == 1){
	      # first horizon
	      y.ll <- pmin(y0[j] + hdo[j], y0[j+1]) # cannot exceed y.ll of next horizon
	      y.lr <- pmax(y0[j] - hdo[j], y1[j]) # cannot exceed y.ur of this horizon
	      y.ur <- y1[j] # use upper-right verbatim
	      y.ul <- y1[j] # use upper-left verbatim
	      # mid-points for horizon topography
	      y.lc <- pmax(y0[j] - hto[j], y1[j]) # cannot exceed top depth of current horizon
	      y.uc <- y1[j] # use upper-center verbatim
	      
	      # assemble y-coords and plot first horizon polygon, without borders
	      yy <- c(y.ll, y.lc, y.lr, y.ur, y.uc, y.ul)
	      polygon(x = xx, y = yy, col=this_profile_colors[j], border=NA, density=this_profile_density[j], lwd=lwd, lty=lty, lend=1)
	      
	    } else if(j < nh) {
	      # next horizons, except bottom-most horizon
	      y.ll <- pmin(y0[j] + hdo[j], y0[j+1]) # cannot exceed y.ll of next horizon
	      y.lr <- pmax(y0[j] - hdo[j], y0[j-1]) # cannot exceed y.lr of previous horizon
	      y.ur <- pmax(y1[j] - hdo[j-1], y1[j-1]) # cannot exceed y.ur of previous horizon
	      y.ul <- pmin(y1[j] + hdo[j-1], y0[j]) # cannot exceed y.ul of previous horizon
	      # mid-points for horizon topography
	      y.lc <- pmax(y0[j] - hto[j], y1[j]) # cannot exceed top depth of current horizon
	      y.uc <- pmax(y1[j] - hto[j-1], y1[j-1]) # cannot exceed top depth of previous horizon
	      
	      # assemble y-coords and plot next n horizon's polygon, without borders
	      yy <- c(y.ll, y.lc, y.lr, y.ur, y.uc, y.ul)
	      polygon(x = xx, y = yy, col=this_profile_colors[j], border=NA, density=this_profile_density[j], lwd=lwd, lty=lty, lend=1)
	      ## debugging
	      # polygon(x = xx, y = yy, col=NA, border='red', density=this_profile_density[j], lwd=lwd, lty=lty)
	      
	    } else {
	      # last horizon
	      y.ll <- y0[j] # user lower-left verbatim
	      y.lr <- y0[j] # use lower-right verbatim
	      y.ur <- pmax(y1[j] - hdo[j-1], y1[j-1]) # cannot exceed y.ur of previous horizon
	      y.ul <- pmin(y1[j] + hdo[j-1], y0[j]) # cannot exceed lower depth of profile
	      # mid-points for horizon topography
	      y.lc <- y0[j] # truncate to lower depth of last horizon
	      y.uc <- pmax(y1[j] - hto[j-1], y1[j-1]) # cannot exceed top of previous horizon
	      
	      # assemble y-coords and plot last horizon polygon, without borders
	      yy <- c(y.ll, y.lc, y.lr, y.ur, y.uc, y.ul)
	      polygon(x = xx, y = yy, col=this_profile_colors[j], border=NA, density=this_profile_density[j], lwd=lwd, lty=lty, lend=1)
	      
	    }
	    
	    # save current iteration of coordinates and line type
	    coords.list[[j]] <- list(xx=xx, yy=yy, lty=ht.lty[j])
	  }
	  
	  ## note: have to do this after the polygons, otherwise the lines are over-plotted
	  # optionally divide horizons with line segment
	  if(divide.hz) {
	    
	    # iterate over coordinates
	    # coordinate logic: ll, lc, lr, ur, uc, ul
	    # line style included
	    lapply(coords.list, function(seg) {
	      segments(x0 = seg$xx[1], y0 = seg$yy[1], x1 = seg$xx[2], y1 = seg$yy[2], lwd=lwd, lty=seg$lty, lend=1)
	      segments(x0 = seg$xx[2], y0 = seg$yy[2], x1 = seg$xx[3], y1 = seg$yy[3], lwd=lwd, lty=seg$lty, lend=1)
	    })
	  }
	  
	  ## final rectangle border around entire profile
	  # note: when manually specifying n > length(SPC)
	  # x0,x1,y0,y1 are NA
	  # using `na.rm = TRUE` in the following calls to min() or max() will generate warnings
	  rect(xleft = x0 - width, ybottom = min(y1), xright = x0 + width, ytop = max(y0), lwd = lwd, lty = lty, lend = 2)
	  
	  
	  
	  # # standard rectangles
	  # # default are filled rectangles
	  # if(divide.hz) {
	  #   # classic approach: use rectangles, fully vectorized and automatic recycling over arguments
	  #   # x0 and width have length of 1
	  #   rect(x0 - width, y0, x0 + width, y1, col=this_profile_colors, border=NULL, density=this_profile_density, lwd=lwd, lty=lty)
	  # 
	  # } else {
	  #   # otherwise, we only draw the left, top, right borders, and then fill
	  # 
	  #   rect(x0 - width, y0, x0 + width, y1, col=this_profile_colors, border=NA, density=this_profile_density, lwd=lwd, lty=lty)
	  #   segments(x0 - width, y0, x0 - width, y1, lwd=lwd, lty=lty, lend=2) # left-hand side
	  #   segments(x0 + width, y0, x0 + width, y1, lwd=lwd, lty=lty, lend=2) # right-rand side
	  #   segments(x0 - width, min(y1), x0 + width, min(y1), lwd=lwd, lty=lty, lend=2) # profile top
	  #   segments(x0 - width, max(y0), x0 + width, max(y0), lwd=lwd, lty=lty, lend=2) # profile bottom
	  # }
	  # 
	  
	  

	  ##################################
	  ## horizon designations (names) ##
	  ##################################
	  switch(
	    name.style,
	    'right-center' = {
	      # standard annotation
	      # offset from right-hand side
	      hzname.x0 <- x0 + width + (one.char.width * 0.1)
	      # horizon depth mid-point
	      hzname.y0 <- ( y1 + y0 ) / 2
	      # left-hand / vertical center justification
	      hzname.adj <- c(0, 0.5)
	      hzname.col <- par('fg') # use whatever the foreground color is
	    },
	    'left-center' = {
	      # inset from left-hand side
	      hzname.x0 <- (x0 - width) + (one.char.width * 0.1)
	      # horizon depth mid-point
	      hzname.y0 <- ( y1 + y0 ) / 2
	      # left-hand / vertical center justification
	      hzname.adj <- c(0, 0.5)
	      # high-contrast labels
	      hzname.col <- invertLabelColor(this_profile_colors)
	    },
	    'left-top' = {
	      # soilweb style
	      # inset from upper-left corner
	      hzname.x0 <- (x0 - width) + (one.char.width * 0.1)
	      hzname.y0 <- y1
	      # left-hand / vertical top justification
	      hzname.adj <- c(0, 1)
	      # high-contrast labels
	      hzname.col <- invertLabelColor(this_profile_colors)
	    },
	    'center-center' = {
	      # inset from upper-left corner
	      hzname.x0 <- x0
	      hzname.y0 <- (y1 + y0) / 2
	      # center just
	      hzname.adj <- c(0.5, 0.5)
	      # high-contrast labels
	      hzname.col <- invertLabelColor(this_profile_colors)
	    },
	    'center-top' = {
	      # inset from upper-left corner
	      hzname.x0 <- x0
	      hzname.y0 <- y1
	      # center just
	      hzname.adj <- c(0.5, 1)
	      # high-contrast labels
	      hzname.col <- invertLabelColor(this_profile_colors)
	    }

	  )



	  # optionally shrink the size of names if they are longer than a given thresh
	  if(shrink) {
		  names.to.shrink <- which(nchar(this_profile_names) > shrink.cutoff)
		  cex.names.shrunk <- rep(cex.names, length(this_profile_data[, tcol]))
		  cex.names.shrunk[names.to.shrink] <- cex.names.shrunk[names.to.shrink] * 0.8

		  text(hzname.x0, hzname.y0, labels = this_profile_names, cex=cex.names.shrunk, adj=hzname.adj, col=hzname.col)
		  } else {
	    # standard printing of names, all at the same size
	    text(hzname.x0, hzname.y0, labels = this_profile_names, cex=cex.names, adj=hzname.adj, col=hzname.col)

	    ## old approach: label rigth-center, left justified, 0.1 charwidth offset
	    # text(x0 + width, hzname.y0, this_profile_names, offset=0.1, cex=cex.names, pos=4)
		  }


	  ##################################
	  ## horizon top depth annotation ##
	  ##################################
	  if(hz.depths) {
	    ## TODO: consider use of unicode arrow markers
	    # hzd.txt <- sprintf('\u25c4%s', this_profile_data[, tcol])
	    # text(x = x0 + width, y = y1, labels = hzd.txt, cex = cex.names * 0.9, pos = 4, offset = 0, font = 1)

	    # just labels
	    hzd.txt <- this_profile_data[, tcol]
	    text(x = x0 + width, y = y1, labels = hzd.txt, cex = cex.names * 0.9, pos = 4, offset = 0.1, font = 1)
	  }



	  #################
	  ## profile IDs ##
	  #################
	  if(print.id) {
			# optionally abbreviate
			if(abbr)
		  	id.text <- abbreviate(as.character(this_profile_label), abbr.cutoff)

			# no abbreviations of th ID
			else
	  		id.text <- as.character(this_profile_label)

			# add the text: according to style
			if(id.style == 'top')
				text(x0, y.offset, id.text, pos=3, font=font.id, cex=cex.id)

			if(id.style == 'side')
				text(x0 - (width+0.025), y.offset, id.text, adj=c(1, -width), font=font.id, cex=cex.id, srt=90)
			}
	  }


  ################
  ## depth axis ##
  ################
  if(plot.depth.axis) {
    depth_axis_tick_locations <- (depth_axis_intervals * scaling.factor) + y.offset
    depth_axis_labels <- paste(depth_axis_intervals, depth_units(x))

    axis(side=4, line=axis.line.offset, las=2, at=depth_axis_tick_locations, labels=depth_axis_labels, cex.axis=cex.depth.axis, col.axis=par('fg'))
  }



  ######################
  ## alternate labels ##
  ######################
  if(!missing(alt.label)) {
  	al <- site(x)[[alt.label]]
  	al <- al[plot.order]
  	text(x = 1:length(x), y = y.offset + 3, labels = al, srt = 90, adj = c(1, 0.5), font = 2, cex = cex.id * 1.5, col = alt.label.col)
  }


  ########################################
  ## legend for thematic profile sketch ##
  ########################################
  # extract from interpretation of horizon colors
  cld <- hz.color.interpretation$color.legend.data
  
  if(! is.null(cld)) {
    # if no title given, set col.label to name of column containing thematic information
    mtext(side=3, text=col.label, font=2, line=1.6)

    # gracefully handle all-NA in thematic variable
    if(length(cld$legend) > 0) {

      # possibly split legend across multiple rows
      if(cld$multi.row.legend) {

        # compute max space required for legend items
        # better formatting
        # note: must be called AFTER high level plot()
        leg.text.width <- (max(strwidth(cld$legend, cex = col.legend.cex)))

        # row 1
        legend('bottom', inset = c(0, 0.99),
               legend = cld$legend[cld$leg.row.indices$row.1],
               col = cld$col[cld$leg.row.indices$row.1],
               text.width = leg.text.width,
               bty = 'n', pch = 15, horiz = TRUE, xpd = TRUE, cex = col.legend.cex, x.intersp = 1
               )

        # row 2
        legend('bottom', inset = c(0, 0.94),
               legend = cld$legend[cld$leg.row.indices$row.2],
               col = cld$col[cld$leg.row.indices$row.2],
               text.width = leg.text.width,
               bty = 'n', pch = 15, horiz = TRUE, xpd = TRUE, cex = col.legend.cex, x.intersp = 1
        )

      } else {
        # standard invocation
        legend(
          'bottom', 
          legend = cld$legend, 
          col = cld$col,
          bty = 'n',
          pch = 15,
          horiz = TRUE,
          xpd = TRUE,
          inset = c(0, 0.99),
          cex = col.legend.cex,
          x.intersp = 1
          )
      }
    }

  }


  }

#' generic plot method for \code{SoilProfileCollection} objects
#' @name plot
#' @param y [not used]
#' @usage
#' ## S4 method for signature 'SoilProfileCollection,ANY'
#' ## note: y argument in generic definition is not currently used
#' plot(x, y, ...)
#' @aliases plot,SoilProfileCollection,ANY-method
#' @rdname SoilProfileCollection-plotting-methods
#'
setMethod("plot", signature(x = "SoilProfileCollection",
                            y = "ANY"),
          definition = {function(x, y, ...) plotSPC(x, ...)})
